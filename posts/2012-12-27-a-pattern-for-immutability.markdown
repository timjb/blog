---
title: A Pattern for Lightweight Immutability in Objective-C
---

Today, I hope to introduce a more lightweight approach to immutable
objects in Objective-C that sidesteps some of the problems with class
clusters (such as hostility to subclassing) by making some trade-offs.
The class cluster approach provides the following fronts of protection
against illegal mutation:

1. Mutation methods only occur in the interfaces for the mutable
   variants. This protects us when types are preserved.
2. Sending a mutation message to an immutable object results in an
   exception.

I'm going to argue that the first force of protection (that of types) is
far more important than the second (that of runtime exceptions), and
that if we are willing to give it up, we can have a vastly simpler
notion of immutability and mutability. Finally, I'll discuss ways we can
recover runtime exceptions in the case of an unsafe cast, and finally
how we can recover the functionality of `-mutableCopy` in a
subclass-friendly fashion.

<!--more-->

### Immutability: Axioms for a Minimal Implementation

Let us agree on the following axioms (that is to say, the extent to
which you find this article valuable will vary with the extent to which
you stipulate the following):

1. Objects should be immutable; no shared object should ever have its
   state changed underneath from another object. If multiple objects
   need to share the changing state of another object, it should be
   represented as a reactive event stream.

2. It should be trivial to make an updated _copy_ of an object; it
   should be trivial to update multiple aspects of an object in
   whichever order you wish.

3. We need not code defensively against unsafe casting. If API consumers
   perform a downward cast, they objectively deserve every bit of
   bizarre and confusing behavior they are rewarded with.

And so from **Axiom 3**, we can dispatch the issue of whether it is
necessary to have both static and runtime assurances about immutability.
If API consumers agree not to perform any obviously unsafe casts, then
we can rely on static immutability alone. Later we'll discuss what is
necessary if we wish to discard this last axiom.

### Implementation Example: An Ordered Dictionary

The key conceit of our plan is to provide a mutable and an immutable
interface to an object; then, we limit access to the mutable interface
to a few safe points in our code. That is, we will allow dictionaries to
be built and updated imperativiely, but the mutations will be upon a
*copy*. It should look something like this:

```{.ObjectiveC}
JSOrderedDictionary *updated = [oldDict update:^(id<JSMutableOrderedDictionary> dict) {
    dict[@"dogs"] = @[ @"tucker", @"rover" ];
    dict[@"cats"] = @[ @"emma" ];
    [dict removeObjectForKey:@"hamsters"];
}];
```

Now, we can get started. First, we will define the immutable and mutable
interfaces for an ordered dictionary; we expose the mutable one only in
`-update:`. The mutable interface of course inherits all the immutable
kit.

```{.ObjectiveC .numberLines}
@protocol JSMutableOrderedDictionary;
typedef void (^JSOrderedDictionaryUpdate)(id<JSMutableOrderedDictionary> dict);

@protocol JSOrderedDictionary <NSFastEnumeration>
- (id)objectForKey:(id<NSCopying>)key;
- (id)objectForKeyedSubscript:(id<NSCopying>)key;

- (NSArray *)allKeys;
- (NSArray *)allValues;
- (NSUInteger)count;

- (instancetype)update:(JSOrderedDictionaryUpdate)updateBlock;
@end

@protocol JSMutableOrderedDictionary <JSOrderedDictionary>
- (void)setObject:(id)object forKey:(id<NSCopying>)key;
- (void)setObject:(id)object forKeyedSubscript:(id<NSCopying>)key;
- (void)removeObjectForKey:(id<NSCopying>)key;
@end

@interface JSOrderedDictionary : NSObject <JSOrderedDictionary, NSCopying>
- (id)initWithOrderedDictionary:(id<JSOrderedDictionary>)dictionary;
- (id)initWithObjects:(NSArray *)objects forKeys:(NSArray *)keys;
@end
```

The implementation is quite straightforward:

```{.ObjectiveC .numberLines}
@interface JSOrderedDictionary (JSMutableOrderedDictionary) <JSMutableOrderedDictionary>
@end

@implementation JSOrderedDictionary {
    NSMutableArray *_keys;
    NSMutableDictionary *_dictionary;
}

#pragma mark - Initializers

- (id)init {
    return [self initWithObjects:@[ ] forKeys:@[ ]];
}

- (id)initWithOrderedDictionary:(id<JSOrderedDictionary>)dictionary {
    return [self initWithObjects:dictionary.allValues forKeys:dictionary.allKeys];
}

- (id)initWithObjects:(NSArray *)objects forKeys:(NSArray *)keys {
    if (self = [super init]) {
        _keys = [keys mutableCopy];
        _dictionary = [NSMutableDictionary dictionaryWithObjects:objects forKeys:keys];
    }

    return self;
}

#pragma mark - NSCopying

- (id)copyWithZone:(NSZone *)zone {
    return [[[self class] alloc] initWithOrderedDictionary:self];
}

#pragma mark - NSFastEnumeration

- (NSUInteger)countByEnumeratingWithState:(NSFastEnumerationState *)state
     objects:(__unsafe_unretained id [])buffer count:(NSUInteger)len {
    return [_keys countByEnumeratingWithState:countByEnumeratingWithState:state objects:buffer count:len];
}

#pragma mark - JSOrderedDictionary

- (id)objectForKey:(id<NSCopying>)key {
    return [_dictionary objectForKey:key];
}

- (id)objectForKeyedSubscript:(id<NSCopying>)key {
    return [self objectForKey:key];
}

- (NSArray *)allKeys {
    return [_keys copy];
}

- (NSArray *)allValues {
    NSMutableArray *values = [NSMutableArray arrayWithCapacity:self.count];
    for (id<NSCopying> key in self) {
        [values addObject:self[key]];
    }

    return [values copy];
}

- (NSUInteger)count {
    return _keys.count;
}

- (instancetype)update:(JSOrderedDictionaryUpdate)updateBlock {
    JSOrderedDictionary *copy = [self copy];
    updateBlock(copy);
    return copy;
}

@end

@implementation JSOrderedDictionary (JSMutableOrderedDictionary)
// The implementation is trivial.
@end

```

## Fudging the Axioms: What can we recover?

Now that we've shown the Taliban approach, we can see what is necessary
to recover some of the things we might miss.

### Recovering Exceptions

As explained above, the current model expresses the boundary between
mutability and immutability using types. That is, we provide a mutable
interface to our dictionary, exposed in such a way as to aggregate
imperative updates and perform them safely on a copy. But we do nothing
to prevent a user from casting `JSOrderedDictionary` to
`id<JSMutableOrderedDictionary>`; if a dictionary object is shared, and
one partaker unscrupulously (and unsafely) casts the dictionary to `id`
or the mutable interface, this may represent a broken program that would
be hard to debug.

We can recover exceptions (which would provide a way to determine when a
dictionary is being used unsafely) very simply by adding a private flag
to the dictionary:

```{.ObjectiveC .numberLines}
@implementation JSOrderedDictionary {
    BOOL _isMutable;
    // ...
}

- (instancetype)modify:(JSOrderedDictionaryUpdate)block {
    JSOrderedDictionary *copy = [self copy];
    copy->_isMutable = YES;
    block(copy);
    copy->_isMutable = NO;
    return copy;
}

// ...
@end

@implementation JSOrderedDictionary (JSMutableOrderedDictionary)

- (void)assertMutableForSelector:(SEL)selector {
    if (_isMutable) return;

    NSString *reason = [NSString stringWithFormat:
                        @"Attempted to send -%@ to immutable object %@",
                        NSStringFromSelector(selector), self];
    @throw [NSException exceptionWithName:NSInternalInconsistencyException reason:reason userInfo:nil];
}

- (void)setObject:(id)object forKey:(id<NSCopying>)key {
    NSParameterAssert(key != nil);
    [self assertMutableForSelector:_cmd];

    if (object == nil) [_keys removeObject:key];
    else if (![_keys containsObject:key]) {
        [_keys addObject:key];
    }

    [_dictionary setObject:object forKey:key];
}

// ...

@end

```

### Mutability Unchained: Omphale's Revenge

We can even recover the last remaining thing that the class cluster
approach gives us (to have a real mutable dictionary, at your own peril)
by adding a bit more kit. Let's wipe out what we did in the last section
and refine it a bit. First, let's adopt `<NSMutableCopying>` to our interface:

```{.ObjectiveC .numberLines}
@interface JSOrderedDictionary : NSObject <JSOrderedDictionary, NSCopying, NSMutableCopying>
// ...
@end
```

Now for some new kit:

```{.ObjectiveC .numberLines}
typedef enum {
    JSMutabilityNone,
    JSMutabilityTemporary,
    JSMutabilityPermanent
} JSMutabilityState;

@implementation JSOrderedDictionary {
    JSMutabilityState _mutabilityState;
    // ...
}

- (void)performWithTemporaryMutability:(JSOrderedDictionaryUpdate)block {
    BOOL immutableByDefault = _mutabilityState != JSMutabilityPermanent;
    if (immutableByDefault) _mutabilityState = JSMutabilityTemporary;
    block(self);
    if (immutableByDefault) _mutabilityState = JSMutabilityNone;
}

- (instancetype)modify:(JSOrderedDictionaryUpdate)block {
    BOOL immutableByDefault = _mutabilityState != JSMutabilityPermanent;
    JSOrderedDictionary *copy = immutableByDefault ? [self copy] : [self mutableCopy];
    [copy performWithTemporaryMutability:^(id<JSMutableOrderedDictionary> dict) {
        block(dict);
    }];
    return copy;
}

#pragma mark - NSMutableCopying

- (id)mutableCopyWithZone:(NSZone *)zone {
    JSOrderedDictionary *copy = [self copyWithZone:zone];
    copy->_mutabilityState = JSMutabilityPermanent;
    return copy;
}

@end

@implementation JSOrderedDictionary (JSMutableOrderedDictionary)

- (void)assertMutableForSelector:(SEL)selector {
    if (_mutabilityState != JSMutabilityNone) return;
    // ...
}

@end

```

The beauty of this approach is that it is now possible to provide full
mutability by adding only a tiny bit of code to the immutable
implementation, in a way that is very friendly to subclasses.

Whereas with class clusters, for a class cluster
$\{A_\text{mutable},A_\text{immutable}\}$, a subclass $B$ which wishes
to provide correct behavior must provide $\{B_\text{mutable} <
A_\text{mutable}, B_\text{immutable} < A_\text{mutable}\}$; furthermore,
methods having free ocurrences of $A_\text{mutable}$ must be rewritten
in the subclass to use $B_\text{mutable}$ and so forth.

Our approach sidesteps these problems quite nicely.

## Parting Notes

I'm using this approach for ordered dictionaries in
[ReactiveFormlets](https://github.com/jonsterling/ReactiveFormlets/blob/master/iOS%20Formlets/RAFOrderedDictionary.h).
It seems sufficient for my purposes; I'm excited to see if it will do
for more advanced ones.
