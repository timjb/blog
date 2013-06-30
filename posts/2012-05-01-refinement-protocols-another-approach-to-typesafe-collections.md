---
title: Refinement Protocols: Another Approach to Typed Collections
---

> In February, I
> [wrote](2012-02-05-typed-collections-with-self-types-in-objective-c.html)
> about using `instancetype` to get semi-typesafe collections in
> Objective-C. After an email conversation with Patrick Beard, I'd like
> to present another approach.

One sad consequence of protocols only being commonly used to encode
delegation is that beginners frequently conflate protocols with
delegation. Hence the proliferation of tutorials on how to "do
delegates" in Objective-C; in fact, I had written one such tutorial a
long time ago, and it became so popular that I deleted it, for it was
feeding directly into this very conflation (which I deem damaging to
learners).

In this article, I'll be showing a more interesting use of protocols:
typesafe collections.

<!--more-->

### Narrowing Static Type with Refinement Protocols

In a static-dynamic language like Objective-C, the static type of an
object is distinct from its runtime type; in fact, because of mechanisms
like forwarding, the runtime type of an object is really quite difficult
to pin down (and may change over time)[^dynamic_type_note].

[^dynamic_type_note]: This is a *bad thing*, but we're going to roll
with it since it's how the language works. Basically, in an ideal
message-passing language, the set of messages an object is guaranteed to
respond to (its structural type) should be invariant over time, whereas
the way in which the object responds to those methods may change over
time. (In this ideal world, we have a structurally typed language rather
than a nominally typed one.)

    An example of this distinction is Futures: from the moment a future
is created, it should be decorated with the protocol of its promised
object, and is guaranteed to respond to messages that are meant to the
object it encloses. When its promised object has not yet been created,
the future will respond to messages by queuing them up for later; after
the object has been created, the future should behave transparently as
though it were the object itself.


We can take advantage of this disparity by enforcing further
restrictions on an object's interface at compiletime, this eliminating
some common sources of programmer error, as well as removing the need
for downcasts in many cases.



### A protocol for dictionaries of numbers

Take, for instance, the following protocols:

~~~~{.ObjectiveC}
@protocol NSNumberDictionary <NSObject>
- (NSNumber *)objectForKey:(NSString *)key;
- (NSNumber *)objectForKeyedSubscript:(NSString *)key;
@end

@protocol NSNumberMutableDictionary <NSNumberDictionary>
- (void)setObject:(NSNumber *)number forKey:(NSString *)key;
- (void)setObject:(NSNumber *)number forKeyedSubscript:(NSString *)key;
@end

@interface NSDictionary (NSNumberDictionary) <NSNumberDictionary>
@end
@interface NSMutableDictionary (NSNumberMutableDictionary) <NSNumberMutableDictionary>
@end
~~~~

Now, we can safely interact with a dictionary that is supposed to have
only numbers as values:

~~~~{.ObjectiveC}
id <NSNumberMutableDictionary> ages = [NSMutableDictionary new];
ages[@"jon"] = @19;
ages[@"dan"] = @"53"; // <--- Incompatible pointer types sending 'NSString *' to parameter of type 'NSNumber *'

// dot notation also works, because our types are fixed statically:
NSUInteger jonAge = ages[@"jon"].unsignedIntegerValue;
~~~~

Incidentally, due to what would seem to be a bug in Clang, even though
our `<NSNumberDictionary>` protocol does *not* include a signature for
`-setObject:forKeyedSubscript:`, the following code compiles with no
complaint:

~~~~{.ObjectiveC}
// this compiles, but it shouldn't:
id <NSNumberDictionary> immutableDictionary = [NSDictionary new];
immutableDictionary[@"key"] = @45;
~~~~

Until this is fixed, we can work around it by providing an impossible
signature in our immutable protocol:

~~~~{.ObjectiveC}
typedef struct {} MutationUnavailableForImmutableObject;
@protocol NSNumberDictionary <NSObject>
- (NSNumber *)objectForKey:(NSString *)key;
- (NSNumber *)objectForKeyedSubscript:(NSString *)key;
- (void)setObject:(MutationUnavailableForImmutableObject)object forKeyedSubscript:(NSString *)key;
@end
~~~~

~~~~{.ObjectiveC}
id <NSNumberDictionary> immutableDictionary = [NSDictionary new];
immutableDictionary[@"key"] = @45;
// ^-- Method object parameter type 'MutationUnavailableForImmutableObject' is not object type
~~~~


# Generalizing our technique with macros

So far, our approach has involved making new refinement protocols for
every possible element type. This is obviously untenable, but we can
make things easier by encoding generic protocols as macros:

First, let's make a macro that handles the creation of any protocol and
a dummy adopting category for a class:

~~~~{.ObjectiveC}
#define GenericProtocol(T,E,Name,...)\
@protocol E##Name <NSObject>\
__VA_ARGS__\
@end\
@interface T (E##Name) <E##Name>\
@end
~~~~

We'll also need a macro for a mutable variant that depends on the
immutable protocol:

~~~~{.ObjectiveC}
#define GenericMutableProtocol(T,E,Name,...)\
@protocol E##Mutable##Name <E##Name>\
__VA_ARGS__\
@end\
@interface T (E##Mutable##Name) <E##Mutable##Name>\
@end
~~~~

Now, if the immutable protocol includes a `-mutableCopy` method, it will
have to return an object of the mutable variant. So, we need to provide
forward declarations for the protocols:

~~~~{.ObjectiveC}
#define ForwardDeclareMutable(E,Name)\
protocol E##Mutable##Name
~~~~

You probably noticed that we didn't include the `@` before `protocol` in
this macro; this is a little trick that will allow us to have our macros
begin with the `@`-sign, just like real Objective-C directives. It's not
necessary, but it's a nice little sleight of hand. And now, we can
provide our generic dictionary macro:

~~~~{.ObjectiveC}
#define DictProtocol(E) ForwardDeclareMutable(E,Dictionary)\
GenericProtocol(NSDictionary, E, Dictionary,\
  - (id <E##Dictionary>)copy;\
  - (id <E##Mutable##Dictionary>)mutableCopy;\
  - (E *)objectForKey:(NSString *)key;\
  - (E *)objectForKeyedSubscript:(NSString *)key;\
  - (void)setObject:(MutationUnavailableForImmutableObject)object forKeyedSubscript:(NSString *)key;\
)\
GenericMutableProtocol(NSMutableDictionary, E, Dictionary,\
  - (void)setObject:(E *)object forKey:(NSString *)key;\
  - (void)setObject:(E *)object forKeyedSubscript:(NSString *)key;\
  - (void)addEntriesFromDictionary:(id <E##Dictionary>)otherDictionary;\
  - (void)setDictionary:(id <E##Dictionary>)otherDictionary;
)
~~~~

Finally, we can instantiate typed dictionary protocols for various
object types:[^qualified_types]

~~~~{.ObjectiveC}
@DictProtocol(NSNumber);
@DictProtocol(NSString);
@DictProtocol(NSData);
@DictProtocol(NSArray);
~~~~

[^qualified_types]: One limitation is that we can't provide a contract
for elements to be, for instance, of type `id <UITextFieldDelegate>` or
something. We could extend our system to support this, but protocol
names would have to be supplied rather than generated.


## The downside to our approach

### Ambiguity: multiple signatures for a method

The most glaring problem with this approach is that we are injecting
multiple signatures for a single method into a single class (via our
categories). So, the result is that it becomes impossible to interact
with a non-pimped collection; the following code may fail:

~~~~{.ObjectiveC}
NSMutableDictionary *plainDict = [NSMutableDictionary new];
plainDict[@"key"] = @"a string";
// <-- Compiler was expecting some other random type that we have instantiated,
//     like NSData or something. Oops.
~~~~

So anywhere these refinement protocols and the associated categories are
in scope, we are obliged to use them. For many people, that is too great
a demand.

We can instead just remove our categories can keep these method
signatures scoped in their protocols. But then we have to do an unsafe
downcast to `id` whenever we wish to instantiate a new typed dictionary.
That's rather close to beating the point.

Lastly, literals can never be typesafe, since the signature of
`+arrayWithObjects:count:` and friends is required to take a parameter
no more precise than `id[]` for literal creation (if you don't believe
me, see `Sema::BuildObjCArrayLiteral` in
[`SemaExprObjC.cpp`](http://clang.llvm.org/doxygen/SemaExprObjC_8cpp_source.html)).
And even more important, the literal expression compiler of course does
not abstract over the intended type of its result! So even if it were
possible to have one of these methods with a more precise signature, the
compiler would still have no way to disambiguate which method signature
to follow in any given instance. This may be fixable, but would likely
require a not insignificant amount of compiler rejiggering.


## Concluding Remarks

It's a rather nice idea for getting around Objective-C's lack of type
safety, and I thank Patrick for getting me to think about it a little
more. It's not perfect, however, and its necessary deficiencies will
likely prevent its use in shipping code for now. If anyone has any ideas
on how to improve this and make it work better, I'm all ears!
