---
title: Typed Collections with Self Types in Objective-C
---

> The latest versions of the Clang compiler extend the Objective-C
> language with related return types, which allow a limited form of
> covariance to be expressed. Methods with certain names (`alloc`,
> `init`, etc.) are inferred to return an object of the instance type
> for the receiver; other methods can participate in this covariance by
> using the `instancetype` keyword for the return type annotation.

Typically, this feature is used for convenience constructors which
would previously have returned `id`. However, we can also use it to
encode statically-typed collections without full-blown
generics.[^caveat]

[^caveat]: Please duplicate
[rdar://10848469](http://www.openradar.me/radar?id=1517409) if you
want `instancetype` to be allowed in argument types.

<!--more-->

## Decorating Types with Protocols

If Objective-C had parametric polymorphism (that is, the ability to
abstract over types), then a simple typesafe collection would be
trivial:

~~~~{.ObjectiveC .numberLines}
@protocol OrderedCollection[V]
- (V)at:(NSUInteger)index;
- (void)put:(V)object;
@end
~~~~

With `instancetype`, we support a subset of parametric polymorphism:
that is, we can abstract over one type (the type of an instance of the
implementing class), and we are limited to referring to this type in
method return types.[^protocol] So, we can approximate something rather close,
but slightly less safe and precise:

~~~~{.ObjectiveC .numberLines}
@protocol OrderedCollection
- (instancetype)at:(NSUInteger)index;
- (void)put:(id)object;
@end
~~~~

[^protocol]: In the context of protocol, `instancetype` refers to the
conforming class; so, `instancetype` of `NSString
<OrderedCollection>*` is `NSString*`.


Since we are limited to abstracting over the type of `self`, the
static type of any such collection must actually be the type of its
elements decorated by the `<OrderedCollection>` protocol. So, a
collection of strings statically be understood to be a single string,
decorated with collection methods:

~~~~{.ObjectiveC .numberLines}
NSString <OrderedCollection> *strings = ...;
[strings put:@"hello,"];
[strings put:@"world!"];
~~~~

## Higher Order Messaging

The fact that the static type of such a collection is the product of
the type of its elements and a collection trait gives rise
serendipitously to the applicability of *higher-order-messaging*, or
HOM. For instance, what does it mean if you send `NSString`-messages
to an object `NSString <OrderedCollection>*`? It makes sense to treat
the collection as a $\textbf{Functor}$ and map the message over its elements:

~~~~{.ObjectiveC .numberLines}
for (NSString *upperString in [strings.uppercaseString substringFromIndex:1])
  NSLog(@"%@", upperString);

// => ELLO,
// => ORLD!
~~~~

## An Implementation

We'll implement covariant protocols
`<OrderedCollection, MapCollection>`.

### The collection interfaces

~~~~{.ObjectiveC .numberLines}
@protocol OrderedCollection <NSFastEnumeration>
- (instancetype)at:(NSUInteger)index;
- (void)put:(id)object;
@end

@protocol MapCollection <NSFastEnumeration>
- (instancetype)at:(id)key;
- (void)put:(id)object at:(id)key;
@end
~~~~

### Collection Proxies

We will use proxy objects to implement both the HOM and the checked
collection accessors. First, we start with an abstract base
`CollectionProxy` class, in terms of which proxies for both arrays and
dictionaries will be expressed:

~~~~{.ObjectiveC .numberLines}
@interface CollectionProxy : NSObject
@property (strong) id target;
@property (assign) class elementClass;

- (id)initWithTarget:(id)target;

+ (Class)collectionClass;

// Subclasses will provide a technique for mapping an element in one
// collection to a new element in another.
- (void)appendMappedObject:(id)mapped fromObject:(id)original toBuffer:(id)buffer;

// Subclasses may wish to map over an object derivable from object
// given in fast enumeration. For instance, dictionaries map over
// values, rather than keys.
- (id)redirectIteration:(id)object;
@end

@implementation CollectionProxy
@synthesize target, elementClass;

- (id)initWithTarget:(id)aTarget {
  if ((self = [super init]))
    target = aTarget;

  return self;
}

- (id)init {
  return [self initWithTarget:[self.class.collectionClass new]];
}

- (NSMethodSignature *)methodSignatureForSelector:(SEL)sel {
  if ([self.class.collectionClass instancesRespondToSelector:sel])
    return [self.class.collectionClass instanceMethodSignatureForSelector:sel];

  return [self.elementClass instanceMethodSignatureForSelector:sel];
}

- (BOOL)respondsToSelector:(SEL)aSelector {
  return [super respondsToSelector:aSelector]
      || [target respondsToSelector:aSelector]
      || [self.elementClass instancesRespondToSelector:aSelector];
}

- (void)forwardInvocation:(NSInvocation *)invocation {
  // If the collection itself responds to this selector (like if
  // someone sent -count), we'll forward the message to it.
  if ([target respondsToSelector:invocation.selector])
    return [invocation invokeWithTarget:target];

  // If the invocation returns void, we still want to invoke it, but
  // we don't want to try to do anything with its results.
  BOOL returnsValue = strcmp("v", invocation.methodSignature.methodReturnType) != 0;
  id buffer = returnsValue ? [self.class.collectionClass new] : nil;

  for (id obj in target) {
    [invocation retainArguments];
    [invocation invokeWithTarget:[self redirectIteration:obj]];

    void *outPtr = NULL;
    if (returnsValue) {
      [invocation getReturnValue:&outPtr];

      // We marshall the return value of the invocation back into our space.
      id mapped;
      if ((mapped = objc_unretainedObject(outPtr)))
        [self appendMappedObject:mapped fromObject:obj toBuffer:buffer];
    }
  }

  if (returnsValue && [buffer count] > 0) {
    // Build up a new proxy of the same kind to return.
    CollectionProxy *proxy = [[self.class alloc] initWithTarget:buffer];

    // We marshall the proxy out of our space and set it as the return
    // value of our invocation.
    invocation.returnValue = &(const void *){
      objc_unretainedPointer(proxy)
    };
  }
}

// Default behavior

+ (Class)collectionClass { return nil; }
- (void)appendMappedObject:(id)mapped fromObject:(id)original toBuffer:(id)buffer {}
- (id)redirectIteration:(id)object { return object; }

@end

@interface OrderedCollectionProxy : CollectionProxy
@end

@interface MapCollectionProxy : CollectionProxy
@end


@implementation OrderedCollectionProxy

- (id)initWithTarget:(id)target {
  if ((self = [super initWithTarget:target]) && [target count])
    self.elementClass = [[target lastObject] class];

  return self;
}

+ (Class)collectionClass {
  return [NSMutableArray class];
}

- (void)appendMappedObject:(id)mapped fromObject:(id)original toBuffer:(id)buffer {
  [buffer addObject:mapped];
}

- (void)put:(id)object {
  assert([object isKindOfClass:self.elementClass]);
  [self.target addObject:object];
}

- (instancetype)at:(NSUInteger)index {
  return [self.target objectAtIndex:index];
}

@end


@implementation MapCollectionProxy

- (id)initWithTarget:(id)target {
  if ((self = [super initWithTarget:target]) && [target count])
    self.elementClass = [[target allValues].lastObject class];

  return self;
}


+ (Class)collectionClass {
  return [NSMutableDictionary class];
}

- (void)appendMappedObject:(id)mapped fromObject:(id)key toBuffer:(id)buffer {
  [buffer setObject:mapped forKey:key];
}

- (id)redirectIteration:(id)key {
  return [self.target objectForKey:key];
}

- (void)put:(id)object at:(id)key {
  assert([object isKindOfClass:self.elementClass]);
  [self.target setObject:object forKey:key];
}

- (instancetype)at:(id)key {
  return [self.target objectForKey:key];
}

@end

~~~~




### Collection Constructors

To construct a collection of some class, we send a message to that
class with a covariant return type; unfortunately, we cannot decorate
`instancetype` with any further protocols. So, ideally
`+orderedCollection` would return `instancetype <OrderedCollection>`,
but this is currently impossible; thus, you will have to provide the
type decoration yourself.[^instancetypeprotocol]

[^instancetypeprotocol]: Please duplicate
[rdar://10849187](http://www.openradar.me/radar?id=1513402) if you
want to be able to decorate `instancetype` with a protocol list.

~~~~{.ObjectiveC .numberLines}
@interface NSObject (Collections)
+ (instancetype)orderedCollection;
+ (instancetype)mapCollection;
@end

@implementation NSObject (Collections)

+ (instancetype)orderedCollection {
  CollectionProxy *proxy = [OrderedCollectionProxy new];
  proxy.elementClass = self;
  return proxy
}

+ (instancetype)mapCollection {
  CollectionProxy *proxy = [MapCollectionProxy new];
  proxy.elementClass = self;
  return proxy;
}

@end
~~~~


## See it in action

~~~~{.ObjectiveC .numberLines}
NSURL <MapCollection> *sites = (id)[NSURL mapCollection];
[sites put:[NSURL URLWithString:@"http://www.jonmsterling.com/"]
        at:@"jon"];
[sites put:[NSURL URLWithString:@"http://www.reddit.com/"]
        at:@"reddit"];
[sites put:[NSURL URLWithString:@"git://github.com/jonsterling/Foam.git"]
        at:"foam_repo"];

NSURL *jonsSite = [sites at:@"jon"];
// => http://www.jonmsterling.com/

NSString <MapCollection> *schemes = (id)sites.scheme.uppercaseString;
/* => { jon: "HTTP://",
        reddit: "HTTP://",
        foam_repo: "GIT://" }
 */
~~~~


## Further Exercises

These HOMs do not correctly handle methods that return non-object
types. It is definitely possible to write a more robust version that
will box primitives appropriately, but not within the scope of this
post. This will require further inspection of the method signature of
the forwarded invocation.
