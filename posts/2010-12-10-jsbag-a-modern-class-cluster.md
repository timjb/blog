---
title: JSBag: A Modern Class Cluster
---

I've long been frustrated by the complexity of the Class Cluster
pattern, and feel that in most cases, it can be replaced with something
cleaner and more flexible. `JSBag` is an example of a new collection
class with mutable and immutable variants that is implemented with a
pattern similar to the Class Cluster, but far less coupled. I do this
using Traits.

*A 'bag' is a multiset, or a set which can contain duplicate objects.
`CoreFoundation` includes `CFBag`, which is the same concept; for
reasons of simplicity, `JSBag` was implemented on top of `NSArray`
instead of `CFBag`, but it certainly would be possible if better
performance were desired.*

<!--more-->

Traits
------

Really, they're just protocols that have concrete implementations. The
way this works is that when a `JSBag` object is created, the traits
(such as mutability) that it needs to have are specified; there are
default implementations of each of these traits included (the
implementation is in the form of a class adopting the trait's protocol),
but these can be switched out for alternate implementations. You can
even add your own traits.

The way this all works is that when the `JSBag` object receives a
message, it looks through its enabled traits until it finds the one
which declares a corresponding method; then, it will look through its
traits-to-implementation-class map to find the class that implements
that method. Then, it grabs the `IMP` from that class and applies it to
`self` with whichever arguments are necessary.

Whereas in the traditional Class Cluster, the abstract superclass tends
to spend a lot of time jiggering about how to delegate its unimplemented
tasks, and the concrete subclasses tend to duplicate a lot of
functionality among themselves, this system allows pluggable
implementations for traits that can easily be turned on and off. So, the
`JSMutableBag` subclass of `JSBag` actually only implements one method,
which is to override the enabled traits to include
`@protocol(JSMutableBag)`. Other similar subclasses of `JSBag` could be
created which add support for more, user-defined traits.


Drawbacks
---------

The idea of this is not to use alternate implementations that carry down
abstract bits from a superclass: the idea is to compose functionality by
registering implementations of traits, and then dispatching tasks
dynamically to them. That means that a few benefits of the traditional
Class Cluster are lost, namely the ability for implementations to have
alternative instance-variable layouts.

In the end, I think that the performance gain from that sort of
optimization is going to be minimal for most things (remember, *YAGNI*).
So, I believe that in many cases, a trait-composition design like that
shown in `JSBag` could be useful.


Design
------

You may note that I've avoided using specific types of collections in my
interface, because in most cases, all that matters is that they can be
iterated over. Hence, instead of taking parameters of type `NSArray*` or
`NSSet*` for some things, I've generalized to `id <NSFastEnumeration>`.
Since `JSBag` adopts `<NSFastEnumeration>`, that means that methods
which take collections as parameters can also take `JSBag` objects. This
is incredibly useful, as it makes having methods like `+bagWithArray:`,
`+bagWithSet:`, `+bagWithBag:`, etc. actually redundant:

~~~~{.ObjectiveC}
+ (id)bagWithCollection:(id <NSFastEnumeration>)enumerable;
~~~~

will handle all of this for us. This made implementing `<NSCopying>`,
`<NSMutableCopying>`, and several other key bits absolutely trivial:

~~~~{.ObjectiveC .numberLines}
- (id)copyWithZone:(NSZone *)zone {
  return [[JSBag allocWithZone:zone] initWithCollection:self];
}

- (id)mutableCopyWithZone:(NSZone *)zone {
  return [[JSMutableBag allocWithZone:zone] initWithCollection:self];
}
~~~~

Remember that messaging `JSMutableBag` vs. `JSBag` for in this case only
changes the default enabled traits from `{<JSBasicBag>}` to
`{<JSBasicBag>,<JSMutableBag>}`. That's pretty cool.


Moving Forward
--------------

I believe that this trait-composition approach could be generalized to
be usable in many contexts. As I improve it, and make it more flexible,
I hope to find neat uses for it. In the meanwhile, clone or fork `JSBag`
[on Github](https://github.com/jonsterling/JSBag/)!
