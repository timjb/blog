---
title: Eliminating Stringly-Typed Code in Objective-C
---


Perhaps the greatest sin in the common body of Cocoa design patterns is
the pervasiveness of "stringly-typed" code:[^bitchy-comment] it
frequently comes up in KVO and bindings, among other things. Various
macros have arisen to allow type checking of keypaths, largely thanks to
[efforts](https://github.com/jspahrsummers/libextobjc/blob/master/extobjc/EXTKeyPathCoding.h)
by Justin Spahr-Summers and myself. Today, I'd like to talk a bit about
some ideas for enforcing statically trusted boundaries for checked
keypaths.

<!--more-->

   [^bitchy-comment]: You'll have to trust me on this one, since to
enumerate the design-sins of Cocoa and Objective-C would likely take the
rest of my life.


### The Problem: Sometimes We Lie

Let's go over the state of affairs. APIs accept strings for keypaths; we
then conjure up some macro like `@keypath` to allow compile-time
checking of keypaths, gaining in addition the ability for them to
participate in refactoring. So, we can be sure that our own code is
providing safe keypaths to the APIs it consumes.

```{.ObjectiveC}
[detailView bind: @keypath(detailView.titleLabel.text)
        toObject: dogsController
     withKeyPath: @keypath(dogsController.selectedDog.name)
         options: nil];
```

But the other half of the problem remains: when our own APIs use
keypaths, it would be nice to enforce that these keypaths be checked. If
we're just accepting strings and hoping the client code is clever enough to
use our `@keypath` macro, all is well until we begin to lie. And we all
lie.

### Trusted Boundaries: Types For Certified Keypaths

Whilst we really can't do anything about Cocoa's APIs, we should like to
force keypaths passed into our own APIs to be checked. We can do this
with a type, and some clever higgle-piggling about with deprecation
annotations:[^impl-omitted]

   [^impl-omitted]: The implementations of these interfaces are
trivial; I will leave them to your imagination.


```{.ObjectiveC .numberLines}
@interface CheckedKeyPath : NSObject
@property (strong, readonly) NSString *stringValue;
- (id)initWithUnsafeKeyPath:(NSString *)unsafeKeyPath DEPRECATED_ATTRIBUTE;
@end

@interface NSString (CheckedKeyPath)
- (CheckedKeyPath *)unsafeMarshalKeyPath DEPRECATED_ATTRIBUTE;
@end

#define keypath(PATH) \
_Pragma("clang diagnostic push")\
_Pragma("clang diagnostic ignored \"-Wdeprecated\"")\
    (strchr(# PATH, '.') + 1).unsafeMarshalKeypath \
_Pragma("clang diagnostic pop")
```

Now, the real trick is in the `@keypath` macro definition above; feel
free to swap in something more advanced for the meat of the definition.
The important part is that the macro includes pragmas[^pragmas] to suppress
warnings about deprecated methods, and then goes ahead and uses
`-unsafeMarshalKeypath` in that scope. In this way, we can be reasonably
assured (though not entirely, of course) that the only way anyone will
_ever_ create a term of type `CheckedKeyPath` is by using this macro.

   [^pragmas]: We cannot use `#pragma` within a macro, but `_Pragma`
works just the same.

Now, in our APIs, we can expose only safe interfaces for keypaths by
accepting `CheckedKeyPath` rather than `NSString`; when a keypath needs
to be passed to Cocoa, we can then trivially throw away the
certification by sending `-stringValue`.
