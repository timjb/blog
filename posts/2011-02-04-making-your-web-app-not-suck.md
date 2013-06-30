---
title: Making your web app not suck
---

In the past, I have railed against companies who choose to write mobile web
applications instead of native apps. Having recently tried the mobile version of
[Pinboard](http://m.pinboard.in/), as well as the new [Basecamp mobile
version](http://basecamphq.com/mobile), I must slightly revise my opinion.

<!--more-->

## Hence, my new shitlist.

1. Never embed your web application in a native application; that is to say, do
not slap a webview above a `UITabBarController` and call it good. This is a sin.

2. Never use controls which attempt to mimic those of the substrate platfom: the
bullshit interaction of your iPhone-y navigation bar will only serve to
highlight your incompetence to experienced users, and confuse inexperienced
ones.

3. Do not make some sort of header attempt to hover at the top of your view,
unless you have coded your own scrolling system (like in PastryKit). All the
time, I see bullshit websites that have some sort of terrible navigation bar
that sits at the top of the view, no matter how far down you are scrolled; but
because it is bullshit, when you start dragging to scroll, the header
disappears, and reappears when scrolling has stopped. This is bullshit.

4. Don't overuse animation. WebKit has some neat stuff these days, and in an
attempt to make web apps feel at home, some devs insist of using view animations
(like in an iOS navigation controller). Don't do this: it's often laggy, and
takes away from the speed of a transition that ought to be instant! When I do
something in a native navigation controller, I expect some animation-feedback;
when I tap a link in a website, I expect to brought **immediately** to my
destination.


5. Don't, for the love of God, try to make it look like iOS. The whole reason
for web applications is that your don't have to spend resources building a
separate app for each platform. If you are content to force all users,
regardless of their platform, to use an app that looks like you disemboweled
iOS and filled her with bullshit, then you ought to just make a nice native
app for iOS and ignore the other platforms.

> **Note:** [@stevestreza](http://twitter.com/stevestreza), the creator of the
> awesome web app [Swearch](http://swearch.me/), informs me that it's possible
> to make WebKit animations 60fps on iPhone 3GS and up, if you code them the
> hell properly.  The problem is that most developers do a poor job at this, I
> suppose.


## What you must do.

If you must make a mobile web app, make it as simply as possible, without lots
of custom controls. In fact, just try to make it exactly like Pinboard;
37signals also did a great job with Basecamp (though I disagree with their use
of animated transitions). Last thing: for the sake of iOS users, please make
sure that your app is capable of being clipped to the home screen and opening in
its own chromeless browser.
