---
title: More useful snapping for NSSlider
---

If you want an `NSSlider` to snap on certain intervals, your option
out-of-the-box is to give it tick marks (using
`-setNumberOfTickMarks:`), and then enforce that it can only be set to
values coinciding with a tick mark (using
`setAllowsTickMarkValuesOnly:`). This is great, if you only want to
accept values at certain intervals; but it's not very helpful if you
simply want to *snap to* values at key points (like quarters, or
thirds).

<!--more-->

I've seen a few solutions to this problem; [the one that came
closest](http://stackoverflow.com/questions/5843699/hot-to-create-custom-nsslider-like-start-screen-saver-slider-in-system-prefer)
ended up overriding

~~~~{.ObjectiveC .numberLines}
-[NSSliderCell startTrackingAt:inView:],
-[NSSliderCell continueTracking:at:inView:]
~~~~

and cleverly switching `allowsTickMarkValuesOnly` on and off at
opportune moments. This worked visually, but the data that was streamed
from the slider didn't actually snap until after tracking ended. This is
obviously a non-starter, if you're showing continuous feedback for your
slider.

My solution
------------

So, I've come up with a simpler and slightly more clever solution, which behaves
just as one would expect it to.

~~~~{.ObjectiveC .numberLines}
@interface JSSnappingSliderCell : NSSliderCell
@end

@implementation JSSnappingSliderCell
static const CGFloat kSnappingZone = 10.0f;

- (BOOL)continueTracking:(NSPoint)lastPoint at:(NSPoint)currentPoint inView:(NSView *)controlView
{
  CGPoint snapToPoint = currentPoint;

  for (NSUInteger i = 0; i < self.numberOfTickMarks; i++)
  {
    NSRect tickMarkRect = [self rectOfTickMarkAtIndex:i];
    if (ABS(tickMarkRect.origin.x - currentPoint.x) <= kSnappingZone)
    {
      snapToPoint = [self rectOfTickMarkAtIndex:i].origin;
      break;
    }
  }

  return [super continueTracking:lastPoint at:snapToPoint inView:controlView];
}

@end
~~~~

Enjoy, and [do let me know](http://www.twitter.com/jonsterling) if you
have a better idea!
