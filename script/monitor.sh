#!/bin/sh
EXTERNAL_OUTPUT="VGA1"
EXTERNAL_OUTPUT_MODE="1360x768"
INTERNAL_OUTPUT="HDMI1"
INTERNAL_OUTPUT_MODE="1920x1080"
EXTERNAL_LOCATION="right"

if (xrandr | grep "$EXTERNAL_OUTPUT disconnected"); then
    xrandr --output $INTERNAL_OUTPUT --auto --output $EXTERNAL_OUTPUT --off
elif (xrandr | grep "$INTERNAL_OUTPUT disconnected"); then
    xrandr --output $EXTERNAL_OUTPUT --auto --output $INTERNAL_OUTPUT --off
else
case "$EXTERNAL_LOCATION" in
  left|LEFT)
    EXTERNAL_LOCATION="--left-of $INTERNAL_OUTPUT"
    ;;
  right|RIGHT)
    EXTERNAL_LOCATION="--right-of $INTERNAL_OUTPUT"
    ;;
  top|TOP|above|ABOVE)
    EXTERNAL_LOCATION="--above $INTERNAL_OUTPUT"
    ;;
  bottom|BOTTOM|below|BELOW)
    EXTERNAL_LOCATION="--below $INTERNAL_OUTPUT"
    ;;
  *)
    EXTERNAL_LOCATION="--left-of $INTERNAL_OUTPUT"
    ;;
esac
xrandr |grep $EXTERNAL_OUTPUT | grep " connected "
if [ $? -eq 0 ]; then
  xrandr --output $INTERNAL_OUTPUT --mode $INTERNAL_OUTPUT_MODE --output $EXTERNAL_OUTPUT --mode $EXTERNAL_OUTPUT_MODE $EXTERNAL_LOCATION
else
  xrandr --output $INTERNAL_OUTPUT --auto --output $EXTERNAL_OUTPUT --off
fi
fi
