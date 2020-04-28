
# Important things for V1

1. When the lines go outside the artboard, it's still in the SVG, so by default
   they should be cropped out
   
   This is maybe the trickiest thing to figure out:
   
   Basically, for each object, determine if it's in or out of the box, and if it
   is, then figure out the logic for cutting it along the axis... (ie. build the
   actual path that is relevant...)
   
   Basically do this on export only... but for the drawing, just rely on the
   normal clipping

# Other things I might want

1. A draggable anchor point for the internal coordinate space for each object
   that allows you to do rotations in an interesting way
2. Multi-color should be multiple SVGs on export, so it's easy to do multi color
   / multipen
3. Draggable corners and edges... standard
