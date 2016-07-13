# pietc

A piet interpreter

The functionality of this interpreter is dependent on a few basic ideas native to piet:

* Each color block may be exited in exactly 8 ways (North, East, South, and
  West, each with a Left and Right chooser)
* Which of these exits map to which other color blocks can be determined
  statically
* The transition of one color block to any other color block can be represented
  as an exact instruction
* White regions can be traced from each exit deterministically to another color
  block with a NOP instruction
* From where a color block has been entered doesn't matter, neither do any of
  the other dimensions (though this information may be retained for debugging
  purposes).  The only thing that matters for routing one color block to
  another is the DP and CC when the first block was entered.
* If a color block is entered which has no exits, the program may terminate
  (this color block can be shorted out to be a simple exit, and may be not even
  entered as an optimization)
* An exit into a white color block can be traced by the compiler to determine
  if it starts looping, which can be used to directly treat the exit into that
  white color as a simple exit as with a "dead block".


So in the end, no colors or shades are important, only the colors and shades of
blocks relative to one another.

Perhaps the LLVM generation will be finished one day.
