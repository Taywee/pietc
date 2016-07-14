# pietc

A piet LLVM compiler, using imagemagick to parse images.  Compiles an object
file with a main symbol for linking with some compiler toolchain (or native
linker if you want to get down and dirty).  Lets you do detailed tracing to
debug your own programs.

The program's own help menu is as such:

```
  ./pietc {OPTIONS} [input]

    A LLVM piet compiler, which simply compiles a piet program into an object
    file that you can compile with your favorite C compiler.

  OPTIONS:

      -h, --help                        Display this help menu
      --codel-size=[size], --cs=[size]  Set the codel size manually (0 tries to
                                        auto-detect, which usually works, but
                                        may be imperfect in certain scenarios)
      -t, --trace                       Trace program run
      --unknown-white                   Make unknown colors white
      --unknown-black                   Make unknown colors black
      -O[optlevel]                      Specify the optimization level (default
                                        1)
      -s[size], --stack=[size]          Specify the starting stack size. The
                                        stack resizes when necessary regardless.
                                        (defaults to 64)
      -p[prompt], --prompt=[prompt]     Change prompt for input operations
      -o[output], --output=[output]     Output object filename to print
                                        (defaults to the input filename + .o)
      input                             Input file to use
      "--" can be used to terminate flag options and force all following
      arguments to be treated as positional options
```

# Sample runs (with included png files)

```
$ ./pietc -o/tmp/test.o -O3 hello1.png  
autodetected codel size: 1
$ cc -o/tmp/test /tmp/test.o
$ /tmp/test 
Hello world!
$ ./pietc -t -o/tmp/test.o -O3 hello1.png 
autodetected codel size: 1
$ cc -o/tmp/test /tmp/test.o                  
$ /tmp/test | head -n 30                 
##########
Codel   : NR
Size    : 72
Exits   : 
    EL  : 10x0
    ER  : 10x0
    SL  : 9x10
    SR  : 5x10

Running operation PUSH size 72
##########
Codel   : DR
Size    : 1
Exits   : 
    EL  : 11x0
    ER  : 11x0
    SL  : 11x0
    SR  : 11x0
    WL  : 11x0
    WR  : 11x0

Running operation OUTC size 1
H##########
Codel   : NM
Size    : 101
Exits   : 
    EL  : 18x0
    ER  : 18x9
    SL  : 18x9
    SR  : 9x9
$ 
```

# Compiler ideas

The functionality of this compiler is dependent on a few basic ideas native to piet:

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
blocks relative to one another.  This will likely create redundant entrances
that are never used between color blocks, but hopefully LLVM can optimize it
out.  White and black aren't necessary, as black is simply a block and not used
for logic after tracing, and white is a deterministic conduit between one exit
and one color block with a NOP, which lets the two be treated as directly
connected.

Each color block, in this way, is a function, or more accurately, a symbol that
can be GOTO'd, in order to prevent possibility of stack overflow, particularly
when a program is expected to loop forever.
