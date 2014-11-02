haskell-diamond-square
======================

Haskell implementation of the Diamond-Square algorithm for heightmap generation.

Example
=======
Running:
```
haskell-diamond-square -s1 -w1000 -h500 -bmap.bmp
```

The output is:
```
Diamond square generator
------------------------
Seed      : 1
Width     : 1000
Height    : 500
Bitmap    : 'map.bmp'
Heightmap : <not produced>
```

And the bitmap generated is:
![](https://raw.githubusercontent.com/ftomassetti/haskell-diamond-square/master/examples/map.bmp)

Usage
=====

The program accepts the following options. Note that -b or -m should be specified, otherwise the program exit without doing nothing.

| Example       | Effect |  Notes |
|---------------|---|---|---|---|
|-s1            | Specify the seed to use  |   |
|-w1000         | Width of the map |  Should be in [1,8192] |
|-h500          |   | Should be in [1,8192]  |
|-bmap.bmp      | Specify where to save the bitmap | |
|-mheightmap.hm | Specify where to save the map in binary format  | This is serialized using [Google Protocol Buffers](https://developers.google.com/protocol-buffers/) |

Author
======
The implementation of the algorithm was done by [mikeswoods](https://github.com/mikeswoods/haskell-diamond-square), I just wrapped it in a command line utility.
