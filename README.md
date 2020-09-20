# Countdown

This tiny project finds a solution to the [numbers round on the British gameshow Countdown](https://en.wikipedia.org/wiki/Countdown_%28game_show%29#Numbers_round).

![Image of Numbers Round](https://hips.hearstapps.com/digitalspyuk.cdnds.net/18/15/1523712878-screen-shot-2018-04-14-at-143341.png)


The goal of the game is to find a way to combine a list of six numbers into a single large number using Addition, Subtraction, Multiplication, or Division. Not all six numbers need to be used, but at any point in the chain of operations the result must be an integer greater than zero.

This program solves this problem using a lazily-evaluated Depth First Search.

|Solver Properties||
|---|---|
|Complete|True|
|Optimal|False|
|Time Complexity|O(n!) where n = number list length|
|Space Complexity|O(n) where n = number list length|


Usage:
```
Usage: countdown [-h] DES NUM [NUM ...]
British gameshow mathematical solver.

  -h  --help  Print this help message
  -r  --rpn   Return output in Reverse Polish Notation

Examples:

  $ countdown 666 25 3 8 1 7 4
  666 = ((25 + 8) * 3 - 4) * 7 + 1
  
  $ countdown 420 69 700 7 28 3 14
  420 = (69 + 28 - 7) * 14 / 3
```
