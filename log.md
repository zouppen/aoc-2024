# Logbook 2024

## Day 1, Sunday

Started in the evening. Started using Haskell because I had been using
Haskell in a company project in last few weeks and had the libraries
in fresh memory.

I noticed it would've been possible to do it with LibreOffice
spreadsheet, but decided to write a decent parser since I started late
and being quick wasn't important and it was Sunday so had free time.

Joined to the private leaderboard of my former student club.

Very easy, did both stars it in less than 30 min from scratch while
wandering in Hackage library docs.

View my code: [Day01.hs](src/Day01.hs)

## Day 2, Monday

A little bit dull CS1 grade assignment. First part could've be done in a
spreadsheet but the second probably not since it required branching.

Well, maybe it could've been done without loops with clever conditions
and state passing but I didn't want it to become too complex and
unreadable mess. In Haskell I have a garbage collector and I'm not afraid
to use it.

I was so bored after this that I made proper command line parser and
small framework to help solving the assignments in upcoming days.

I'm so tired about the culture war around master/main terminology so I
decided to rename my AoC repository default branch as *pukki* after
*joulupukki*, the Finnish word for Santa Claus.

View my code: [Day02.hs](src/Day02.hs)

## Day 3, Tuesday

I really didn't do the full parser initially. I hacked a very quick
and dirty parser first and I wrote a proper parser in Part 2. I reused
the more complete parser from Part 2 and processed the output with
input that has been pre-filtered to remove *Don't* statements.

I liked the assignment because it was no longer a spreadsheet
task. Finally got the possibility to benefit from a proper
backtracking parser library.

Maybe 30 min solution again. I was quicker than expected because I
already had the bells and whistles at hand.

View my code: [Day03.hs](src/Day03.hs)
