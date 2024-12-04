# Logbook 2024

## Day 1, Sunday

Assignment: [Historian Hysteria](https://adventofcode.com/2024/day/1)

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

Assignment: [Red-Nosed Reports](https://adventofcode.com/2024/day/2)

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

Assignment: [Mull It Over](https://adventofcode.com/2024/day/3)

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

## Day 4, Wednesday

Assignment: [Ceres Search](https://adventofcode.com/2024/day/4)

Not my day, not the right tools.

Started over 4 times after getting super desperate about how to
process this in a functional language.

Noticed that Text index function is O(n) rather than O(1) so decided
to fix the "framework" to use ByteStrigs instead of Text. This
should've been the first thought and not an afterthought since AoC
never uses weird UTF-8 input. Let's see if tomorrow there will be 🎅
in the input and I'll start spinning again.

There was nothing to parse so the solution was to just extract bounds
from the input and write the index to coordinate conversion functions
so that they manage with the newlines.

The correct language in this case would have been plain old C. Very
straightforward character comparison and stuff. I just didn't want to
give up. I was **that** close to make it in C with FFI interface.

After getting my shit together after hours of running around aimlessly
I had one incorrect answer because I forgot top right to bottom left
and bottom left to top right diagonals. After adding them, I got a
correct answer.

The second part required new approach but I was able to reuse the
coordinate conversions. This time I got everything correct on first
try but getting there lead me to matrix algebra and stuff like that
which I decided not to use.

I would've sacked the guy who was hopelessly pressing of buttons in
front of a computer. Not definitely a professional. That guy was me.

PS. Obfuscated variable and type names for your extra amusement.

View my code: [Day04.hs](src/Day04.hs)
