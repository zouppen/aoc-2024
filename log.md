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

I would've sacked the guy who was hopelessly pressing buttons in
front of a computer. Definitely not a professional. That guy was me.

PS. Obfuscated variable and type names for your extra amusement.

View my code: [Day04.hs](src/Day04.hs)

## Day 5, Thursday

Assignment: [Print Queue](https://adventofcode.com/2024/day/5)

Okay, this time it was a Haskell day. Even though I could've been fast,
it was morning and didn't manage to speedcode. It took something like
1h 13min to complete the Part 1 and about two minutes to complete the
second, both without retrys.

Initially, I read the instructions incorrectly and thought what would
come in Part 2 would've been the first part. No bad feelings though,
since I managed to do Part 2 extraordinarily quick, just changing `==`
to `/=`.

I was happy that I had my CS degree, immediately noticed that this is
a case for a user-supplied comparator. So I did it and we can enjoy a
nice and smooth *O(n log n)* complexity in computation. Well, in AoC,
the assignments are simple enough to brute-force anyway. But it gives
a warm fuzzy feeling, like a glass of warm mulled wine. 🍵

Afterwards, while cleaning up code, I noticed I had a bug in the
comparator, where I did compare `a < b` correctly but not `a > b` because
of quick copypasting around. Well, it didn't hit, but a fixed version is
now in the repo.

PS. In case you've wondered why there are JSON instances in my code
even though I'm not processing JSON. It's for the command line switch
`-j` which dumps the parsed output to JSON which is quick to process
with `jq` if I needed to. In most cases I write them afterwards, not
while the timer runs.

View my code: [Day05.hs](src/Day05.hs)

## Day 6, Friday

Assignment: [Guard Gallivant](https://adventofcode.com/2024/day/6)

Alright, it's a day off. It's the Finnish independence day. Last
evening I had couple of beers with bunch of nerds, one of them was
also doing AoC and does it way more competitively. Has a wake up every
morning at 07:00 (05:00 UTC) so he starts immediately when the
assignment goes live. I often start at some point in the morning, the
earliest so far was at 09:30.

This morning I wanted to make it easy and not competitive at all. I
wanted to make a proper parser since the last table based assignment
was yesterday (Day 4) and I started over multiple times. This time I
wanted to parse the input properly so instead of a table I had a set
of obstacles and bounds of the arena. It took too long and my day
schedule went out of the window.

After hours of hacking I finally managed to book train and plane
tickets to
[38C3](https://events.ccc.de/congress/2024/infos/index.html) and even
go to a sauna. It was very relaxing, better than ragecoding AoC
assignments.

The code is rather beautiful in my opinion, but naming of parsers
could be better. The efficiency could be better, since I just iterate
over all possible obstacle positions in part 2. I just didn't have a
good idea about any drastically more efficient algorithm.

Part 2 run time on my laptop was 19 seconds (non-threaded) and under 6
seconds when parallelized.

View my code: [Day06.hs](src/Day06.hs)

## Day 7, Saturday

Assignment: [Bridge Repair](https://adventofcode.com/2024/day/7)

Very haskellish task. Super straightforward, in theory.

At first I was accidentally first doing right-to-left evaluation and
then fixing the algorithm to left-to-right but so that every other
element it was doing right-to-left. The mistake was I was using two
functions, `evalLTR` and `evalRTL` and I was calling the other
recursively. 🤣 I tilted.

Due to tilting the easy assignment went to a try-and-fail loop, and I
got 5 minute ban from AoC. The ban lead to me reading my code with a
thought and found `evalLTR` in `evalRTL` function and the part 1
solved immediately.

Part 2 was just adding a new number concatenation operator which I did
using just Show and Read, not fancy and optimized base 10 shifting.

Run time 3.5 seconds on my laptop without threading.

View my code: [Day06.hs](src/Day07.hs)
