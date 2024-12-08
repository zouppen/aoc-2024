# Helpers

These helpers make my day easier by downloading puzzle input and
creates fancy PDF assignments, which can be sent to a printer at 05:00
UTC. 🙂

## Requirements for `new_day`

First, install dependencies. On Fedora:

```sh
sudo dnf install php-cli php-xml pandoc
```

or on Debian:

```sh
sudo apt install php-cli php-curl php-xml pandoc
```

Then, steal AoC cookie and put it to `../cookies.txt`. To make the task
easier, use [cookies.txt Firefox
add-on](https://addons.mozilla.org/fi/firefox/addon/cookies-txt).

## Run

To get the puzzle text, input and log book template for day 3 of 2024, run:

```sh
./new_day https://adventofcode.com/2024/day/3
```
