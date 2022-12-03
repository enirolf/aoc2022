# AOC2022

yay

## Setup

```sh
$ stack setup # only required the first time
$ stack build
$ stack install # optional, for adding the `add_day` executable to $PATH
```

## Running the code for day X

Don't be fancy, just use ghci:
```sh
$ stack ghci src/AOC/DayX.hs
λ dayX(a|b) <MODE>
```
`MODE` is either `Test` or `Full`



## Adding days

Make sure `add_day` is installed (or prepend `stack run`):
```sh
$ add_day X
```
