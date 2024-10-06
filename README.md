# Tournesol - a calculator for the CLI that understands units

A simple, scriptable, scalar expression and units calculator for the terminal. [Tournesol](https://www.tintin.com/en/characters/professor-calculus) - AKA Professor Calculus - is (quoting the linked page)...

> ...very absent-minded, hard of hearing, intuitive and very sentimental. He is capable of the most unexpected, and sometimes really weird, connections with reality, by simply using his pendulum. First and foremost he is a fine handyman, then a clever inventor. Calculus is intrigued by everything, including botany, physics, electronics and dowsing. He has all the traits of a scientist who is determined to make his ideas work. Self-assurance, and obstinacy verging on irritability.

![](https://cdn001.tintin.com/public/tintin/img/static/professor-calculus/tournesol-calculus.jpg)

## Setup

### Install Haskell and Stack

1. Install [ghcup](https://www.haskell.org/ghcup/)
2. Run `ghcup` to install [stack](https://docs.haskellstack.org/en/stable/README/)

### Build from Source

1. Clone the repository
2. From within the repository directory run `stack build` and/or `stack install`
3. Optionally ensure it's all good with `stack test`

## Examples

```bash
# simple expressions
tn '1+2'

# more complex expressions with unit conversions
tn '300 W * 2 hr : BTU'
tn '100 mL + 1 cup : in^3'

# shift (_) to pull values from stdin
tn '_ / _ in' '3 yd^2' 4

# evaluate piped, delimited values
tn '_ deg/s : rev/min' < values.txt

# call functions that understand units
tn '[cos 200 grad]'
```

## Scripts and Custom Functions

You can define your own dimensions and units in scripts:

```
dim luminosity;

# define a fundamental unit
unit cp "candlepower" = base luminosity;
```

It is also possible to simply define a new unit as its own dimensions. This can be useful for solving custom problems.

```
# cars.tn

unit car;
unit usd;

const pricePerCar = 10000 usd/car;
```

And now you can load the script and use it in your calculations:

```bash
tn -f cars.tn '3 car * [pricePerCar]'
```

## Interactive Mode

It's also possible to simply run in interactive mode:

```
$ tn
Tournesol v1.0.0, (c) Jeffrey Massung
>> 1 + 1
2
>> 10 V * 0.5 A * 2 min
600 V A s
>> 3 hp : W

```

## Dimensions and Units

The following table of all units and fundamental dimensions are understood by Tournesol. Units prefixed with a `-` may also be prefixed with the SI prefixes from atto- to exa- (see: [https://physics.nist.gov/cuu/Units/prefixes.html](https://physics.nist.gov/cuu/Units/prefixes.html)).

| Dimension | Base dimensions | Units
|-|-|-
| angle | `[angle]` | rad, grad, deg, rev, turn
| area | `[length^2]` | ha, acre
| capacitance | `[current^2;time^4;length^-2;mass^-1]` | -F
| charge | `[current;time]` | -C
| current | `[current]` | -A
| energy | `[mass;length^2;time^-2]` | -J, -eV, BTU, thm
| force | `[mass;length;time^-2]` | -N, lbf, pond
| frequency | `[time^-1]` | -hz
| length | `[length]` | -m, mil, in, h, ft, yd, ftm, ch, fur, mi, lea, cable, nmi, link, rod, pc, au, ly
| mass | `[mass]` | -g, oz, lb, st, cwt, t
| power | `[mass;length^2;time^-3]` | -W, hp
| pressure | `[mass;length^-1;time^-2]` | -Pa, psi, bar
| resistance | `[mass;length^2;current^-4;time^-3]` | -O
| speed | `[length;time^-1]` | kph, mph, kn
| storage | `[storage]` | -B, -b
| time | `[time]` | s, min, hr, day, week
| voltage | `[mass;length^2;current^-1;time^-3]` | -V
| volume | `[length^3]` | -L, tsp, tbp, floz, cup, pt, qt, gal

_NOTE: Storage units (byte and bit) use base-2 SI prefixes (e.g. MB - megabyte - is 1048576 bytes, not 1000000)._

## FAQ

***Why is `1/2ft` equal to `0.5 ft^-1` and not `0.5 ft`?***

Math operators have a lower precedence than unit assignment. You can either use parenthesis or the `%` division operator which has a higher precedence:

```
>> 1/2 ft
0.5 ft^-1

>> (1/2) ft
0.5 ft

>> 1%2 ft
0.5 ft
```

## Built-in Functions

The following functions are built-in and available for use:

```
# -------------------------------------------------
# Conditional functions

function if [any:test; any:then; any:else]

# -------------------------------------------------
# Constants functions

function pi []

# -------------------------------------------------
# Trigometric functions

function sin [rad]
function cos [rad]
function tan [rad]
function sinh [rad]
function cosh [rad]
function tanh [rad]
function asin [none]
function acos [none]
function atan [none]
function asinh [none]
function acosh [none]
function atanh [none]

# -------------------------------------------------
# Math functions

function abs [any]
function signum [any]
function sqrt [any]
function log [none]
function exp [none]
function truncate [any]
function floor [any]
function ceil [any]
function round [any]

# -------------------------------------------------
# Geometry functions

function circumference [dim length:r]
function perimeter [dim length:b; dim length:h]

function areaOfCircle [dim length:r]
function areaOfRect [length:b;length:h]
function areaOfTriangle [length:b;length:h]

function volumeOfSphere [length:r]
function volumeOfCylinder [length:r;length:h]
function volumeOfPrism [length:b;length:h;length:d]
function volumeOfTrianglePrism [length:b;length:h;length:d]
function volumeOfCube [length]
function volumeOfSquarePyramid [length:b;length:h]
function volumeOfTetrahedron [length]
function volumeOfTorus [length:r;length:R]
```
