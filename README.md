# Hydromel Language

Hydromel is a work-in-progress Hardware Description Language (HDL).

It is a *functional* language that supports the description of *synchronous*
digital circuits with a *single clock domain*.

The reference implementation of Hydromel is written in Racket.
It is available under the conditions of the Mozilla Public License 2.0.

## Install

Install [Racket](https://racket-lang.org/), then:

```
git clone https://github.com/aumouvantsillage/Hydromel-lang.git
cd Hydromel-lang/hydromel
raco pkg install --auto
cd ..
```

## Run an example

The following command simulates a counter circuit described in `examples/counter/counter.mel`. It checks the output waveforms, saves all waveforms to a VCD file, and prints them to the standard output.

```
racket examples/counter/counter.rkt
```

If you have installed GTKWave, you can view the waveforms using this command:

```
gtkwave -S examples/counter/counter.tcl examples/counter/counter.vcd
```
