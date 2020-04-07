# Box Wrapping Problem - CP

This README file only contains instruction to running the program and does NOT contain specifications about the problem or the solution.

## Formal Documentation

In order to see the report in which it is described the formal definition of the solution go [here](docs/report.pdf)

## Running the Program

To run the program you are going to need first modify the `Makefile` in order to set the proper environment variables for compiling the program according to you **OS** and **Gecode** instalation.

Open `Makefile` and edit the first 3 lines according to your **Operative System**. In my case which is a **OSX** operative system this is my setup

```text
CXX  = g++ -std=c++11 -F/Library/Frameworks
DIR  = /usr/local
LIBS = -framework gecode
```

- `CXX` variable contains compiler with flags
- `DIR` variable contains the location of include libs for linking
- `LIBS` variable contains additional flag for dynamic linking libraries. In our case geocode.

> NOTE: In OSX Geocode lib is different than other system like Windows or Linux

Once you did that, you can just run:

```shell
> make clean
> make all
```

All the binaries are going to be place in [bin](bin/) directory

For running the program just do:

```shell
> bin/boxw < input

```


