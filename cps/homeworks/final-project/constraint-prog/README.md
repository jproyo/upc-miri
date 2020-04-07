# Box Roll Problem - CP

This README file only contains instruction to running the program and does NOT contain specifications about the problem or the solution.

## Formal Documentation

In order to see the report in which it is described the formal definition of the solution go [here](docs/report.pdf)

## Running the Program

To run the program you are going to need first modify the `Makefile` in order to set the proper environment variables for compiling the program according to you **OS** and **Gecode** instalation.

Once you did that, you can just run:

```shell
> make clean
> make all
```

All the binaries are going to be place in [bin](bin/) directory

For running the program just do:

```shell
> bin/roll-paper < input
```


