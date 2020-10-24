# Box Wrapping Problem - CP

This README file only contains instruction to running the program and does NOT contain specifications about the problem or the solution.

## Folder Structure

- **`bin`**: Folder with compiled binaries. The content of this folder is deleted on `make clean` task.
- **`docs`**: Latex and PDF generated report with the formal specification of the solution.
- **`instances`**: Instances provided to test against the solution. Input of the program.
- **`out`**: Output with the solution of each instance.

    > NOTE: We are delivering the solutions obtained in the best running of the program. If you want to clean up this folder, please first save this outputs files in other place.

- **`src`**: C++ Source Code Solution with the `Space` subclass and all the utilities needed to solve the problem
- `Makefile`: To build and run the program
- `run_script.sh`: Scripting file to run all instances under **`instance`** folder.

    > NOTE: Running this will overwrite out files and you might lost previous running. Please check Running with Script section.

## Formal Documentation

In order to see the report in which it is described the formal definition of the solution go [here](docs/report.pdf)

## Running the Program

### OS Configuration

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

### Compiling

Once you did that, you can just run:

```shell
> make clean
> make all
```

All the binaries are going to be place in [bin](bin/) directory

### Help

```shell
> bin/boxw -h
```

### Running

For running the program just do:

```shell
> bin/boxw < input

```

## Running with Script

In the root of this project there is a script file called `run_script.sh`. With this file you can run all the instances provided in `instance` folder and it is going to output the results in `out` folder.

> WARNING: Running this script might overwrite the output files already present in out folder if those output has the same name.

```shell
> ./run_script.sh
```

