# Box Wrapping Problem - LP

This README file only contains instruction to running the program and does NOT contain specifications about the problem or the solution.

## Folder Structure

- **`bin`**: Folder with compiled binaries. The content of this folder is deleted on `make clean` task.
- **`docs`**: Latex and PDF generated report with the formal specification of the solution.
- **`instances`**: Instances provided to test against the solution. Input of the program.
- **`out`**: Output with the solution of each instance.

    > NOTE: We are delivering the solutions obtained in the best running of the program. If you want to clean up this folder, please first save this outputs files in other place.

- **`src`**: C++ Source Code Solution classes and all the utilities needed to solve the problem
- `Makefile`: To build and run the program
- `run_script.sh`: Scripting file to run all instances under **`instance`** folder.
- `run_checker.sh`: Scripting file to check all results under **`out`** folder.

    > NOTE: Running this will overwrite out files and you might lost previous running. Please check Running with Script section.

## Formal Documentation

In order to see the report in which it is described the formal definition of the solution go [here](docs/report.pdf)

## Running the Program

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

