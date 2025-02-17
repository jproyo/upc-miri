# Closeness Centrality - CSN

This README file only contains instruction to running the program and does NOT contain specifications about the problem or the solution.

## Folder Structure

- **`bin`**: Folder with compiled binaries. The content of this folder is deleted on `make clean` task.
- **`docs`**: Latex and PDF generated report with the formal specification of the solution.
- **`data`**: Language models that input the program
- **`results`**: Results with experiment conducted. 
> PLEASE DO NOT DELETE THIS FOLDER OR ITS CONTENT
- **`reports`**: Output of running the program with raw data

    > NOTE: We are delivering the solutions obtained in the best running of the program. If you want to clean up this folder, please first save this outputs files in other place.

- **`src`**: C++ Source Code with the Solution
- `Makefile`: To build and run the program
- `run_closeness.sh`: Scripting file to run for all Languages Table 1 and Closeness Centrality exact measure.

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
> bin/closeness -h
> bin/montecarlo -h
```

### Running for a Single language

- Closeness Centrality for real Model

```shell
> bin/closeness "data/Basque_syntactic_dependency_network.txt"
```

- Montecarlo Approximation for Switching Model

```shell
> bin/montecarlo "data/Basque_syntactic_dependency_network.txt" T Q 1
```
Where `T` is the number of iterations of Montecarlo Method and `Q` the number of Switching that is going to be multiplied by the number of Edges to get the total number of switching.

- Montecarlo Approximation for Binomial Model

```shell
> bin/montecarlo "data/Basque_syntactic_dependency_network.txt" T Q 2
```
Where `T` is the number of iterations of Montecarlo Method and `Q` the number of Switching that is going to be multiplied by the number of Edges to get the total number of switching.

### Running with Script


For running the program just do:

```shell
> sh run_closeness.sh
```

> WARNING: This script might take more than 1 hour to finish

or 

```shell
> sh run_montecarlo.sh
```

> WARNING: This script might take more than 12 HOURS TO FINISH.


