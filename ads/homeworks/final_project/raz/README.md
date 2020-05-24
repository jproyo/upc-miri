# Random Access Zipper - RAZ

This README file only contains instruction to running the program and does NOT contain specifications about the problem or the solution.

## Folder Structure

- **`app`**: Folder containing `main` programs for running the different experiments.
- **`docs`**: Latex and PDF generated report with the formal specification of the solution.
- **`output`**: Raw output files like csv or screenshot gathered from experiments run
- **`src`**: Haskell Source Code with solutions and experiments
- **`test`**: Property based testing on the Data Structure.

## Formal Documentation

In order to see the report go [here](docs/report.pdf)

## Running the Program

### Compiling

Once you did that, you can just run:

```shell
> stack clean
> stack build
```

### Running

For running the program just do the following and you will the see the different experiments to run:

```shell
> stack exec raz -- --help
```


