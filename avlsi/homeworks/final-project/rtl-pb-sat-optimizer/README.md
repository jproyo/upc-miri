# RTL Pseudo-Boolean SAT Optimizer

This README file only contains instruction to running the program and does NOT contain specifications about the problem or the solution.

## Folder Structure

- **`app`**: Folder that contains the `Main.hs` program that execute `RTLOptimizer.hs` as a library.
- **`docs`**: Latex and PDF generated report with the formal specification of the solution.
- **`input`**: File Input with some given Schedule example to be optimized.
- **`result`**: This is the default folder where the program left `dot` and `png` files with the results of the Graph after optimization
- **`src`**: Haskell Source Code Solution needed to solve the problem

    > NOTE: Running this will overwrite out files and you might lost previous running. Please check Running with Script section.

## Formal Documentation

In order to see the report in which it is described the formal definition of the solution go [here](docs/presentation.pdf)

## Prerequisites to running the program

You need to install **Stack** which is building and dependency manager tool for **Haskell**

Stack documentation for installing can be found [here](https://docs.haskellstack.org/en/stable/README/) but i will provide the basic installation process which in Linux is quite easy:

```shell
curl -sSL https://get.haskellstack.org/ | sh
```

or

```shell
wget -qO- https://get.haskellstack.org/ | sh
```

After this you need to have `stack` executable in your system.

> IMPORTANT: The first time you build the project is going to take a lot of time. Maybe 20 minutes or so, because it needs to download all dependencies, GHC (which is Haskell compiler) and compile from scratch. In contrast with C++, Dependencies in Haskell are pure Source Code that is downloaded to your machine and compile there. Once the first building is done after the compilation it is a matter of seconds.

## Running the Program

### Compiling

Once you did that, you can just run in the root of the project where `stack.yaml` is located:

```shell
> stack build
```

### Help

```shell
> stack exec solver -- --help
```

### Running

For running the program for one input just do:

```shell
> stack exec solver -- -f input/example.sch -o result
```

## Result of the program

Depends on your `-o` parameter the program is going to leave 4 **png** files in the output folder. By default this folder is **result**.

- **`input_alap.png`**: Input ALAP Schedule to be optimized
- **`input_asap.png`**: Input ASAP Schedule to be optimized
- **`time_optimized.png`**: Time Schedule Optimization Result
- **`resource_optimized.png`**: Resource Schedule Optimization Result