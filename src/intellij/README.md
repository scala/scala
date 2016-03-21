# Building Scala in IntelliJ IDEA

## Requirements

Use the latest IntelliJ release and install the Scala plugin from within the IDE.

## Initial setup

To create the IntelliJ project files:

  - Run `sbt intellij`
  - Open `src/intellij/scala.ipr` in IntelliJ
  - In `File` → `Project Structure` → `Project` → `Project SDK`, create an SDK entry named "1.8" containing the Java 1.8 SDK

The project files are created by as copies of the `.SAMPLE` files, which are under version control.
The actual IntelliJ project files are in `.gitignore` so that local changes are ignored.

## Dependencies

For every module in the IntelliJ project there is a corresponding `-deps` library, for exmaple `compiler-deps` provides `ant.jar` for the compiler codebase.
The `.jar` files in these `-deps` libraries can be easily kept up-to-date by running `sbt intellij` again.
This is necessary whenever the dependencies in the sbt build change, for example when the STARR version is updated.

Note that this command only patches the dependency lists, all other settings in the IntelliJ project definition are unchanged.
To overwrite the project definition files by copying the `.SAMPLE` files again run `sbt intellijFromSample`.

## Switching Branches

The 2.12.x branch contains IntelliJ module files for `actors` and `forkjoin` even though these modules only exist in 2.11.x.
This allows using the same IntelliJ project files when switching to the 2.11.x branch (without causing any issues while working on 2.12.x).

When switching between 2.11.x and 2.12.x, make sure to run `sbt intellij`.
Note that the `Project SDK` is not updated in this process.
If you want to use the Java 1.6 SDK while working on 2.11.x you need to change it manually (`File` → `Project Structure` → `Project` → `Project SDK`).

## Usage

Compiling, running, JUnit tests and debugging should all work.
You can work on the compiler, the standard library, and other components as well.

Note that compilation within IntelliJ is performed in a single pass.
The code is compiled using the "STARR" (stable reference) compiler, as specified by `starr.version` in `versions.properties`.
This is consistent with the sbt build.

Note that the output directory when compiling in IntelliJ is the same as for the sbt build.
This allows building incrementally in IntelliJ and directly use the changes using the command-line scripts in `build/quick/bin/`.

## Updating the `.SAMPLE` files

The command `intellijToSample` overwrites the `.SAMPLE` files using the current project definition files.
