# Building Scala using IntelliJ IDEA

## Requirements

Use the latest IntelliJ release and install the Scala plugin from within the IDE.

## Initial setup

To create the IntelliJ project definition,

 - Run `ant init`. This will download some JARs to `./build/deps`, which are included in IntelliJ's classpath.
 - Run `./src/intellij/setup.sh`.
 - Open `./src/intellij/scala.ipr` in IntelliJ.
 - In `File` → `Project Structure` → `Project` → `Project SDK`, create an SDK entry named "1.8" containing the Java 1.8 SDK.

## Usage

Compiling, running, and debugging should all work.  You can work on the compiler, the standard library, and other components as well.

Note that compilation within IntelliJ is performed in `-Dlocker.skip=1` mode. Code is compiled not by bootstrapping the current compiler sources, but simply by using the "STARR" (stable reference) compiler, as specified by `starr.version` in `versions.properties`.
