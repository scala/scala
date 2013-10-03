Eclipse project files
=====================

The following points describe how to get Scala to run in Eclipse:

0. To get Scala to work inside of Eclipse Kepler it is necessary to build the Scala IDE by your own
because for the moment there is no update site provided for the newest development version
of Scala. To do so enter the following commands one after the other:

        git clone https://github.com/scala-ide/scala-ide.git
        cd scala-ide
        ./build-all.sh clean install -Pscala-2.11.x -Peclipse-kepler -DskipTests

  After that you have an update site in `scala-ide/org.scala-ide.sdt.update-site/target/site`, which needs to be
installed in Eclipse.

0. The second thing that needs to be done is building Scala in order to get all necessary
dependencies. To do that simply enter

        ant

  and wait until it is completed. To verify that everything has been built successfully, execute the REPL that can be found
at `scala/build/pack/bin/scala`.

0. Import all projects inside of Eclipse by choosing `File/Import Existing Projects`
and navigate to `scala/src/eclipse`. Check all projects and click ok.

0. You need to define a `path variable` inside Eclipse. Define `SCALA_BASEDIR` in 
`Preferences/General/Workspace/Linked Resources`. The value should be the absolute 
path to your Scala checkout. All paths in the project files are relative to this one,
so nothing will work before you do so.

  The same `SCALA_BASEDIR` variable needs to be defined as a `classpath variable` in
`Java/Build Path/Classpath Variables`.

  Additionally, we start using Maven dependencies (e.g. `JUnit`) so you need to define another
`classpath variable` inside Eclipse. Define `M2_REPO` in `Java/Build Path/Classpath Variables`
to point to your local Maven repository (e.g. `$HOME/.m2/repository`).

  Lastly, the JRE used by Eclipse needs to know the path to the `JLine` library, which is used by the REPL.
To set the JAR file, navigate to `Java/Installed JREs`, select the default JRE, press `Edit/Add External JARs...`
and enter the path to JLine whose location is `SCALA_BASEDIR/build/deps/repl/jline-2.11.jar` (`SCALA_BASEDIR` cannot be entered,
it needs to be replaced with its absolute path).

0. The Eclipse Java compiler does not allow certain calls to restricted APIs in the
JDK. The Scala library uses such APIs, so you'd see this error:

        Access restriction: The method compareAndSwapObject(Object, long, Object, Object)
        from the type Unsafe is not accessible due to restriction on required library.

  You can *fix* it by allowing calls to restricted APIs in `Java/Compiler/Errors/Warnings/Deprecated and Restricted API` 
settings.

0. Project files are tracked by Git, so adding them to `.gitignore` won't prevent them
from being shown as dirty in `git status`. You can still ignore them by telling Git to
consider them unchanged:

        git update-index --assume-unchanged `find src/eclipse -iname .classpath -or -iname .project`

  If you want to go back to normal (for instance, to commit your changes to project files), run:

        git update-index --no-assume-unchanged `find src/eclipse -iname .classpath -or -iname .project`

DETAILS
=======

The compiler project depends on the library, reflect, and asm projects. The
builder will take care of the correct ordering, and changes in one project will
be picked up by the dependent projects.

The output directory is set to be `build/quick`, so the runner scripts in quick
work as they are (they are generated after an ant build).
