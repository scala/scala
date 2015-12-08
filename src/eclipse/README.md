Eclipse project files
=====================

For important details on building, debugging and file encodings, please see [the excellent tutorial on scala-ide.org](http://scala-ide.org/docs/tutorials/scalac-trunk/index.html).

The following points describe how to build Scala using Eclipse.

0. Download the [Scala IDE bundle](http://scala-ide.org/download/sdk.html). It comes preconfigured for optimal performance.

0. Run `ant init` to download some necessary jars.

0. Import the project (in `src/eclipse`) via `File` → `Import Existing Projects` and navigate to `scala/src/eclipse`. Check all projects and click ok.

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

0. The 2.12, sources of Scala need to be built with a 2.12 version of the compiler. One can configure a 2.12 Scala installation
in Eclipse. In order to do this, go to `Window -> Preferences -> Scala -> Installations` and add a 2.12 installation. You can
either download a prepackaged version of 2.12 from the Scala homepage or you add the Scala installation that is part of the
`build/pack/lib` directory. The latter is required in case you absolutely need to depend on a nightly build of the compiler to
compile the compiler itself. Once the 2.12 Scala installation is created you need to select all Scala projects, do a right click
and select `Scala -> Set the Scala installation` where you have to choose the newly created 2.12 Scala installation.

If it doesn’t compile
=====================

The likely reason is that the build path of the imported projects isn’t correct. This can happen for instance
when the [version.properties](https://github.com/scala/scala/blob/master/versions.properties) file is updated,
and Eclipse .classpath of the different projects isn’t updated accordingly. The fix is simple, manually inspect
the build path of each project and make sure the version of the declared dependencies is in sync with the version
declared in the `version.properties` file. If it isn’t, update it manually and, when done, don’t forget to share
your changes via a pull request.
(We are aware this is cumbersome. If you feel like scripting the process, pull requests are of course welcome.)

Launching & Debugging scalac
============================

Read [here](http://scala-ide.org/docs/tutorials/scalac-trunk/index.html#Launching_and_Debugging_scalac).

DETAILS
=======

The compiler project depends on the library, reflect, and asm projects. The
builder will take care of the correct ordering, and changes in one project will
be picked up by the dependent projects.

The output directory is set to be `build/quick`, so the runner scripts in quick
work as they are (they are generated after an ant build).
