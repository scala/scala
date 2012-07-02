Eclipse project files
=====================

Import all projects inside Eclipse by choosing File/Import Existing Projects
and navigate to src/eclipse. Check all projects and click ok.

IMPORTANT
=========

1. You need to define a `path variable` inside Eclipse. Define SCALA_BASEDIR in 
Preferences/General/Workspace/Linked Resources. The value should be the absolute 
path to your scala checkout. All paths in project files are relative to this one,
so nothing will work before you do so.

2. The Eclipse Java compiler does not allow certain calls to restricted APIs in the
JDK. The Scala library uses such APIs, so you'd see this error:

        Access restriction: The method compareAndSwapObject(Object, long, Object, Object)
        from the type Unsafe is not accessible due to restriction on required library.
You can *fix* it by allowing calls to restricted APIs in `Java=>Compiler=>Errors/Warnings=>Deprecated and Restricted API` 
settings.

3. The IDE guesses the Scala library version by looking for `library.properties` inside 
the library jar. The `scala-library` project does not have such a file, so you will see
an error about incompatible libraries. You can work around it by adding a `library.properties`
inside `src/library` with the following contents:

        #Mon, 04 Jun 2012 02:08:56 +0200
        version.number=2.10.0-20120603-141530-b34313db72
        maven.version.number=2.10.0-SNAPSHOT
        osgi.version.number=2.10.0.v20120603-141530-b34313db72
        copyright.string=Copyright 2002-2011, LAMP/EPFL

4. Project files are tracked by Git, so adding them to `.gitignore` won't prevent them
from being shown as dirty in `git status`. You can still ignore them by telling Git to
consider them unchanged:

        git update-index --assume-unchanged `find src/eclipse -iname .classpath -or -iname .project`

If you want to go back to normal (for instance, to commit your changes to project files), run:

        git update-index --no-assume-unchanged `find src/eclipse -iname .classpath -or -iname .project`

DETAILS
=======

The compiler project depends on the library, reflect, asm and fjbg projects. The
builder will take care of the correct ordering, and changes in one project will
be picked up by the dependent projects.

The output directory is set to be build/quick, so the runner scripts in quick
work as they are (run an ant build to have them generated once)