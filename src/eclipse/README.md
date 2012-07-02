Eclipse project files
=====================

Import all projects inside Eclipse by choosing File/Import Existing Projects
and navigate to src/eclipse. Check all projects and click ok.

IMPORTANT
=========

You need to define a `path variable` inside Eclipse. Define SCALA_BASEDIR in 
Preferences/General/Workspace/Linked Resources. The value should be the absolute 
path to your scala checkout. All paths in project files are relative to this one,
so nothing will work before you do so.

DETAILS
=======

The compiler project depends on the library, reflect, asm and fjbg projects. The
builder will take care of the correct ordering, and changes in one project will
be picked up by the dependent projects.

The output directory is set to be build/quick, so the runner scripts in quick
work as they are (run an ant build to have the generated once)