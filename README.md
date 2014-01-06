This is the repository for the [Scala Programming Language](http://www.scala-lang.org).

  - [Report an issue](https://issues.scala-lang.org);
  - [Read about the development of the compiler and the standard library](http://docs.scala-lang.org/scala/);
  - [Check our Jenkins status](https://scala-webapps.epfl.ch/jenkins/);
  - [Download the latest nightly](https://scala-webapps.epfl.ch/jenkins/job/scala-nightly-main-master/ws/dists/latest/*zip*/latest.zip);
  - ... and contribute right here! Please, first read our [policy](http://docs.scala-lang.org/scala/pull-request-policy.html),
and [sign the contributor's license agreement](http://typesafe.com/contribute/cla/scala).

Also in this branch is the language extension named SubScript.
For information about SubScript, you can visit the web site:

* [http://www.subscript-lang.org](http://www.subscript-lang.org)

##SubScript Compiler issues
The current SubScript compiler (a branch of `scalac`) is good enough to compile 
the given example programs, but there are some limitations and bugs:

* Only language features used in the examples are present and tested.
 Other features, most notably communication, is TBD

* `here` and `there` are not yet implicit values. Therefore you would need 
 to write things like `@{gui(there)}:` instead of `@gui`

* Local values and variables in scripts need both be typed and initialized.
 E.g., `val b=true` is not accepted yet; instead write `val b:Boolean = true`

* The "?" syntax for actual output parameters, actual constrained parameters and
 actual adapting parameters are not yet completely handled. For the time being 
 you may have to use alternative notations. E.g.,

 ```
 ?p           ==>   ActualOutputParameter     (p, (v:Int)=>p=v)
 ?p ?if(cond) ==>   ActualConstrainedParameter(p, (v:Int)=>p=v, (v:Int)=>cond)
 ??p          ==>   ActualAdaptingParameter   (_p)
 Note the underscore in the latter line.
 ```

* The compiler may crash on values in a script expression for which 
 no implicit conversion to a script exist. 
 This may for instance happen if you specify `x` in a script
 expression to denote an event that the `x` key is pressed, and if you forgot
 to provide an implicit script `key(c:Char)`

##Note on the repository layout
Follows the file layout of the Scala repository. 
Two folders have been added for SubScript:

```
scala/
    src/                      All the source files of Scala.
        subscript/            The sources of the core SubScript library.
        subscript/examples    The sources of SubScript example applications
```

Note: the file `subscript.swing.Scripts` uses the SubScript syntax and can 
therefore not be compiled with the starr compiler; it needs to be compiled
with the locker compiler or quick compiler.

SubScript examples are not built with the usual ant targets.

##Contributing to SubScript

Contributors are welcome. The official SubScript website is: [www.subscript-lang.org](www.subscript-lang.org).

As of writing there are no mailing lists, and bug and issue trackers.

Thank you!

The SubScript Team

