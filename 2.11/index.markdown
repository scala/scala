---
layout: default
title: Overview of Scala 2.11
---

The current 2.11 milestone is [M4](https://github.com/scala/scala/releases/tag/v2.11.0-M4).

To contribute to this overview, [please submit a pull request](https://github.com/scala/scala/tree/gh-pages/2.11/index.markdown).

# Smaller
We're modularizing the standard library and the compiler to allow more selective use of Scala's features, and to facilitate contributions to more clearly delineated sub-projects.

The library opens up along the following fault lines: scala-library, scala-xml, scala-parser-combinators, ... (TODO).
The compiler platform will provide the following services: scala-compiler, scala-interactive, scala-scaladoc, and scala-repl.

We're also slimming down by removing dead or deprecated code, and enabling future weight loss through aggressive deprecation:

  - The old implementations of the Pattern Matcher and the Bytecode Emitter have been removed, as they were replaced wholesale in Scala 2.10. The experimental .NET backend had been scrapped, and the search and destroy mission in #1648 snuffed ~5000 chunks of dead code. 
  - The following packages have been deprecated:
    - `scala.actors`: see the [actors migration guide](http://docs.scala-lang.org/overviews/core/actors-migration-guide.html)
    - `scala.text`
    - TODO

## XML
The package scala.xml has been moved out of scala-library.jar.
To compile code that contains XML literals, add a dependency on scala-xml or your preferred alternative.

# Faster
Branch elimination through constant analysis #2214
Improve performance of reflection SI-6638

# Stronger

## Language

  - Case classes with > 22 parameters are now supported SI-7296
  - Infer bounds of existential types SI-1786

## REPL

The REPL is improved with several new commands that ease its usage. An overview of the commands can be seen by typing `:help` into the REPL.

### `:paste` ([#2725](https://github.com/scala/scala/pull/2725))

`:load` can only interpret a file from top to bottom which is for example a problem when there is a reference to a definition that is defined later. `:paste` is overworked to solve this limitation. It now can not only load a file but also handle it as a single unit.

Contents of file `test.scala`:
```scala
// Foo has a companion object
class Foo { private val foo = 7 }
object Foo { def apply(f: Foo) = f.foo }
Foo(new Foo)
```

REPL session:
```scala
scala> :paste test.scala
Pasting file y.scala...
defined class Foo
defined object Foo
res4: Int = 7
```

Theres is also the `-raw` option available, which denotes that the following is not a script anymore, but a normal Scala file:

```scala
scala> :paste -raw
// Entering paste mode (ctrl-D to finish)

package abc
case class Foo(bar: Int)

// Exiting paste mode, now interpreting.


scala> abc.Foo(5)
res5: abc.Foo = Foo(5)
```

Furthermore, there is a bug fixed that led to the problem that no error message is reported when the read code was incomplete ([#2672](https://github.com/scala/scala/pull/2672)).

### `:save` ([#2697](https://github.com/scala/scala/pull/2697))

With `:save` it is now possible to store the current REPL session to a file which allows one to extend the current state of the REPL at a later time:

```scala
scala> val i = 7
i: Int = 7

scala> val j = 8
j: Int = 8

scala> i*j
res0: Int = 56

scala> :save session.scala

scala> :load session.scala
Loading session.repl...
i: Int = 7
j: Int = 8
res1: Int = 56
```

The `:save` command does only save the Scala commands to file not the full REPL output:

```
$ cat session.scala
val i = 7
val j = 8
i*j
```

### `:settings` ([#2701](https://github.com/scala/scala/pull/2701))

Sometimes one get warnings from the compiler about some code snippets. Previously it was needed to switch to `:power` mode in order to enable a full output of the warnings. Changing some other variables required a switch to the `:power` mode too. With this release it is possible to do this directly with the `:settings` command. By prefixing an property with a `+` sign the setting is enabled, an `-` disables it:

```scala
scala> new BigInt(java.math.BigInteger.TEN)
res16: scala.math.BigInt = 10

scala> new BigInt(java.math.BigInteger.TEN) {}
warning: there were 1 deprecation warning(s); re-run with -deprecation for details
res17: BigInt = 10

scala> :settings +deprecation

scala> new BigInt(java.math.BigInteger.TEN) {}
<console>:11: warning: inheritance from class BigInt in package math is deprecated: This class will be made final.
              new BigInt(java.math.BigInteger.TEN) {}
                  ^
res18: BigInt = 10

scala> :settings -deprecation

scala> new BigInt(java.math.BigInteger.TEN) {}
res19: BigInt = 10
```

### `:edit` ([#2706](https://github.com/scala/scala/pull/2706))

This command allows a user to change already inserted commands. It is possible to change a range of lines. The syntax is:

- line number: 123 (only the line 123)
- range: 123-130 (the lines 123 to 130)
- offset: 123+7 (the lines 127 to 130)
- remaining: 123- (all lines up from 123)
- previous: -10 (the last ten lines)

The environment variable EDITOR is used to specify the editor to invoke. If EDITOR is not set or if the `:line` command is used instead of `:edit`, the selected text is added to the end of the history in order to allow fast editing by navigating through the history with the arrow keys.

```scala
scala> :history
<snip>
2505  val i = 5
2506  val j = 7
2507  :history

scala> :edit 2505+2
+val i = 5
+val j = 12
i: Int = 5
j: Int = 12

scala> :history
<snip>
2505  val i = 5
2506  val j = 7
2507  :history
2508  :edit 2505+2
2509  val i = 5
2510  val j = 12
2511  :history
```

### `:javap` ([#1880](https://github.com/scala/scala/pull/1880))

The command to dissasemble Java output got some bugfixes and new features. It is now possible to filter out members of classes:

- `Bar#foo` filters out all `foo` members
- `Bar#` filters out all `apply` members
- `-fun Bar#foo` filters out all anonfuns of `foo` members
- `-fun Bar#` filters out all anonfuns of `apply` members
- `-app Bar` filters out `Bar.delayedInit`

Example usage:

```scala
scala> object Foo { def apply = 10; def bar = 5 }
defined object Foo

scala> :javap Foo#bar
  public int bar();
    flags: ACC_PUBLIC
    Code:
      stack=1, locals=1, args_size=1
         0: iconst_5      
         1: ireturn       
      LocalVariableTable:
        Start  Length  Slot  Name   Signature
               0       2     0  this   LFoo$;
      LineNumberTable:
        line 7: 0

scala> :javap Foo#
  public int apply();
    flags: ACC_PUBLIC
    Code:
      stack=1, locals=1, args_size=1
         0: bipush        10
         2: ireturn       
      LocalVariableTable:
        Start  Length  Slot  Name   Signature
               0       3     0  this   LFoo$;
      LineNumberTable:
        line 7: 0
```

### `:kind` ([#2340](https://github.com/scala/scala/pull/2340))

Because Scala supports working with higher kinded types, we might want to be able to inspects kinds as well as types:

```scala
scala> :kind Either
scala.util.Either's kind is F[+A1,+A2]

scala> :kind -v Either
scala.util.Either's kind is F[+A1,+A2]
* -(+)-> * -(+)-> *
This is a type constructor: a 1st-order-kinded type.

scala> :k -v Int
scala.Int's kind is A
*
This is a proper type.

scala> :k -v scala.Function1
scala.Function1's kind is F[-A1,+A2]
* -(-)-> * -(+)-> *
This is a type constructor: a 1st-order-kinded type.
```

For a more detailful explanation on the notation see the discussion of the PR.

### JSR-223 Scripting Engine support ([#2206](https://github.com/scala/scala/pull/2206))

It is now possible to use the REPL as a Scripting Engine:

```scala
scala> import javax.script.ScriptEngineManager
import javax.script.ScriptEngineManager

scala> val e = new ScriptEngineManager().getEngineByName("scala")
e: javax.script.ScriptEngine = scala.tools.nsc.interpreter.IMain@7debe95d

scala> e.put("n", 10)
n: Object = 10

scala> e.eval("1 to n.asInstanceOf[Int] foreach println")
1
2
3
4
5
6
7
8
9
10
res4: Object = null
```
