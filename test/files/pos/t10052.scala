
package foo { trait identifier }
package foo { package identifier.foo { } }
object Test13 { def identifier = 2 }

package bar { trait identifier }
package bar { package identifier.bar { class X } }
object Test14 { def identifier = 2 }

/* Was:
error: Error while emitting t10012.scala
assertion failed:
  Java member module without member class: package foo - List(package foo)
     while compiling: t10012.scala
        during phase: jvm
     library version: version 2.13.1
    compiler version: version 2.13.1
  reconstructed args: -d /tmp

  last tree to typer: TypeTree(class Int)
       tree position: line 12 of t10012.scala
            tree tpe: Int
              symbol: (final abstract) class Int in package scala
   symbol definition: final abstract class Int extends  (a ClassSymbol)
      symbol package: scala
       symbol owners: class Int
           call site: constructor Test13 in object Test13 in package <empty>

== Source file context for tree position ==

     9 //object Test9 { trait identifier }
    10 //package identifier { package identifier.bar {} }
    11 package foo { package identifier.foo {} }
    12 object Test13 { def identifier = 2 }
    13
 */
