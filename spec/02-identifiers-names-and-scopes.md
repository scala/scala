---
title: Identifiers, Names & Scopes
layout: default
chapter: 2
---

# Identifiers, Names and Scopes

Names in Scala identify types, values, methods, and classes which are
collectively called _entities_. Names are introduced by
[definitions and declarations](04-basic-declarations-and-definitions.html#basic-declarations-and-definitions),
[inheritance](05-classes-and-objects.html#class-members),
[import clauses](04-basic-declarations-and-definitions.html#import-clauses), or
[package clauses](09-top-level-definitions.html#packagings)
which are collectively called _bindings_.

Bindings of each kind are assigned a precedence which determines
whether one binding can shadow another:

<!-- Not in the spec since Scala 2 only warns (scala/scala#10339)
1. Definitions and declarations that are local, or made available by a package clause and also
   defined in the same compilation unit as the reference to them, have the highest precedence.
1. Definitions and declarations that are inherited have the next highest precedence.
-->
1. Definitions and declarations that are local, inherited, or made
   available by a package clause and also defined in the same compilation unit
   as the reference to them, have the highest precedence.
1. Explicit imports have the next highest precedence.
1. Wildcard imports have the next highest precedence.
1. Bindings made available by a package clause,
   but not also defined in the same compilation unit as the reference to them,
   as well as bindings supplied by the compiler but not explicitly written in source code,
   have the lowest precedence.

There are two different name spaces, one for [types](03-types.html#types)
and one for [terms](06-expressions.html#expressions). The same name may designate a
type and a term, depending on the context where the name is used.

A binding has a _scope_ in which the entity defined by a single
name can be accessed using a simple name. Scopes are nested.  A binding
in some inner scope _shadows_ bindings of lower precedence in the
same scope as well as bindings of the same or lower precedence in outer
scopes.

Note that shadowing is only a partial order. In the following example,
neither binding of `x` shadows the other. Consequently, the
reference to `x` in the last line of the block is ambiguous.

```scala
val x = 1
locally {
  import p.X.x
  x
}
```

A reference to an unqualified (type- or term-) identifier ´x´ is bound
by the unique binding, which

- defines an entity with name ´x´ in the same namespace as the identifier, and
- shadows all other bindings that define entities with name ´x´ in that
  namespace.

It is an error if no such binding exists.  If ´x´ is bound by an
import clause, then the simple name ´x´ is taken to be equivalent to
the qualified name to which ´x´ is mapped by the import clause. If ´x´
is bound by a definition or declaration, then ´x´ refers to the entity
introduced by that binding. In that case, the type of ´x´ is the type
of the referenced entity.

A reference to a qualified (type- or term-) identifier ´e.x´ refers to
the member of the type ´T´ of ´e´ which has the name ´x´ in the same
namespace as the identifier. It is an error if ´T´ is not a [value type](03-types.html#value-types).
The type of ´e.x´ is the member type of the referenced entity in ´T´.

Binding precedence implies that the way source is bundled in files affects name resolution.
In particular, imported names have higher precedence than names, defined in other files,
that might otherwise be visible because they are defined in
either the current package or an enclosing package.

Note that a binding introduced by a packaging is taken as lowest precedence,
since packages are open and can be defined across arbitrary compilation units.

```scala
package util {
  import scala.util
  class Random
  object Test extends App {
    println(new util.Random)  // scala.util.Random
  }
}
```

The compiler supplies bindings from well-known packages and objects, called "root contexts".
The standard locations for these bindings are:

1. The object `scala.Predef`.
1. The package `scala`.
1. The package `java.lang`.

These bindings are taken as lowest precedence, so that they are always shadowed
by user code, which may contain competing imports and definitions.

A binding is available from a root context if it would also be available
using an ordinary import clause. In particular, ordinary access restrictions apply.

A binding from an earlier root context shadows a binding of the same name from a later one.
For example, `scala.Predef.String` shadows `java.lang.String`, for which it is a type alias.

Multiple binding of a type identifier to the same underlying type is permitted.
This is possible when import clauses introduce a binding of a member type alias
with the same binding precedence, typically through wildcard imports.
This allows redundant type aliases to be imported without introducing an ambiguity.

```scala
object X { type T = annotation.tailrec }
object Y { type T = annotation.tailrec }
object Z {
  import X._, Y._             // OK, both T mean tailrec
  @T def f: Int = { f ; 42 }  // the annotation worked: error, f is not tail recursive
}
```

Similarly, imported aliases of names introduced by package statements are permitted:

```scala
// c.scala
package p { class C }

// xy.scala
import p._
package p { class X extends C } // not ambiguous (compiles without the import)
package q { class Y extends C } // requires the import
```
###### Example

Assume the following two definitions of objects named `X` in packages `p` and `q`
in separate compilation units.

```scala
package p {
  object X { val x = 1; val y = 2 }
}

package q {
  object X { val x = true; val y = false }
}
```

The following program illustrates different kinds of bindings and
precedences between them.

```scala
package p {                   // `X' bound by package clause
import Console._              // `println' bound by wildcard import
object Y {
  println(s"L4: $X")          // `X' refers to `p.X' here
  locally {
    import q._                // `X' bound by wildcard import
    println(s"L7: $X")        // `X' refers to `q.X' here
    import X._                // `x' and `y' bound by wildcard import
    println(s"L9: $x")        // `x' refers to `q.X.x' here
    locally {
      val x = 3               // `x' bound by local definition
      println(s"L12: $x")     // `x' refers to constant `3' here
      locally {
        import q.X._          // `x' and `y' bound by wildcard import
//      println(s"L15: $x")   // reference to `x' is ambiguous here
        import X.y            // `y' bound by explicit import
        println(s"L17: $y")   // `y' refers to `q.X.y' here
        locally {
          val x = "abc"       // `x' bound by local definition
          import p.X._        // `x' and `y' bound by wildcard import
//        println(s"L21: $y") // reference to `y' is ambiguous here
          println(s"L22: $x") // `x' refers to string "abc" here
}}}}}}
```
