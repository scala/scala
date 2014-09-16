---
title: Identifiers, Names & Scopes
layout: default
chapter: 2
---

# Identifiers, Names and Scopes

Names in Scala identify types, values, methods, and classes which are
collectively called _entities_. Names are introduced by local
[definitions and declarations](04-basic-declarations-and-definitions.html#basic-declarations-and-definitions),
[inheritance](05-classes-and-objects.html#class-members),
[import clauses](04-basic-declarations-and-definitions.html#import-clauses), or
[package clauses](09-top-level-definitions.html#packagings)
which are collectively called _bindings_.

Bindings of different kinds have a precedence defined on them:

1. Definitions and declarations that are local, inherited, or made
   available by a package clause in the same compilation unit where the
   definition occurs have highest precedence.
1. Explicit imports have next highest precedence.
1. Wildcard imports  have next highest precedence.
1. Definitions made available by a package clause not in the
   compilation unit where the definition occurs have lowest precedence.

There are two different name spaces, one for [types](03-types.html#types)
and one for [terms](06-expressions.html#expressions). The same name may designate a
type and a term, depending on the context where the name is used.

A binding has a _scope_ in which the entity defined by a single
name can be accessed using a simple name. Scopes are nested.  A binding
in some inner scope _shadows_ bindings of lower precedence in the
same scope as well as bindings of the same or lower precedence in outer
scopes.

<!-- TODO: either the example, the spec, or the compiler is wrong

Note that shadowing is only a partial order. In a situation like

```scala
val x = 1
{
  import p.x
  x
}
```

neither binding of `x` shadows the other. Consequently, the
reference to `x` in the last line of the block above would be ambiguous.
-->

A reference to an unqualified (type- or term-) identifier $x$ is bound
by the unique binding, which

- defines an entity with name $x$ in the same namespace as the identifier, and
- shadows all other bindings that define entities with name $x$ in that
  namespace.

It is an error if no such binding exists.  If $x$ is bound by an
import clause, then the simple name $x$ is taken to be equivalent to
the qualified name to which $x$ is mapped by the import clause. If $x$
is bound by a definition or declaration, then $x$ refers to the entity
introduced by that binding. In that case, the type of $x$ is the type
of the referenced entity.

A reference to a qualified (type- or term-) identifier $e.x$ refers to
the member of the type $T$ of $e$ which has the name $x$ in the same
namespace as the identifier. It is an error if $T$ is not a [value type](03-types.html#value-types).
The type of $e.x$ is the member type of the referenced entity in $T$.

###### Example

Assume the following two definitions of objects named `X` in packages `P` and `Q`.

```scala
package P {
  object X { val x = 1; val y = 2 }
}

package Q {
  object X { val x = true; val y = "" }
}
```

The following program illustrates different kinds of bindings and
precedences between them.

```scala
package P {                  // `X' bound by package clause
import Console._             // `println' bound by wildcard import
object A {
  println("L4: "+X)          // `X' refers to `P.X' here
  object B {
    import Q._               // `X' bound by wildcard import
    println("L7: "+X)        // `X' refers to `Q.X' here
    import X._               // `x' and `y' bound by wildcard import
    println("L8: "+x)        // `x' refers to `Q.X.x' here
    object C {
      val x = 3              // `x' bound by local definition
      println("L12: "+x)     // `x' refers to constant `3' here
      { import Q.X._         // `x' and `y' bound by wildcard import
//      println("L14: "+x)   // reference to `x' is ambiguous here
        import X.y           // `y' bound by explicit import
        println("L16: "+y)   // `y' refers to `Q.X.y' here
        { val x = "abc"      // `x' bound by local definition
          import P.X._       // `x' and `y' bound by wildcard import
//        println("L19: "+y) // reference to `y' is ambiguous here
          println("L20: "+x) // `x' refers to string "abc" here
}}}}}}
```
