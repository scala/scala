Identifiers, Names and Scopes
=============================

Names in Scala identify types, values, methods, and classes which are
collectively called {\em entities}. Names are introduced by local
definitions and declarations (\sref{sec:defs}), inheritance (\sref{sec:members}),
import clauses (\sref{sec:import}), or package clauses
(\sref{sec:packagings}) which are collectively called {\em
bindings}.

Bindings of different kinds have a precedence defined on them:
\begin{enumerate} 
\item Definitions and declarations that are local, inherited, or made 
available by a package clause in the same compilation unit where the 
definition occurs have highest precedence. 
\item Explicit imports have next highest precedence.
\item Wildcard imports  have next highest precedence.
\item Definitions made available by a package clause not in the 
compilation unit where the definition occurs have lowest precedence.
\end{enumerate} 

There are two different name spaces, one for types (\sref{sec:types})
and one for terms (\sref{sec:exprs}).  The same name may designate a
type and a term, depending on the context where the name is used.

A binding has a {\em scope} in which the entity defined by a single
name can be accessed using a simple name. Scopes are nested.  A binding
in some inner scope {\em shadows} bindings of lower precedence in the
same scope as well as bindings of the same or lower precedence in outer
scopes. 

Note that shadowing is only a partial order. In a situation like
\begin{lstlisting}
val x = 1;
{ import p.x; 
  x }
\end{lstlisting}
neither binding of \code{x} shadows the other. Consequently, the
reference to \code{x} in the third line above would be ambiguous.

A reference to an unqualified (type- or term-) identifier $x$ is bound
by the unique binding, which
\begin{itemize}
\item defines an entity with name $x$ in the same namespace as the
identifier, and
\item shadows all other bindings that define entities with name $x$ in that namespace.
\end{itemize}
It is an error if no such binding exists.  If $x$ is bound by an
import clause, then the simple name $x$ is taken to be equivalent to
the qualified name to which $x$ is mapped by the import clause. If $x$
is bound by a definition or declaration, then $x$ refers to the entity
introduced by that binding. In that case, the type of $x$ is the type
of the referenced entity.

\example Assume the following two definitions of a objects named \lstinline@X@ in packages \lstinline@P@ and \lstinline@Q@.
%ref: shadowing.scala
\begin{lstlisting}
package P {
  object X { val x = 1; val y = 2 }
}
\end{lstlisting}
\begin{lstlisting}
package Q {
  object X { val x = true; val y = "" }
}
\end{lstlisting}
The following program illustrates different kinds of bindings and
precedences between them.
\begin{lstlisting}
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
          println("L20: "+x) // `x' refers to string ``abc'' here
}}}}}}
\end{lstlisting}

A reference to a qualified (type- or term-) identifier $e.x$ refers to
the member of the type $T$ of $e$ which has the name $x$ in the same
namespace as the identifier. It is an error if $T$ is not a value type
(\sref{sec:value-types}). The type of $e.x$ is the member type of the
referenced entity in $T$.

