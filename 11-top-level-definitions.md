Top-Level Definitions
=====================

\section{Compilation Units}

\syntax\begin{lstlisting}
  CompilationUnit  ::=  {`package' QualId semi} TopStatSeq
  TopStatSeq       ::=  TopStat {semi TopStat}
  TopStat          ::=  {Annotation} {Modifier} TmplDef
                     |  Import
                     |  Packaging
                     |  PackageObject
                     |
  QualId           ::=  id {`.' id}
\end{lstlisting}

A compilation unit consists of a sequence of packagings, import
clauses, and class and object definitions, which may be preceded by a
package clause.

A compilation unit 
\begin{lstlisting}
package $p_1$;
$\ldots$
package $p_n$;
$\stats$
\end{lstlisting}
starting with one or more package
clauses is equivalent to a compilation unit consisting of the
packaging 
\begin{lstlisting}
package $p_1$ { $\ldots$
  package $p_n$ {
    $\stats$
  } $\ldots$
}
\end{lstlisting}

Implicitly imported into every compilation unit are, in that order :
the package \code{java.lang}, the package \code{scala}, and the object
\code{scala.Predef} (\sref{cls:predef}). Members of a later import in
that order hide members of an earlier import.

\section{Packagings}\label{sec:packagings}

\syntax\begin{lstlisting}
  Packaging       ::=  `package' QualId [nl] `{' TopStatSeq `}'
\end{lstlisting}

A package is a special object which defines a set of member classes,
objects and packages.  Unlike other objects, packages are not introduced
by a definition.  Instead, the set of members of a package is determined by
packagings.

A packaging ~\lstinline@package $p$ { $\ds$ }@~ injects all
definitions in $\ds$ as members into the package whose qualified name
is $p$. Members of a package are called {\em top-level} definitions.
If a definition in $\ds$ is labeled \code{private}, it is
visible only for other members in the package.

Inside the packaging, all members of package $p$ are visible under their
simple names. However this rule does not extend to members of enclosing
packages of $p$ that are designated by a prefix of the path $p$.

\example Given the packaging
\begin{lstlisting}
package org.net.prj {
  ...
}
\end{lstlisting}
all members of package \lstinline@org.net.prj@ are visible under their
simple names, but members of packages \code{org} or \code{org.net} require
explicit qualification or imports.

Selections $p$.$m$ from $p$ as well as imports from $p$
work as for objects. However, unlike other objects, packages may not
be used as values. It is illegal to have a package with the same fully
qualified name as a module or a class.

Top-level definitions outside a packaging are assumed to be injected
into a special empty package. That package cannot be named and
therefore cannot be imported. However, members of the empty package
are visible to each other without qualification.

\section{Package Objects}
\label{sec:pkg-obj}

\syntax\begin{lstlisting}
  PackageObject   ::=  `package' `object' ObjectDef
\end{lstlisting}

A package object ~\lstinline@package object $p$ extends $t$@~ adds the
members of template $t$ to the package $p$. There can be only one
package object per package. The standard naming convention is to place
the definition above in a file named \lstinline@package.scala@ that's
located in the directory corresponding to package $p$.

The package object should not define a member with the same name as
one of the top-level objects or classes defined in package $p$. If
there is a name conflict, the behavior of the program is currently
undefined. It is expected that this restriction will be lifted in a
future version of Scala.

\section{Package References}

\syntax\begin{lstlisting}
  QualId           ::=  id {`.' id}
\end{lstlisting}
A reference to a package takes the form of a qualified identifier.
Like all other references, package references are relative. That is, 
a package reference starting in a name $p$ will be looked up in the
closest enclosing scope that defines a member named $p$.

The special predefined name \lstinline@_root_@  refers to the
outermost root package which contains all top-level packages.  

\example\label{ex:package-ids}
Consider the following program:
\begin{lstlisting}
package b {
  class B 
}

package a.b {
  class A {
    val x = new _root_.b.B
  }
}
\end{lstlisting}  
Here, the reference \code{_root_.b.B} refers to class \code{B} in the
toplevel package \code{b}. If the \code{_root_} prefix had been
omitted, the name \code{b} would instead resolve to the package
\code{a.b}, and, provided that package does not also
contain a class \code{B}, a compiler-time error would result.

\section{Programs}

A {\em program} is a top-level object that has a member method
\code{main} of type ~\lstinline@(Array[String])Unit@. Programs can be
executed from a command shell. The program's command arguments are are
passed to the \code{main} method as a parameter of type
\code{Array[String]}.

The \code{main} method of a program can be directly defined in the
object, or it can be inherited. The scala library defines a class
\code{scala.Application} that defines an empty inherited \code{main} method.
An objects $m$ inheriting from this class is thus a program, 
which executes the initializaton code of the object $m$.

\example The following example will create a hello world program by defining
a method \code{main} in module \code{test.HelloWorld}.
\begin{lstlisting}
package test
object HelloWord {
  def main(args: Array[String]) { println("hello world") }
}
\end{lstlisting}

This program can be started by the command
\begin{lstlisting}
scala test.HelloWorld
\end{lstlisting}
In a Java environment, the command
\begin{lstlisting}
java test.HelloWorld
\end{lstlisting}
would work as well. 

\code{HelloWorld} can also be defined without a \code{main} method 
by inheriting from \code{Application} instead:
\begin{lstlisting}
package test 
object HelloWord extends Application {
  println("hello world")
}
\end{lstlisting}

