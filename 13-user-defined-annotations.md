User-Defined Annotations
========================

\syntax\begin{lstlisting}
  Annotation       ::=  `@' SimpleType {ArgumentExprs}
  ConstrAnnotation ::=  `@' SimpleType ArgumentExprs
\end{lstlisting}

User-defined annotations associate meta-information with definitions.
A simple annotation has the form \lstinline^@$c$^ or
\lstinline^@$c(a_1 \commadots a_n)$^.  
Here, $c$ is a constructor of a class $C$, which must conform
to the class \lstinline@scala.Annotation@. 

Annotations may apply to definitions or declarations, types, or
expressions.  An annotation of a definition or declaration appears in
front of that definition.  An annotation of a type appears after
that type. An annotation of an expression $e$ appears after the
expression $e$, separated by a colon. More than one annotation clause
may apply to an entity. The order in which these annotations are given
does not matter.

Examples:
\begin{lstlisting}
@serializable class C { ... }         // A class annotation.
@transient @volatile var m: Int       // A variable annotation
String @local                         // A type annotation
(e: @unchecked) match { ... }         // An expression annotation
\end{lstlisting}

The meaning of annotation clauses is implementation-dependent. On the
Java platform, the following annotations have a standard meaning.\bigskip

\lstinline^@transient^
\begin{quote}
Marks a field to be non-persistent; this is
equivalent to the \lstinline^transient^
modifier in Java.
\end{quote}

\lstinline^@volatile^
\begin{quote}Marks a field which can change its value
outside the control of the program; this
is equivalent to the \lstinline^volatile^
modifier in Java.
\end{quote}

\lstinline^@serializable^
\begin{quote}Marks a class to be serializable; this is
equivalent to inheriting from the 
\lstinline^java.io.Serializable^ interface
in Java.
\end{quote}

\lstinline^@SerialVersionUID(<longlit>)^
\begin{quote}Attaches a serial version identifier (a
\lstinline^long^ constant) to a class.
This is equivalent to a the following field
definition in Java:
\begin{lstlisting}[language=Java]
  private final static SerialVersionUID = <longlit> 
\end{lstlisting}
\end{quote}

\lstinline^@throws(<classlit>)^
\begin{quote}
A Java compiler checks that a program contains handlers for checked exceptions
by analyzing which checked exceptions can result from execution of a method or
constructor. For each checked exception which is a possible result, the \code{throws}
clause for the method or constructor must mention the class of that exception
or one of the superclasses of the class of that exception.
\end{quote}

\lstinline^@deprecated(<stringlit>)^
\begin{quote} Marks a definition as deprecated. Accesses to the
  defined entity will then cause a deprecated warning mentioning the
  message \code{<stringlit>} to be issued from the compiler.  Deprecated
  warnings are suppressed in code that belongs itself to a definition
  that is labeled deprecated.
\end{quote}

\lstinline^@scala.reflect.BeanProperty^
\begin{quote}
When prefixed to a definition of some variable \code{X}, this
annotation causes getter and setter methods \code{getX}, \code{setX}
in the Java bean style to be added in the class containing the
variable. The first letter of the variable appears capitalized after
the \code{get} or \code{set}. When the annotation is added to the
definition of an immutable value definition \code{X}, only a getter is
generated. The construction of these methods is part of
code-generation; therefore, these methods become visible only once a
classfile for the containing class is generated.
\end{quote}

\lstinline^@scala.reflect.BooleanBeanProperty^
\begin{quote}
This annotation is equivalent to \code{scala.reflect.BeanProperty}, but
the generated getter method is named \code{isX} instead of \code{getX}.
\end{quote}

\lstinline^@unchecked^
\begin{quote}
When applied to the selector of a \lstinline@match@ expression,
this attribute suppresses any warnings about non-exhaustive pattern
matches which would otherwise be emitted. For instance, no warnings
would be produced for the method definition below.
\begin{lstlisting}
def f(x: Option[Int]) = (x: @unchecked) match {
  case Some(y) => y
}
\end{lstlisting}
Without the \lstinline^@unchecked^ annotation, a Scala compiler could
infer that the pattern match is non-exhaustive, and could produce a
warning because \lstinline@Option@ is a \lstinline@sealed@ class.
\end{quote}

\lstinline^@uncheckedStable^
\begin{quote}
When applied a value declaration or definition, it allows the defined
value to appear in a path, even if its type is volatile (\sref{volatile-types}).
For instance, the following member definitions are legal:
\begin{lstlisting}
type A { type T }
type B 
@uncheckedStable val x: A with B // volatile type 
val y: x.T                       // OK since `x' is still a path
\end{lstlisting}
Without the \lstinline^@uncheckedStable^ annotation, the designator \code{x}
would not be a path since its type \code{A with B} is volatile. Hence,
the reference \code{x.T} would be malformed. 

When applied to value declarations or definitions that have non-volatile types, 
the annotation has no effect. 
\end{quote}

\lstinline^@specialized^
\begin{quote}
When applied to the definition of a type parameter, this annotation causes the compiler
to generate specialized definitions for primitive types. An optional list of primitive
types may be given, in which case specialization takes into account only those types.
For instance, the following code would generate specialized traits for \lstinline@Unit@, 
\lstinline@Int@ and \lstinline@Double@
\begin{lstlisting}
trait Function0[@specialized(Unit, Int, Double) T] {
  def apply: T
}
\end{lstlisting}
Whenever the static type of an expression matches a specialized variant of a definition,
the compiler will instead use the specialized version. See \cite{spec-sid} for more details
of the implementation.
\end{quote}


Other annotations may be interpreted by platform- or
application-dependent tools. Class \code{scala.Annotation} has two
sub-traits which are used to indicate how these annotations are
retained. Instances of an annotation class inheriting from trait
\code{scala.ClassfileAnnotation} will be stored in the generated class
files. Instances of an annotation class inheriting from trait
\code{scala.StaticAnnotation} will be visible to the Scala type-checker
in every compilation unit where the annotated symbol is accessed. An
annotation class can inherit from both \code{scala.ClassfileAnnotation}
and \code{scala.StaticAnnotation}. If an annotation class inherits from
neither \code{scala.ClassfileAnnotation} nor
\code{scala.StaticAnnotation}, its instances are visible only locally
during the compilation run that analyzes them.

Classes inheriting from \code{scala.ClassfileAnnotation} may be
subject to further restrictions in order to assure that they can be
mapped to the host environment. In particular, on both the Java and
the .NET platforms, such classes must be toplevel; i.e.\ they may not
be contained in another class or object.  Additionally, on both
Java and .NET, all constructor arguments must be constant expressions.

