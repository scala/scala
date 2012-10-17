Pattern Matching
================

\section{Patterns}

\label{sec:patterns}

\syntax\begin{lstlisting}
  Pattern         ::=  Pattern1 { `|' Pattern1 }
  Pattern1        ::=  varid `:' TypePat
                    |  `_' `:' TypePat
                    |  Pattern2
  Pattern2        ::=  varid [`@' Pattern3]
                    |  Pattern3
  Pattern3        ::=  SimplePattern 
                    |  SimplePattern {id [nl] SimplePattern}
  SimplePattern   ::=  `_'
                    |  varid
                    |  Literal
                    |  StableId
                    |  StableId `(' [Patterns] `)'
                    |  StableId `(' [Patterns `,'] [varid `@'] `_' `*' `)'
                    |  `(' [Patterns] `)'
                    |  XmlPattern
  Patterns        ::=  Pattern {`,' Patterns}
\end{lstlisting}

%For clarity, this section deals with a subset of the Scala pattern language.
%The extended Scala pattern language, which is described below, adds more
%flexible variable binding and regular hedge expressions.

A pattern is built from constants, constructors, variables and type
tests. Pattern matching tests whether a given value (or sequence of values)
has the shape defined by a pattern, and, if it does, binds the
variables in the pattern to the corresponding components of the value
(or sequence of values).  The same variable name may not be bound more
than once in a pattern.

\example Some examples of patterns are:
\begin{enumerate}
\item
The pattern ~\lstinline@ex: IOException@ matches all instances of class
\lstinline@IOException@, binding variable \verb@ex@ to the instance.
\item
The pattern ~\lstinline@Some(x)@~ matches values of the form ~\lstinline@Some($v$)@,
binding \lstinline@x@ to the argument value $v$ of the \code{Some} constructor.
\item
The pattern ~\lstinline@(x, _)@~ matches pairs of values, binding \lstinline@x@ to
the first component of the pair. The second component is matched
with a wildcard pattern.
\item
The pattern ~\lstinline@x :: y :: xs@~ matches lists of length $\geq 2$,
binding \lstinline@x@ to the list's first element, \lstinline@y@ to the list's
second element, and \lstinline@xs@ to the remainder.
\item
The pattern ~\lstinline@1 | 2 | 3@~ matches the integers between 1 and 3.
\end{enumerate}

Pattern matching is always done in a context which supplies an
expected type of the pattern. We distinguish the following kinds of
patterns.

\subsection{Variable Patterns}

\syntax\begin{lstlisting}
  SimplePattern   ::=  `_'
                    |  varid
\end{lstlisting}

A variable pattern $x$ is a simple identifier which starts with a
lower case letter.  It matches any value, and binds the variable name
to that value.  The type of $x$ is the expected type of the pattern as
given from outside.  A special case is the wild-card pattern $\_$
which is treated as if it was a fresh variable on each occurrence.

\subsection{Typed Patterns}
\label{sec:typed-patterns}
\syntax
\begin{lstlisting}
  Pattern1        ::=  varid `:' TypePat
                    |  `_' `:' TypePat
\end{lstlisting}

A typed pattern $x: T$ consists of a pattern variable $x$ and a
type pattern $T$.  The type of $x$ is the type pattern $T$, where 
each type variable and wildcard is replaced by a fresh, unknown type.
This pattern matches any value matched by the type
pattern $T$ (\sref{sec:type-patterns}); it binds the variable name to
that value.  

\subsection{Pattern Binders}
\label{sec:pattern-binders}
\syntax
\begin{lstlisting}
  Pattern2        ::=  varid `@' Pattern3
\end{lstlisting}
A pattern binder \lstinline|$x$@$p$| consists of a pattern variable $x$ and a 
pattern $p$. The type of the variable $x$ is the static type $T$ of the pattern $p$.
This pattern matches any value $v$ matched by the pattern $p$, 
provided the run-time type of $v$ is also an instance of $T$, 
and it binds the variable name to that value.

\subsection{Literal Patterns}

\syntax\begin{lstlisting}
  SimplePattern   ::=  Literal
\end{lstlisting}

A literal pattern $L$ matches any value that is equal (in terms of
$==$) to the literal $L$. The type of $L$ must conform to the
expected type of the pattern.

\subsection{Stable Identifier Patterns} 

\syntax
\begin{lstlisting}
  SimplePattern   ::=  StableId
\end{lstlisting}

A stable identifier pattern is a stable identifier $r$
(\sref{sec:stable-ids}). The type of $r$ must conform to the expected
type of the pattern. The pattern matches any value $v$ such that
~\lstinline@$r$ == $v$@~ (\sref{sec:cls-object}).

To resolve the syntactic overlap with a variable pattern, a
stable identifier pattern may not be a simple name starting with a lower-case
letter. However, it is possible to enclose a such a variable name in
backquotes; then it is treated as a stable identifier pattern.

\example Consider the following function definition:
\begin{lstlisting}
def f(x: Int, y: Int) = x match {
  case y => ...
}
\end{lstlisting}
Here, \lstinline@y@ is a variable pattern, which matches any value.
If we wanted to turn the pattern into a stable identifier pattern, this
can be achieved as follows:
\begin{lstlisting}
def f(x: Int, y: Int) = x match {
  case `y` => ...
}
\end{lstlisting}
Now, the pattern matches the \code{y} parameter of the enclosing function \code{f}.
That is, the match succeeds only if the \code{x} argument and the \code{y}
argument of \code{f} are equal.

\subsection{Constructor Patterns}

\syntax\begin{lstlisting}
  SimplePattern   ::=  StableId `(' [Patterns] `)
\end{lstlisting}

A constructor pattern is of the form $c(p_1 \commadots p_n)$ where $n
\geq 0$. It consists of a stable identifier $c$, followed by element
patterns $p_1 \commadots p_n$. The constructor $c$ is a simple or
qualified name which denotes a case class
(\sref{sec:case-classes}). If the case class is monomorphic, then it
must conform to the expected type of the pattern, and the formal
parameter types of $x$'s primary constructor (\sref{sec:class-defs})
are taken as the expected types of the element patterns $p_1\commadots
p_n$.  If the case class is polymorphic, then its type parameters are
instantiated so that the instantiation of $c$ conforms to the expected
type of the pattern. The instantiated formal parameter types of $c$'s
primary constructor are then taken as the expected types of the
component patterns $p_1\commadots p_n$.  The pattern matches all
objects created from constructor invocations $c(v_1 \commadots v_n)$
where each element pattern $p_i$ matches the corresponding value
$v_i$.

A special case arises when $c$'s formal parameter types end in a
repeated parameter. This is further discussed in
(\sref{sec:pattern-seqs}).

\subsection{Tuple Patterns}

\syntax\begin{lstlisting}
  SimplePattern   ::=  `(' [Patterns] `)'
\end{lstlisting}

A tuple pattern \lstinline@($p_1 \commadots p_n$)@ is an alias
for the constructor pattern ~\lstinline@scala.Tuple$n$($p_1 \commadots
p_n$)@, where $n \geq 2$. The empty tuple
\lstinline@()@ is the unique value of type \lstinline@scala.Unit@.

\subsection{Extractor Patterns}\label{sec:extractor-patterns}

\syntax\begin{lstlisting}
  SimplePattern   ::=  StableId `(' [Patterns] `)'
\end{lstlisting}

An extractor pattern $x(p_1 \commadots p_n)$ where $n \geq 0$ is of
the same syntactic form as a constructor pattern. However, instead of
a case class, the stable identifier $x$ denotes an object which has a
member method named \code{unapply} or \code{unapplySeq} that matches
the pattern.

An \code{unapply} method in an object $x$ {\em matches} the pattern
$x(p_1 \commadots p_n)$ if it takes exactly one argument and one of
the following applies:
\begin{itemize}
\item[]
$n=0$ and \code{unapply}'s result type is \code{Boolean}. In this case
the extractor pattern matches all values $v$ for which 
\lstinline@$x$.unapply($v$)@ yields \code{true}.
\item[]
$n=1$ and \code{unapply}'s result type is \lstinline@Option[$T$]@, for some
type $T$.  In this case, the (only) argument pattern $p_1$ is typed in
turn with expected type $T$.  The extractor pattern matches then all
values $v$ for which \lstinline@$x$.unapply($v$)@ yields a value of form
\lstinline@Some($v_1$)@, and $p_1$ matches $v_1$.
\item[]
$n>1$ and \code{unapply}'s result type is 
\lstinline@Option[($T_1 \commadots T_n$)]@, for some
types $T_1 \commadots T_n$.  In this case, the argument patterns $p_1
\commadots p_n$ are typed in turn with expected types $T_1 \commadots
T_n$.  The extractor pattern matches then all values $v$ for which
\lstinline@$x$.unapply($v$)@ yields a value of form
\lstinline@Some(($v_1 \commadots v_n$))@, and each pattern
$p_i$ matches the corresponding value $v_i$.
\end{itemize}

An \code{unapplySeq} method in an object $x$ matches the pattern
$x(p_1 \commadots p_n)$ if it takes exactly one argument and its
result type is of the form \lstinline@Option[$S$]@, where $S$ is a subtype of
\lstinline@Seq[$T$]@ for some element type $T$. 
This case is further discussed in (\sref{sec:pattern-seqs}).

\example  The \code{Predef} object contains a definition of an
extractor object \code{Pair}:
\begin{lstlisting}
object Pair {
  def apply[A, B](x: A, y: B) = Tuple2(x, y)
  def unapply[A, B](x: Tuple2[A, B]): Option[Tuple2[A, B]] = Some(x)
}
\end{lstlisting}
This means that the name \code{Pair} can be used in place of \code{Tuple2} for tuple 
formation as well as for deconstruction of tuples in patterns.
Hence, the following is possible:
\begin{lstlisting}
val x = (1, 2)
val y = x match {
  case Pair(i, s) => Pair(s + i, i * i)
}
\end{lstlisting}

\subsection{Pattern Sequences}\label{sec:pattern-seqs}

\syntax\begin{lstlisting}
  SimplePattern ::= StableId `(' [Patterns `,'] [varid `@'] `_' `*' `)'
\end{lstlisting}

A pattern sequence $p_1 \commadots p_n$ appears in two
contexts. First, in a constructor pattern
$c(q_1 \commadots q_m, p_1 \commadots p_n$), where $c$ is a case
class which has $m+1$ primary constructor parameters, 
ending in a repeated parameter (\sref{sec:repeated-params}) of type 
$S*$. Second, in an extractor pattern
$x(p_1 \commadots p_n)$ if the extractor object $x$ has an
\code{unapplySeq} method with a result type conforming to 
\lstinline@Seq[$S$]@, but does not have an \code{unapply} method that 
matches $p_1 \commadots p_n$.
The expected type for the pattern sequence is in each case the type $S$.

The last pattern in a pattern sequence may be a {\em sequence
wildcard} \code{_*}. Each element pattern $p_i$ is type-checked with
$S$ as expected type, unless it is a sequence wildcard. If a final
sequence wildcard is present, the pattern matches all values $v$ that
are sequences which start with elements matching patterns
$p_1 \commadots p_{n-1}$.  If no final sequence wildcard is given, the
pattern matches all values $v$ that are sequences of
length $n$ which consist of elements matching patterns $p_1 \commadots
p_n$.

\subsection{Infix Operation Patterns}

\syntax\begin{lstlisting}
  Pattern3  ::=  SimplePattern {id [nl] SimplePattern}
\end{lstlisting}

An infix operation pattern $p;\op;q$ is a shorthand for the
constructor or extractor pattern $\op(p, q)$.  The precedence and
associativity of operators in patterns is the same as in expressions
(\sref{sec:infix-operations}).

An infix operation pattern $p;\op;(q_1 \commadots q_n)$ is a
shorthand for the constructor or extractor pattern $\op(p, q_1
\commadots q_n)$.

\subsection{Pattern Alternatives}

\syntax\begin{lstlisting}
  Pattern   ::=  Pattern1 { `|' Pattern1 }
\end{lstlisting}

A pattern alternative ~\lstinline@$p_1$ | $\ldots$ | $p_n$@~
consists of a number of alternative patterns $p_i$. All alternative
patterns are type checked with the expected type of the pattern. They
may no bind variables other than wildcards. The alternative pattern 
matches a value $v$ if at least one its alternatives matches $v$.

\subsection{XML Patterns}

XML patterns are treated in \sref{sec:xml-pats}.

\subsection{Regular Expression Patterns}\label{sec:reg-pats}

Regular expression patterns have been discontinued in Scala from version 2.0.

Later version of Scala provide a much simplified version of regular
expression patterns that cover most scenarios of non-text sequence
processing.  A {\em sequence pattern} is a pattern that stands in a
position where either (1) a pattern of a type \lstinline+T+ which is
conforming to
\lstinline+Seq[A]+ for some \lstinline+A+ is expected, or (2) a case
class constructor that has an iterated formal parameter
\lstinline+A*+.  A wildcard star pattern \lstinline+_*+ in the
rightmost position stands for arbitrary long sequences. It can be
bound to variables using \lstinline+@+, as usual, in which case the variable will have the
type \lstinline+Seq[A]+.

\comment{
\syntax\begin{lstlisting}
  Pattern         ::=  Pattern1 { `|' Pattern1 }
  Pattern1        ::=  varid `:' Type
                    |  `_' `:' Type
                    |  Pattern2
  Pattern2        ::=  [varid `@'] Pattern3
  Pattern3        ::=  SimplePattern [ `*' | `?' | `+' ]
                    |  SimplePattern { id' SimplePattern }
  SimplePattern   ::=  `_'
                    |  varid
                    |  Literal
                    |  `null'
                    |  StableId [ `(' [Patterns] `)' ]
                    |  `(' [Patterns] `)'
  Patterns        ::=  Pattern {`,' Pattern}
  id'             ::=  id $\textit{ but not }$ '*' | '?' | '+' | `@' | `|'
\end{lstlisting}

We distinguish between tree patterns and hedge patterns (hedges 
are ordered sequences of trees). A {\em tree pattern} describes 
a set of matching trees (like above). A {\em hedge pattern} describes 
a set of matching hedges. Both kinds of patterns may contain {\em 
variable bindings} which serve to extract constituents of a tree or hedge.

The type of a patterns and the expected types of variables 
within patterns are determined by the context and the structure of the
patterns. The last case ensures that a variable bound 
to a hedge pattern will have a sequence type.

The following patterns are added:

A {\em hedge pattern} $p_1 \commadots p_n$ where $n \geq 0$ is a
sequence of patterns separated by commas and matching the hedge described
by the components. Hedge patterns may appear as arguments to constructor 
applications, or nested within another hedge pattern if grouped with
parentheses. Note that empty hedge patterns are allowed. The type of tree 
patterns that appear in a hedge pattern is the expected type as 
determined from the enclosing constructor.
A {\em fixed-length argument pattern} is a special hedge pattern where 
where all $p_i$ are tree patterns.

A {\em choice pattern} $p_1 | \ldots | p_n$ is a choice among several
alternatives, which may not contain variable-binding patterns. It
matches every tree and every hedge matched by at least one of its 
alternatives.  Note that the empty sequence may appear as an alternative.  
An {\em option pattern} $p?$ is an abbreviation for $(p| )$. A choice is 
a tree pattern if all its branches are tree patterns. In this case, all 
branches must conform to the expected type and the type 
of the choice is the least upper bound of the branches. Otherwise, 
its type is determined by the enclosing hedge pattern it is part of.

An {\em iterated pattern} $p*$ matches zero, one or more occurrences 
of items matched by $p$, where $p$ may be either a tree pattern or a hedge pattern. $p$ may not 
contain a variable-binding. A {\em non-empty iterated pattern} $p+$ is an 
abbreviation for $(p,p*)$. 

The treatment of the following patterns changes with to the 
previous section:

A {\em constructor pattern} $c(p)$ consists of a simple type $c$
followed by a pattern $p$.  If $c$ designates a monomorphic case
class, then it must conform to the expected type of the pattern, the
pattern must be a fixed length argument pattern $p_1 \commadots p_n$
whose length corresponds to the number of arguments of $c$'s primary
constructor. The expected types of the component patterns are then
taken from the formal parameter types of (said) constructor.  If $c$
designates a polymorphic case class, then there must be a unique type
application instance of it such that the instantiation of $c$ conforms
to the expected type of the pattern. The instantiated formal parameter
types of $c$'s primary constructor are then taken as the expected
types of the component patterns $p_1\commadots p_n$.  In both cases,
the pattern matches all objects created from constructor invocations
$c(v_1 \commadots v_n)$ where each component pattern $p_i$ matches the
corresponding value $v_i$. If $c$ does not designate a case class, it
must be a subclass of \lstinline@Seq[$T\,$]@. In that case $p$ may be an
arbitrary sequence pattern. Value patterns in $p$ are expected to conform to
type $T$, and the pattern matches all objects whose \lstinline@elements()@
method returns a sequence that matches $p$.

The pattern $(p)$ is regarded as equivalent to the pattern $p$, if $p$
is a nonempty sequence pattern. The empty tuple $()$ is a shorthand
for the constructor pattern \code{Unit}.

A {\em variable-binding} $x @ p$ is a simple identifier $x$
which starts with a lower case letter, together with a pattern $p$. It
matches every item (tree or hedge) matched by $p$, and in addition binds 
it to the variable name. If $p$ is a tree pattern of type $T$, the type 
of $x$ is also $T$.
If $p$ is a hedge pattern enclosed by constructor $c <: $\lstinline@Seq[$T\,$]@,
then the type of $x$ is \lstinline@List[$T\,$]@
where $T$ is the expected type as dictated by the constructor.

%A pattern consisting of only a variable $x$ is treated as the bound
%value pattern $x @ \_$. A typed pattern $x:T$ is treated as $x @ (_:T)$.
%
Regular expressions that contain variable bindings may be ambiguous,
i.e.\ there might be several ways to match a sequence against the
pattern. In these cases, the \emph{right-longest policy} applies:
patterns that appear more to the right than others in a sequence take
precedence in case of overlaps.

\example Some examples of patterns are:
\begin{enumerate}
\item
The pattern ~\lstinline@ex: IOException@~ matches all instances of class
\code{IOException}, binding variable \code{ex} to the instance.
\item
The pattern ~\lstinline@Pair(x, _)@~ matches pairs of values, binding \code{x} to
the first component of the pair. The second component is matched
with a wildcard pattern.
\item
The pattern \ \code{List( x, y, xs @ _ * )} matches lists of length $\geq 2$,
binding \code{x} to the list's first element, \code{y} to the list's
second element, and \code{xs} to the remainder, which may be empty.
\item
The pattern \ \code{List( 1, x@(( 'a' | 'b' )+),y,_ )} matches a list that
contains 1 as its first element, continues with a non-empty sequence of 
\code{'a'}s and \code{'b'}s, followed by two more elements. The sequence of 'a's and 'b's
is bound to \code{x}, and the next to last element is bound to \code{y}.
\item
The pattern \code{List( x@( 'a'* ), 'a'+ )} matches a non-empty list of
\code{'a'}s. Because of the shortest match policy, \code{x} will always be bound to
the empty sequence.
\item
The pattern \code{List( x@( 'a'+ ), 'a'* )} also matches a non-empty list of
\code{'a'}s. Here, \code{x} will always be bound to
the sequence containing one \code{'a'}.
\end{enumerate}

}

\subsection{Irrefutable Patterns}

A pattern $p$ is {\em irrefutable} for a type $T$, if one of the following applies:
\begin{enumerate}
\item $p$ is a variable pattern,
\item $p$ is a typed pattern $x: T'$, and $T <: T'$,
\item $p$ is a constructor pattern $c(p_1 \commadots p_n)$, the type $T$
      is an instance of class $c$, the primary constructor
      (\sref{sec:class-defs}) of type $T$ has
      argument types $T_1 \commadots T_n$, and each $p_i$ is irrefutable for $T_i$.
\end{enumerate}

%%% new patterns

\section{Type Patterns}\label{sec:type-patterns}

\syntax\begin{lstlisting}
  TypePat           ::=  Type
\end{lstlisting}
Type patterns consist of types, type variables, and wildcards. 
A type pattern $T$ is of one of the following  forms:
\begin{itemize}
\item A reference to a class $C$, $p.C$, or \lstinline@$T$#$C$@.  This
type pattern matches any non-null instance of the given class. 
Note that the prefix of the class, if it is given, is relevant for determining
class instances. For instance, the pattern $p.C$ matches only
instances of classes $C$ which were created with the path $p$ as
prefix.

The bottom types \code{scala.Nothing} and \code{scala.Null} cannot
be used as type patterns, because they would match nothing in any case.  
\item
A singleton type \lstinline@$p$.type@. This type pattern matches only the value
denoted by the path $p$ (that is, a pattern match involved a
comparison of the matched value with $p$ using method \code{eq} in class
\code{AnyRef}).
\item
A compound type pattern \lstinline@$T_1$ with $\ldots$ with $T_n$@ where each $T_i$ is a
type pattern. This type pattern matches all values that are matched by each of
the type patterns $T_i$.
\item 
A parameterized type pattern $T[a_1 \commadots a_n]$, where the $a_i$
are type variable patterns or wildcards $\_$. 
This type pattern matches all values which match $T$ for
some arbitrary instantiation of the type variables and wildcards. The
bounds or alias type of these type variable are determined as
described in (\sref{sec:type-param-inf-pat}).
\item
A parameterized type pattern \lstinline@scala.Array$[T_1]$@, where
$T_1$ is a type pattern. This type pattern matches any non-null instance
of type \lstinline@scala.Array$[U_1]$@, where $U_1$ is a type matched by $T_1$.
\end{itemize}
Types which are not of one of the forms described above are also 
accepted as type patterns. However, such type patterns will be translated to their
erasure (\sref{sec:erasure}).  The Scala
compiler will issue an ``unchecked'' warning for these patterns to
flag the possible loss of type-safety.

A {\em type variable pattern} is a simple identifier which starts with
a lower case letter. However, the predefined primitive type aliases
\lstinline@unit@, \lstinline@boolean@, \lstinline@byte@,
\lstinline@short@, \lstinline@char@, \lstinline@int@,
\lstinline@long@, \lstinline@float@, and \lstinline@double@ are not
classified as type variable patterns.

\section{Type Parameter Inference in Patterns}\label{sec:type-param-inf-pat}

Type parameter inference is the process of finding bounds for the
bound type variables in a typed pattern or constructor
pattern. Inference takes into account the expected type of the
pattern.

\paragraph{Type parameter inference for typed patterns.}
Assume a typed pattern $p: T'$. Let $T$ result from $T'$ where all wildcards in
$T'$ are renamed to fresh variable names.  Let $a_1 \commadots a_n$ be
the type variables in $T$. These type variables are considered bound
in the pattern. Let the expected type of the pattern be $\proto$.

Type parameter inference constructs first a set of subtype constraints over
the type variables $a_i$. The initial constraints set $\CC_0$ reflects
just the bounds of these type variables. That is, assuming $T$ has
bound type variables $a_1 \commadots a_n$ which correspond to class
type parameters $a'_1 \commadots a'_n$ with lower bounds $L_1
\commadots L_n$ and upper bounds $U_1 \commadots U_n$, $\CC_0$
contains the constraints \bda{rcll} a_i &<:& \sigma U_i & \gap (i = 1
\commadots n)\\ \sigma L_i &<:& a_i & \gap (i = 1 \commadots n) \eda
where $\sigma$ is the substitution $[a'_1 := a_1 \commadots a'_n :=
a_n]$.

The set $\CC_0$ is then augmented by further subtype constraints. There are two
cases.

\paragraph{Case 1:}
If there exists a substitution $\sigma$ over the type variables $a_i
\commadots a_n$ such that $\sigma T$ conforms to $\proto$, one determines
the weakest subtype constraints $\CC_1$ over the type variables $a_1
\commadots a_n$ such that $\CC_0 \wedge \CC_1$ implies that $T$
conforms to $\proto$.

\paragraph{Case 2:}
Otherwise, if $T$ can not be made to conform to $\proto$ by
instantiating its type variables, one determines all type variables in
$\proto$ which are defined as type parameters of a method enclosing
the pattern. Let the set of such type parameters be $b_1 \commadots
b_m$. Let $\CC'_0$ be the subtype constraints reflecting the bounds of the
type variables $b_i$.  If $T$ denotes an instance type of a final
class, let $\CC_2$ be the weakest set of subtype constraints over the type
variables $a_1 \commadots a_n$ and $b_1 \commadots b_m$ such that
$\CC_0 \wedge \CC'_0 \wedge \CC_2$ implies that $T$ conforms to
$\proto$.  If $T$ does not denote an instance type of a final class,
let $\CC_2$ be the weakest set of subtype constraints over the type variables
$a_1 \commadots a_n$ and $b_1 \commadots b_m$ such that $\CC_0 \wedge
\CC'_0 \wedge \CC_2$ implies that it is possible to construct a type
$T'$ which conforms to both $T$ and $\proto$. It is a static error if
there is no satisfiable set of constraints $\CC_2$ with this property.

The final step consists in choosing type bounds for the type
variables which imply the established constraint system. The process
is different for the two cases above.

\paragraph{Case 1:}
We take $a_i >: L_i <: U_i$ where each
$L_i$ is minimal and each $U_i$ is maximal wrt $<:$ such that
$a_i >: L_i <: U_i$ for $i = 1 \commadots n$ implies $\CC_0 \wedge \CC_1$.

\paragraph{Case 2:}
We take $a_i >: L_i <: U_i$ and $b_i >: L'_i <: U'_i$ where each $L_i$
and $L'_j$ is minimal and each $U_i$ and $U'_j$ is maximal such that
$a_i >: L_i <: U_i$ for $i = 1 \commadots n$ and 
$b_j >: L'_j <: U'_j$ for $j = 1 \commadots m$
implies $\CC_0 \wedge \CC'_0 \wedge \CC_2$.

In both cases, local type inference is permitted to limit the
complexity of inferred bounds. Minimality and maximality of types have
to be understood relative to the set of types of acceptable
complexity.

\paragraph{Type parameter inference for constructor patterns.}
Assume a constructor pattern $C(p_1 \commadots p_n)$ where class $C$
has type type parameters $a_1 \commadots a_n$.  These type parameters
are inferred in the same way as for the typed pattern
~\lstinline@(_: $C[a_1 \commadots a_n]$)@.

\example
Consider the program fragment:
\begin{lstlisting}
val x: Any
x match {
  case y: List[a] => ...
}
\end{lstlisting}
Here, the type pattern \lstinline@List[a]@ is matched against the
expected type \lstinline@Any@. The pattern binds the type variable
\lstinline@a@.  Since \lstinline@List[a]@ conforms to \lstinline@Any@
for every type argument, there are no constraints on \lstinline@a@.
Hence, \lstinline@a@ is introduced as an abstract type with no
bounds. The scope of \lstinline@a@ is right-hand side of its case clause.

On the other hand, if \lstinline@x@ is declared as
\begin{lstlisting}
val x: List[List[String]],
\end{lstlisting}
this generates the constraint 
~\lstinline@List[a] <: List[List[String]]@, which simplifies to 
~\lstinline@a <: List[String]@, because \lstinline@List@ is covariant. Hence,
\lstinline@a@ is introduced with upper bound
\lstinline@List[String]@.

\example
Consider the program fragment: 
\begin{lstlisting}
val x: Any
x match {
  case y: List[String] => ...
}
\end{lstlisting}
Scala does not maintain information about type arguments at run-time,
so there is no way to check that \lstinline@x@ is a list of strings.
Instead, the Scala compiler will erase (\sref{sec:erasure}) the
pattern to \lstinline@List[_]@; that is, it will only test whether the
top-level runtime-class of the value \lstinline@x@ conforms to
\lstinline@List@, and the pattern match will succeed if it does.  This
might lead to a class cast exception later on, in the case where the
list \lstinline@x@ contains elements other than strings.  The Scala
compiler will flag this potential loss of type-safety with an
``unchecked'' warning message.

\example
Consider the program fragment
\begin{lstlisting}
class Term[A]
class Number(val n: Int) extends Term[Int]
def f[B](t: Term[B]): B = t match {
  case y: Number => y.n
}
\end{lstlisting}
The expected type of the pattern ~\lstinline@y: Number@~ is
~\lstinline@Term[B]@.  The type \code{Number} does not conform to
~\lstinline@Term[B]@; hence Case 2 of the rules above
applies. This means that \lstinline@b@ is treated as another type
variable for which subtype constraints are inferred. In our case the
applicable constraint is ~\lstinline@Number <: Term[B]@, which
entails \lstinline@B = Int@.  Hence, \lstinline@B@ is treated in
the case clause as an abstract type with lower and upper bound
\lstinline@Int@. Therefore, the right hand side of the case clause,
\lstinline@y.n@, of type \lstinline@Int@, is found to conform to the
function's declared result type, \lstinline@Number@.

\section{Pattern Matching Expressions}
\label{sec:pattern-match}

\syntax\begin{lstlisting}
  Expr            ::=  PostfixExpr `match' `{' CaseClauses `}'
  CaseClauses     ::=  CaseClause {CaseClause}
  CaseClause      ::=  `case' Pattern [Guard] `=>' Block
\end{lstlisting}

A pattern matching expression
\begin{lstlisting}
e match { case $p_1$ => $b_1$ $\ldots$ case $p_n$ => $b_n$ }
\end{lstlisting}
consists of a selector expression $e$ and a number $n > 0$ of
cases. Each case consists of a (possibly guarded) pattern $p_i$ and a
block $b_i$. Each $p_i$ might be complemented by a guard
~\lstinline@if $e$@~ where $e$ is a boolean expression. 
The scope of the pattern
variables in $p_i$ comprises the pattern's guard and the corresponding block $b_i$.

Let $T$ be the type of the selector expression $e$ and let $a_1
\commadots a_m$ be the type parameters of all methods enclosing 
the pattern matching expression.  For every $a_i$, let $L_i$ be its
lower bound and $U_i$ be its higher bound.  Every pattern $p \in
\{p_1, \commadots p_n\}$ can be typed in two ways. First, it is attempted
to type $p$ with $T$ as its expected type. If this fails, $p$ is
instead typed with a modified expected type $T'$ which results from
$T$ by replacing every occurrence of a type parameter $a_i$ by
\mbox{\sl undefined}.  If this second step fails also, a compile-time
error results. If the second step succeeds, let $T_p$ be the type of
pattern $p$ seen as an expression. One then determines minimal bounds
$L'_1 \commadots L'_m$ and maximal bounds $U'_1 \commadots U'_m$ such
that for all $i$, $L_i <: L'_i$ and $U'_i <: U_i$ and the following
constraint system is satisfied:
\[
    L_1 <: a_1 <: U_1\;\wedge\;\ldots\;\wedge\;L_m <: a_m <: U_m
    \ \Rightarrow\ T_p <: T
\]
If no such bounds can be found, a compile time error results.  If such
bounds are found, the pattern matching clause starting with $p$ is
then typed under the assumption that each $a_i$ has lower bound $L'_i$
instead of $L_i$ and has upper bound $U'_i$ instead of $U_i$.

The expected type of every block $b_i$ is the expected type of the
whole pattern matching expression.  The type of the pattern matching
expression is then the weak least upper bound
(\sref{sec:weakconformance})
of the types of all blocks
$b_i$.

When applying a pattern matching expression to a selector value,
patterns are tried in sequence until one is found which matches the
selector value (\sref{sec:patterns}). Say this case is $\CASE;p_i
\Arrow b_i$.  The result of the whole expression is then the result of
evaluating $b_i$, where all pattern variables of $p_i$ are bound to
the corresponding parts of the selector value.  If no matching pattern
is found, a \code{scala.MatchError} exception is thrown.

The pattern in a case may also be followed by a guard suffix \
\code{if e}\ with a boolean expression $e$.  The guard expression is
evaluated if the preceding pattern in the case matches. If the guard
expression evaluates to \code{true}, the pattern match succeeds as
normal. If the guard expression evaluates to \code{false}, the pattern
in the case is considered not to match and the search for a matching
pattern continues.

In the interest of efficiency the evaluation of a pattern matching
expression may try patterns in some other order than textual
sequence. This might affect evaluation through
side effects in guards. However, it is guaranteed that a guard
expression is evaluated only if the pattern it guards matches.

If the selector of a pattern match is an instance of a
\lstinline@sealed@ class (\sref{sec:modifiers}), 
the compilation of pattern matching can emit warnings which diagnose
that a given set of patterns is not exhaustive, i.e.\ that there is a
possibility of a \code{MatchError} being raised at run-time. 

\example\label{ex:eval}
 Consider the following definitions of arithmetic terms:

\begin{lstlisting}
abstract class Term[T]
case class Lit(x: Int) extends Term[Int]
case class Succ(t: Term[Int]) extends Term[Int]
case class IsZero(t: Term[Int]) extends Term[Boolean]
case class If[T](c: Term[Boolean],
                 t1: Term[T],
                 t2: Term[T]) extends Term[T]
\end{lstlisting}
There are terms to represent numeric literals, incrementation, a zero
test, and a conditional. Every term carries as a type parameter the
type of the expression it representes (either \code{Int} or \code{Boolean}).

A type-safe evaluator for such terms can be written as follows.
\begin{lstlisting}
def eval[T](t: Term[T]): T = t match {
  case Lit(n)        => n
  case Succ(u)       => eval(u) + 1
  case IsZero(u)     => eval(u) == 0
  case If(c, u1, u2) => eval(if (eval(c)) u1 else u2)
}
\end{lstlisting}
Note that the evaluator makes crucial use of the fact that type
parameters of enclosing methods can acquire new bounds through pattern
matching.

For instance, the type of the pattern in the second case,
~\lstinline@Succ(u)@, is \code{Int}. It conforms to the selector type
\code{T} only if we assume an upper and lower bound of \code{Int} for \code{T}.
Under the assumption ~\lstinline@Int <: T <: Int@~ we can also
verify that the type right hand side of the second case, \code{Int}
conforms to its expected type, \code{T}.

\section{Pattern Matching Anonymous Functions}
\label{sec:pattern-closures}

\syntax\begin{lstlisting}
  BlockExpr ::= `{' CaseClauses `}'
\end{lstlisting}

An anonymous function can be defined by a sequence of cases 
\begin{lstlisting}
{ case $p_1$ => $b_1$ $\ldots$ case $p_n$ => $b_n$ }
\end{lstlisting}
which appear as an expression without a prior \code{match}.  The
expected type of such an expression must in part be defined. It must
be either ~\lstinline@scala.Function$k$[$S_1 \commadots S_k$, $R$]@~ for some $k > 0$,
or ~\lstinline@scala.PartialFunction[$S_1$, $R$]@, where the
argument type(s) $S_1 \commadots S_k$ must be fully determined, but the result type
$R$ may be undetermined.

If the expected type is ~\lstinline@scala.Function$k$[$S_1 \commadots S_k$, $R$]@~,
the expression is taken to be equivalent to the anonymous function:
\begin{lstlisting}
($x_1: S_1 \commadots x_k: S_k$) => ($x_1 \commadots x_k$) match { 
  case $p_1$ => $b_1$ $\ldots$ case $p_n$ => $b_n$ 
}
\end{lstlisting}
Here, each $x_i$ is a fresh name.
As was shown in (\sref{sec:closures}), this anonymous function is in turn
equivalent to the following instance creation expression, where
 $T$ is the weak least upper bound of the types of all $b_i$.
\begin{lstlisting}
new scala.Function$k$[$S_1 \commadots S_k$, $T$] {
  def apply($x_1: S_1 \commadots x_k: S_k$): $T$ = ($x_1 \commadots x_k$) match {
    case $p_1$ => $b_1$ $\ldots$ case $p_n$ => $b_n$
  }
}
\end{lstlisting}
If the expected type is ~\lstinline@scala.PartialFunction[$S$, $R$]@,
the expression is taken to be equivalent to the following instance creation expression:
\begin{lstlisting}
new scala.PartialFunction[$S$, $T$] {
  def apply($x$: $S$): $T$ = x match {
    case $p_1$ => $b_1$ $\ldots$ case $p_n$ => $b_n$
  }
  def isDefinedAt($x$: $S$): Boolean = {
    case $p_1$ => true $\ldots$ case $p_n$ => true
    case _ => false
  }
}
\end{lstlisting}
Here, $x$ is a fresh name and $T$ is the weak least upper bound of the
types of all $b_i$. The final default case in the \code{isDefinedAt}
method is omitted if one of the patterns $p_1 \commadots p_n$ is
already a variable or wildcard pattern.

\example Here is a method which uses a fold-left operation
\code{/:} to compute the scalar product of 
two vectors:
\begin{lstlisting}
def scalarProduct(xs: Array[Double], ys: Array[Double]) = 
  (0.0 /: (xs zip ys)) {
    case (a, (b, c)) => a + b * c
  }
\end{lstlisting}
The case clauses in this code are equivalent to the following
anonymous funciton:
\begin{lstlisting}
  (x, y) => (x, y) match {
    case (a, (b, c)) => a + b * c
  }
\end{lstlisting}

