Implicit Parameters and Views
=============================

\section{The Implicit Modifier}\label{sec:impl-defs}

\syntax\begin{lstlisting}
  LocalModifier  ::= `implicit'
  ParamClauses   ::= {ParamClause} [nl] `(' `implicit' Params `)'
\end{lstlisting}

Template members and parameters labeled with an \code{implicit}
modifier can be passed to implicit parameters (\sref{sec:impl-params})
and can be used as implicit conversions called views
(\sref{sec:views}). The \code{implicit} modifier is illegal for all
type members, as well as for top-level (\sref{sec:packagings})
objects.

\example\label{ex:impl-monoid}
The following code defines an abstract class of monoids and
two concrete implementations, \code{StringMonoid} and
\code{IntMonoid}. The two implementations are marked implicit.

\begin{lstlisting}
abstract class Monoid[A] extends SemiGroup[A] {
  def unit: A
  def add(x: A, y: A): A
}
object Monoids {
  implicit object stringMonoid extends Monoid[String] {
    def add(x: String, y: String): String = x.concat(y)
    def unit: String = ""
  }
  implicit object intMonoid extends Monoid[Int] {
    def add(x: Int, y: Int): Int = x + y
    def unit: Int = 0
  }
}
\end{lstlisting}

\section{Implicit Parameters}\label{sec:impl-params}

An implicit parameter list
~\lstinline@(implicit $p_1$,$\ldots$,$p_n$)@~ of a method marks the parameters $p_1 \commadots p_n$ as
implicit. A method or constructor can have only one implicit parameter
list, and it must be the last parameter list given.

A method with implicit parameters can be applied to arguments just
like a normal method. In this case the \code{implicit} label has no
effect. However, if such a method misses arguments for its implicit
parameters, such arguments will be automatically provided.

The actual arguments that are eligible to be passed to an implicit
parameter of type $T$ fall into two categories. First, eligible are
all identifiers $x$ that can be accessed at the point of the method
call without a prefix and that denote an implicit definition
(\sref{sec:impl-defs}) or an implicit parameter.  An eligible
identifier may thus be a local name, or a member of an enclosing
template, or it may be have been made accessible without a prefix
through an import clause (\sref{sec:import}). If there are no eligible
identifiers under this rule, then, second, eligible are also all
\code{implicit} members of some object that belongs to the implicit
scope of the implicit parameter's type, $T$.

The {\em implicit scope} of a type $T$ consists of all companion modules
(\sref{sec:object-defs}) of classes that are associated with the
implicit parameter's type.  Here, we say a class $C$ is {\em
associated} with a type $T$, if it is a base class
(\sref{sec:linearization}) of some part of $T$.  The {\em parts} of a
type $T$ are:
\begin{itemize}
\item
if $T$ is a compound type ~\lstinline@$T_1$ with $\ldots$ with $T_n$@, the
union of the parts of $T_1 \commadots T_n$, as well as $T$ itself,
\item
if $T$ is a parameterized type ~\lstinline@$S$[$T_1 \commadots T_n$]@, 
the union of the parts of $S$ and $T_1 \commadots T_n$,
\item
if $T$ is a singleton type ~\lstinline@$p$.type@, the parts of the type
of $p$,
\item
if $T$ is a type projection ~\lstinline@$S$#$U$@, the parts of $S$ as
well as $T$ itself,
\item
in all other cases, just $T$ itself.
\end{itemize}

If there are several eligible arguments which match the implicit
parameter's type, a most specific one will be chosen using the rules
of static overloading resolution (\sref{sec:overloading-resolution}).
If the parameter has a default argument and no implicit argument can
be found the default argument is used.

\example Assuming the classes from \ref{ex:impl-monoid}, here is a 
method which computes the sum of a list of elements using the
monoid's \code{add} and \code{unit} operations.
\begin{lstlisting}
def sum[A](xs: List[A])(implicit m: Monoid[A]): A = 
  if (xs.isEmpty) m.unit
  else m.add(xs.head, sum(xs.tail))
\end{lstlisting}
The monoid in question is marked as an implicit parameter, and can therefore
be inferred based on the type of the list.
Consider for instance the call 
\begin{lstlisting}
  sum(List(1, 2, 3))
\end{lstlisting}
in a context where \lstinline@stringMonoid@ and \lstinline@intMonoid@
are visible.  We know that the formal type parameter \lstinline@a@ of
\lstinline@sum@ needs to be instantiated to \lstinline@Int@. The only
eligible object which matches the implicit formal parameter type
\lstinline@Monoid[Int]@ is \lstinline@intMonoid@ so this object will
be passed as implicit parameter.\bigskip

This discussion also shows that implicit parameters are inferred after
any type arguments are inferred (\sref{sec:local-type-inf}). 

Implicit methods can themselves have implicit parameters. An example
is the following method from module \code{scala.List}, which injects
lists into the \lstinline@scala.Ordered@ class, provided the element
type of the list is also convertible to this type.
\begin{lstlisting}
implicit def list2ordered[A](x: List[A])
  (implicit elem2ordered: A => Ordered[A]): Ordered[List[A]] = 
  ...
\end{lstlisting}
Assume in addition a method
\begin{lstlisting}
implicit def int2ordered(x: Int): Ordered[Int]
\end{lstlisting}
that injects integers into the \lstinline@Ordered@ class.  We can now
define a \code{sort} method over ordered lists:
\begin{lstlisting}
def sort[A](xs: List[A])(implicit a2ordered: A => Ordered[A]) = ...
\end{lstlisting}
We can apply \code{sort} to a list of lists of integers ~\lstinline@yss: List[List[Int]]@~ 
as follows:
\begin{lstlisting}
sort(yss)
\end{lstlisting}
The call above will be completed by passing two nested implicit arguments:
\begin{lstlisting}
sort(yss)(xs: List[Int] => list2ordered[Int](xs)(int2ordered)) .
\end{lstlisting}
The possibility of passing implicit arguments to implicit arguments
raises the possibility of an infinite recursion.  For instance, one
might try to define the following method, which injects {\em every} type into the \lstinline@Ordered@ class:
\begin{lstlisting}
implicit def magic[A](x: A)(implicit a2ordered: A => Ordered[A]): Ordered[A] = 
  a2ordered(x)
\end{lstlisting}
Now, if one tried to apply
\lstinline@sort@ to an argument \code{arg} of a type that did not have
another injection into the \code{Ordered} class, one would obtain an infinite
expansion:
\begin{lstlisting}
sort(arg)(x => magic(x)(x => magic(x)(x => ... )))
\end{lstlisting}
To prevent such infinite expansions, the compiler keeps track of 
a stack of ``open implicit types'' for which implicit arguments are currently being
searched. Whenever an implicit argument for type $T$ is searched, the
``core type'' of $T$ is added to the stack. Here, the {\em core type}
of $T$ is $T$ with aliases expanded, top-level type annotations (\sref{sec:annotations}) and
refinements (\sref{sec:refinements}) removed, and occurrences
of top-level existentially bound variables replaced by their upper
bounds. The core type is removed from the stack once the search for
the implicit argument either definitely fails or succeeds. Everytime a
core type is added to the stack, it is checked that this type does not
dominate any of the other types in the set.

Here, a core type $T$ {\em dominates} a type $U$ if $T$ is equivalent (\sref{sec:type-equiv})
to $U$, or if the top-level type constructors of $T$ and $U$ have a
common element and $T$ is more complex than $U$.

The set of {\em top-level type constructors} $\ttcs(T)$ of a type $T$ depends on the form of
the type:
\begin{quote}
For a type designator, \\
$\ttcs(p.c) ~=~ \{c\}$; \\
For a parameterized type, \\
$\ttcs(p.c[\targs]) ~=~ \{c\}$; \\
For a singleton type, \\
$\ttcs(p.type) ~=~ \ttcs(T)$, provided $p$ has type $T$;\\
For a compound type, \\
\lstinline@$\ttcs(T_1$ with $\ldots$ with $T_n)$@ $~=~ \ttcs(T_1) \cup \ldots \cup \ttcs(T_n)$.
\end{quote}

The {\em complexity} $\complx(T)$ of a core type is an integer which also depends on the form of
the type:
\begin{quote}
For a type designator, \\
$\complx(p.c) ~=~ 1 + \complx(p)$ \\
For a parameterized type, \\
$\complx(p.c[\targs]) ~=~ 1 + \Sigma \complx(\targs)$ \\
For a singleton type denoting a package $p$, \\
$\complx(p.type) ~=~ 0$ \\
For any other singleton type, \\
$\complx(p.type) ~=~ 1 + \complx(T)$, provided $p$ has type $T$;\\
For a compound type, \\
\lstinline@$\complx(T_1$ with $\ldots$ with $T_n)$@ $= \Sigma\complx(T_i)$
\end{quote}

\example When typing \code{sort(xs)} for some list \code{xs} of type \code{List[List[List[Int]]]}, 
the sequence of types for 
which implicit arguments are searched is 
\begin{lstlisting}
List[List[Int]] => Ordered[List[List[Int]]], 
List[Int] => Ordered[List[Int]]
Int => Ordered[Int]
\end{lstlisting}
All types share the common type constructor \code{scala.Function1}, 
but the complexity of the each new type is lower than the complexity of the previous types. 
Hence, the code typechecks.

\example Let \code{ys} be a list of some type which cannot be converted
to \code{Ordered}. For instance:
\begin{lstlisting}
val ys = List(new IllegalArgumentException, new ClassCastException, new Error)
\end{lstlisting}
Assume that the definition of \code{magic} above is in scope. Then the sequence
of types for which implicit arguments are searched is 
\begin{lstlisting}
Throwable => Ordered[Throwable],
Throwable => Ordered[Throwable],
...
\end{lstlisting}
Since the second type in the sequence is equal to the first, the compiler
will issue an error signalling a divergent implicit expansion.

\section{Views}\label{sec:views}

Implicit parameters and methods can also define implicit conversions
called views. A {\em view} from type $S$ to type $T$ is
defined by an implicit value which has function type
\lstinline@$S$=>$T$@ or \lstinline@(=>$S$)=>$T$@ or by a method convertible to a value of that
type.

Views are applied in three situations.
\begin{enumerate}
\item
If an expression $e$ is of type $T$, and $T$ does not conform to the
expression's expected type $\proto$. In this case an implicit $v$ is
searched which is applicable to $e$ and whose result type conforms to
$\proto$.  The search proceeds as in the case of implicit parameters,
where the implicit scope is the one of ~\lstinline@$T$ => $\proto$@. If
such a view is found, the expression $e$ is converted to
\lstinline@$v$($e$)@. 
\item
In a selection $e.m$ with $e$ of type $T$, if the selector $m$ does
not denote a member of $T$.  In this case, a view $v$ is searched
which is applicable to $e$ and whose result contains a member named
$m$.  The search proceeds as in the case of implicit parameters, where
the implicit scope is the one of $T$.  If such a view is found, the
selection $e.m$ is converted to \lstinline@$v$($e$).$m$@.
\item
In a selection $e.m(\args)$ with $e$ of type $T$, if the selector
$m$ denotes some member(s) of $T$, but none of these members is applicable to the arguments
$\args$. In this case a view $v$ is searched which is applicable to $e$ 
and whose result contains a method $m$ which is applicable to $\args$.
The search proceeds as in the case of implicit parameters, where
the implicit scope is the one of $T$.  If such a view is found, the
selection $e.m$ is converted to \lstinline@$v$($e$).$m(\args)$@.
\end{enumerate}
The implicit view, if it is found, can accept is argument $e$ as a
call-by-value or as a call-by-name parameter. However, call-by-value
implicits take precedence over call-by-name implicits.

As for implicit parameters, overloading resolution is applied
if there are several possible candidates (of either the call-by-value
or the call-by-name category).

\example\label{ex:impl-ordered} Class \lstinline@scala.Ordered[A]@ contains a method
\begin{lstlisting}
  def <= [B >: A](that: B)(implicit b2ordered: B => Ordered[B]): Boolean .
\end{lstlisting}
Assume two lists \code{xs} and \code{ys} of type \code{List[Int]}
and assume that the \code{list2ordered} and \code{int2ordered}
methods defined in \sref{sec:impl-params} are in scope.
Then the operation
\begin{lstlisting}
  xs <= ys
\end{lstlisting}
is legal, and is expanded to:
\begin{lstlisting}
  list2ordered(xs)(int2ordered).<=
    (ys)
    (xs => list2ordered(xs)(int2ordered))
\end{lstlisting}
The first application of \lstinline@list2ordered@ converts the list
\code{xs} to an instance of class \code{Ordered}, whereas the second 
occurrence is part of an implicit parameter passed to the \code{<=}
method.

\section{Context Bounds and View Bounds}\label{sec:context-bounds}

\syntax\begin{lstlisting}
  TypeParam ::= (id | `_') [TypeParamClause] [`>:' Type] [`<:'Type] 
                {`<%' Type} {`:' Type}
\end{lstlisting}

A type parameter $A$ of a method or non-trait class may have one or more view
bounds \lstinline@$A$ <% $T$@. In this case the type parameter may be
instantiated to any type $S$ which is convertible by application of a 
view to the bound $T$.

A type parameter $A$ of a method or non-trait class may also have one
or more context bounds \lstinline@$A$ : $T$@. In this case the type parameter may be
instantiated to any type $S$ for which {\em evidence} exists at the
instantiation point that $S$ satisfies the bound $T$. Such evidence
consists of an implicit value with type $T[S]$.

A method or class containing type parameters with view or context bounds is treated as being
equivalent to a method with implicit parameters. Consider first the case of a
single parameter with view and/or context bounds such as:
\begin{lstlisting}
def $f$[$A$ <% $T_1$ ... <% $T_m$ : $U_1$ : $U_n$]($\ps$): $R$ = ...
\end{lstlisting}
Then the method definition above is expanded to
\begin{lstlisting}
def $f$[$A$]($\ps$)(implicit $v_1$: $A$ => $T_1$, ..., $v_m$: $A$ => $T_m$,
                       $w_1$: $U_1$[$A$], ..., $w_n$: $U_n$[$A$]): $R$ = ...
\end{lstlisting}
where the $v_i$ and $w_j$ are fresh names for the newly introduced implicit parameters. These
parameters are called {\em evidence parameters}.

If a class or method has several view- or context-bounded type parameters, each
such type parameter is expanded into evidence parameters in the order
they appear and all the resulting evidence parameters are concatenated
in one implicit parameter section.  Since traits do not take
constructor parameters, this translation does not work for them.
Consequently, type-parameters in traits may not be view- or context-bounded.
Also, a method or class with view- or context bounds may not define any
additional implicit parameters.

\example The \code{<=} method mentioned in \ref{ex:impl-ordered} can be declared
more concisely as follows:
\begin{lstlisting}
  def <= [B >: A <% Ordered[B]](that: B): Boolean
\end{lstlisting}

\section{Manifests}\label{sec:manifests}

\newcommand{\Mobj}{\mbox{\sl Mobj}}

Manifests are type descriptors that can be automatically generated by
the Scala compiler as arguments to implicit parameters. The Scala
standard library contains a hierarchy of four manifest classes, 
with \lstinline@OptManifest@
at the top. Their signatures follow the outline below.
\begin{lstlisting}
trait OptManifest[+T]
object NoManifest extends OptManifest[Nothing]
trait ClassManifest[T] extends OptManifest[T]
trait Manifest[T] extends ClassManifest[T]
\end{lstlisting}

If an implicit parameter of a method or constructor is of a subtype $M[T]$ of
class \lstinline@OptManifest[T]@, {\em a manifest is determined for $M[S]$},
according to the following rules.

First if there is already an implicit argument that matches $M[T]$, this
argument is selected.

Otherwise, let $\Mobj$ be the companion object \lstinline@scala.reflect.Manifest@
if $M$ is trait \lstinline@Manifest@, or be
the companion object \lstinline@scala.reflect.ClassManifest@ otherwise. Let $M'$ be the trait
\lstinline@Manifest@ if $M$ is trait \lstinline@Manifest@, or be the trait \lstinline@OptManifest@ otherwise.  
Then the following rules apply.


\begin{enumerate}
\item
If $T$ is a value class or one of the classes \lstinline@Any@, \lstinline@AnyVal@, \lstinline@Object@, 
\lstinline@Null@, or \lstinline@Nothing@,
a manifest for it is generated by selecting
the corresponding manifest value \lstinline@Manifest.$T$@, which exists in the
\lstinline@Manifest@ module.
\item
If $T$ is an instance of \lstinline@Array[$S$]@, a manifest is generated
with the invocation \lstinline@$\Mobj$.arrayType[S](m)@, where $m$ is the manifest
determined for $M[S]$.
\item
If $T$ is some other class type $S\#C[U_1 \commadots U_n]$ where the prefix type $S$ 
cannot be statically determined from the class $C$, 
a manifest is generated 
with the invocation \lstinline@$\Mobj$.classType[T]($m_0$, classOf[T], $ms$)@
where $m_0$ is the manifest determined for $M'[S]$ and $ms$ are the
manifests determined for $M'[U_1] \commadots M'[U_n]$.
\item
If $T$ is some other class type with type arguments $U_1 \commadots U_n$, 
a manifest is generated 
with the invocation \lstinline@$\Mobj$.classType[T](classOf[T], $ms$)@
where $ms$ are the
manifests determined for $M'[U_1] \commadots M'[U_n]$.
\item 
If $T$ is a singleton type ~\lstinline@$p$.type@, a manifest is generated with 
the invocation
\lstinline@$\Mobj$.singleType[T]($p$)@ 
\item
If $T$ is a refined type $T' \{ R \}$, a manifest is generated for $T'$.
(That is, refinements are never reflected in manifests).
\item
If $T$ is an intersection type 
\lstinline@$T_1$ with $\commadots$ with $T_n$@ 
where $n > 1$, the result depends on whether a full manifest is
to be determined or not. 
If $M$ is trait \lstinline@Manifest@, then
a manifest is generated with the invocation
\lstinline@Manifest.intersectionType[T]($ms$)@ where $ms$ are the manifests
determined for $M[T_1] \commadots M[T_n]$.
Otherwise, if $M$ is trait \lstinline@ClassManifest@, 
then a manifest is generated for the intersection dominator
(\sref{sec:erasure})
of the types $T_1 \commadots T_n$.
\item
If $T$ is some other type, then if $M$ is trait \lstinline@OptManifest@, 
a manifest is generated from the designator \lstinline@scala.reflect.NoManifest@.
If $M$ is a type different from \lstinline@OptManifest@, a static error results.
\end{enumerate}

