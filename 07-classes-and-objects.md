Classes and Objects
===================


\syntax\begin{lstlisting}
  TmplDef          ::= [`case'] `class' ClassDef
                    |  [`case'] `object' ObjectDef
                    |  `trait' TraitDef
\end{lstlisting}

Classes (\sref{sec:class-defs}) and objects
(\sref{sec:object-defs}) are both defined in terms of {\em templates}.

\section{Templates}
\label{sec:templates}

\syntax\begin{lstlisting}
  ClassTemplate   ::=  [EarlyDefs] ClassParents [TemplateBody]
  TraitTemplate   ::=  [EarlyDefs] TraitParents [TemplateBody]
  ClassParents    ::=  Constr {`with' AnnotType}
  TraitParents    ::=  AnnotType {`with' AnnotType}
  TemplateBody    ::=  [nl] `{' [SelfType] TemplateStat {semi TemplateStat} `}'
  SelfType        ::=  id [`:' Type] `=>'
                   |   this `:' Type `=>' 
\end{lstlisting}

A template defines the type signature, behavior and initial state of a
trait or class of objects or of a single object. Templates form part of
instance creation expressions, class definitions, and object
definitions.  A template 
~\lstinline@$sc$ with $mt_1$ with $\ldots$ with $mt_n$ {$\stats\,$}@~ 
consists of a constructor invocation $sc$
which defines the template's {\em superclass}, trait references
~\lstinline@$mt_1 \commadots mt_n$@~ $(n \geq 0)$, which define the
template's {\em traits}, and a statement sequence $\stats$ which
contains initialization code and additional member definitions for the
template.

Each trait reference $mt_i$ must denote a trait (\sref{sec:traits}).
By contrast, the superclass constructor $sc$ normally refers to a
class which is not a trait. It is possible to write a list of
parents that starts with a trait reference, e.g.
~\lstinline@$mt_1$ with $\ldots$ with $mt_n$@. In that case the list
of parents is implicitly extended to include the supertype of $mt_1$
as first parent type. The new supertype must have at least one
constructor that does not take parameters.  In the following, we will
always assume that this implicit extension has been performed, so that
the first parent class of a template is a regular superclass
constructor, not a trait reference.

The list of parents of every class is also always implicitly extended
by a reference to the \code{scala.ScalaObject} trait as last
mixin. E.g.\
\begin{lstlisting}
$sc$ with $mt_1$ with $\ldots$ with $mt_n$ {$\stats\,$}
\end{lstlisting}
becomes
\begin{lstlisting}
$mt_1$ with $\ldots$ with $mt_n$ with ScalaObject {$\stats\,$}.
\end{lstlisting}

The list of parents of a template must be well-formed. This means that
the class denoted by the superclass constructor $sc$ must be a
subclass of the superclasses of all the traits $mt_1 \commadots mt_n$.
In other words, the non-trait classes inherited by a template form a
chain in the inheritance hierarchy which starts with the template's
superclass.

The {\em least proper supertype} of a template is the class type or
compound type (\sref{sec:compound-types}) consisting of all its parent
class types. 

The statement sequence $\stats$ contains member definitions that
define new members or overwrite members in the parent classes.  If the
template forms part of an abstract class or trait definition, the
statement part $\stats$ may also contain declarations of abstract
members. If the template forms part of a concrete class definition,
$\stats$ may still contain declarations of abstract type members, but
not of abstract term members.  Furthermore, $\stats$ may in any case
also contain expressions; these are executed in the order they are
given as part of the initialization of a template.

The sequence of template statements may be prefixed with a formal
parameter definition and an arrow, e.g.\ \lstinline@$x$ =>@, or
~\lstinline@$x$:$T$ =>@.  If a formal parameter is given, it can be
used as an alias for the reference \lstinline@this@ throughout the
body of the template.  
If the formal parameter comes with a type $T$, this definition affects
the {\em self type} $S$ of the underlying class or object as follows:  Let $C$ be the type
of the class or trait or object defining the template.
If a type $T$ is given for the formal self parameter, $S$
is the greatest lower bound of $T$ and $C$.
If no type $T$ is given, $S$ is just $C$.
Inside the template, the type of \code{this} is assumed to be $S$.

The self type of a class or object must conform to the self types of
all classes which are inherited by the template $t$. 

A second form of self type annotation reads just 
~\lstinline@this: $S$ =>@. It prescribes the type $S$ for \lstinline@this@
without introducing an alias name for it. 

\example Consider the following class definitions:

\begin{lstlisting}
class Base extends Object {}
trait Mixin extends Base {}
object O extends Mixin {}
\end{lstlisting}
In this case, the definition of \code{O} is expanded to:
\begin{lstlisting}
object O extends Base with Mixin {}
\end{lstlisting}

%The type of each non-private definition or declaration of a
%template must be equivalent to a type which does not refer to any
%private members of that template.

\todo{Make all references to Java generic}

\paragraph{\em Inheriting from Java Types} A template may have a Java class as
its superclass and Java interfaces as its mixins. 

\paragraph{\em Template Evaluation}
Consider a template ~\lstinline@$sc$ with $mt_1$ with $mt_n$ {$\stats\,$}@.

If this is the template of a trait (\sref{sec:traits}) then its {\em
mixin-evaluation} consists of an evaluation of the statement sequence
$\stats$.

If this is not a template of a trait, then its {\em evaluation}
consists of the following steps.
\begin{itemize}
\item
First, the superclass constructor $sc$ is evaluated (\sref{sec:constr-invoke}).
\item
Then, all base classes in the template's linearization
(\sref{sec:linearization}) up to the
template's superclass denoted by $sc$ are
mixin-evaluated. Mixin-evaluation happens in reverse order of
occurrence in the linearization.
\item 
Finally the statement sequence $\stats\,$ is evaluated.
\end{itemize}

\paragraph{\em Delayed Initializaton}
The initialization code of an object or class (but not a trait) that follows the superclass
constructor invocation and the mixin-evaluation of the template's base
classes is passed to a special hook, which is inaccessible from user
code. Normally, that hook simply executes the code that is passed to
it. But templates inheriting the \lstinline@scala.DelayedInit@ trait
can override the hook by re-implementing the \lstinline@delayedInit@
method, which is defined as follows:

\begin{lstlisting}
  def delayedInit(body: => Unit)
\end{lstlisting}


\subsection{Constructor Invocations}
\label{sec:constr-invoke}
\syntax\begin{lstlisting}
  Constr  ::=  AnnotType {`(' [Exprs] `)'}
\end{lstlisting}

Constructor invocations define the type, members, and initial state of
objects created by an instance creation expression, or of parts of an
object's definition which are inherited by a class or object
definition. A constructor invocation is a function application
~\lstinline@$x$.$c$[$\targs$]($\args_1$)$\ldots$($\args_n$)@, where $x$ is a stable identifier
(\sref{sec:stable-ids}), $c$ is a type name which either designates a
class or defines an alias type for one, $\targs$ is a type argument
list, $\args_1 \commadots \args_n$ are argument lists, and there is a
constructor of that class which is applicable (\sref{sec:apply})
to the given arguments. If the constructor invocation uses named or
default arguments, it is transformed into a block expression using the
same transformation as described in (\sref{sec:named-default}).

The prefix `\lstinline@$x$.@' can be omitted.  A type argument list
can be given only if the class $c$ takes type parameters.  Even then
it can be omitted, in which case a type argument list is synthesized
using local type inference (\sref{sec:local-type-inf}). If no explicit
arguments are given, an empty list \lstinline@()@ is implicitly supplied.

An evaluation of a constructor invocation 
~\lstinline@$x$.$c$[$\targs$]($\args_1$)$\ldots$($\args_n$)@~
consists of the following steps:
\begin{itemize}
\item First, the prefix $x$ is evaluated.
\item Then, the arguments $\args_1 \commadots \args_n$ are evaluated from left to right.
\item Finally, the class being constructed is initialized by evaluating the
  template of the class referred to by $c$.
\end{itemize}

\subsection{Class Linearization}\label{sec:linearization}

The classes reachable through transitive closure of the direct
inheritance relation from a class $C$ are called the {\em
base classes} of $C$.  Because of mixins, the inheritance relationship
on base classes forms in general a directed acyclic graph. A
linearization of this graph is defined as follows.

\newcommand{\uright}{\;\vec +\;}
\newcommand{\lin}[1]{{\cal L}(#1)}

\begin{definition}\label{def:lin} Let $C$ be a class with template
~\lstinline@$C_1$ with ... with $C_n$ { $\stats$ }@.
The {\em linearization} of $C$, $\lin C$ is defined as follows:
\bda{rcl}
\lin C &=& C\ , \ \lin{C_n} \uright \ldots \uright \lin{C_1} 
\eda
Here $\uright$ denotes concatenation where elements of the right operand
replace identical elements of the left operand:
\bda{lcll}
\{a, A\} \uright B &=& a, (A \uright B)  &{\bf if} a \not\in B \\
                 &=& A \uright B       &{\bf if} a \in B
\eda
\end{definition}

\example Consider the following class definitions.
\begin{lstlisting}
abstract class AbsIterator extends AnyRef { ... }
trait RichIterator extends AbsIterator { ... }
class StringIterator extends AbsIterator { ... }
class Iter extends StringIterator with RichIterator { ... }
\end{lstlisting}
Then the linearization of class \lstinline@Iter@ is
\begin{lstlisting}
{ Iter, RichIterator, StringIterator, AbsIterator, ScalaObject, AnyRef, Any }
\end{lstlisting}
Trait \lstinline@ScalaObject@ appears in this list because it 
is added as last mixin to every Scala class (\sref{sec:templates}).

Note that the linearization of a class refines the inheritance
relation: if $C$ is a subclass of $D$, then $C$ precedes $D$ in any
linearization where both $C$ and $D$ occur.
\ref{def:lin} also satisfies the property that a linearization
of a class always contains the linearization of its direct superclass
as a suffix.  For instance, the linearization of
\lstinline@StringIterator@ is
\begin{lstlisting}
{ StringIterator, AbsIterator, ScalaObject, AnyRef, Any }
\end{lstlisting}
which is a suffix of the linearization of its subclass \lstinline@Iter@.
The same is not true for the linearization of mixins.
For instance, the linearization of \lstinline@RichIterator@ is
\begin{lstlisting}
{ RichIterator, AbsIterator, ScalaObject, AnyRef, Any }
\end{lstlisting}
which is not a suffix of the linearization of \lstinline@Iter@.


\subsection{Class Members}
\label{sec:members}

A class $C$ defined by a template 
~\lstinline@$C_1$ with $\ldots$ with $C_n$ { $\stats$ }@~ 
can define members in its statement sequence
$\stats$ and can inherit members from all parent classes.  Scala
adopts Java and C\#'s conventions for static overloading of
methods. It is thus possible that a class defines and/or inherits
several methods with the same name.  To decide whether a defined
member of a class $C$ overrides a member of a parent class, or whether
the two co-exist as overloaded variants in $C$, Scala uses the
following definition of {\em matching} on members:

\begin{definition}
A member definition $M$ {\em matches} a member definition $M'$, if $M$
and $M'$ bind the same name, and one of following holds.
\begin{enumerate}
\item Neither $M$ nor $M'$ is a method definition.
\item $M$ and $M'$ define both monomorphic methods with equivalent argument
  types.
\item $M$ defines a parameterless method and $M'$ defines a method
  with an empty parameter list \code{()} or {\em vice versa}. 
\item $M$ and $M'$ define both polymorphic methods with 
equal number of argument types $\overline T$, $\overline T'$
and equal numbers of type parameters
$\overline t$, $\overline t'$, say, and $\overline T' = [\overline t'/\overline t]\overline T$.
%every argument type
%$T_i$ of $M$ is equal to the corresponding argument type $T'_i$ of
%$M'$ where every occurrence of a type parameter $t'$ of $M'$ has been replaced by the corresponding type parameter $t$ of $M$.
\end{enumerate}
\end{definition}
Member definitions fall into two categories: concrete and abstract.
Members of class $C$ are either {\em directly defined} (i.e.\ they appear in
$C$'s statement sequence $\stats$) or they are {\em inherited}.  There are two rules
that determine the set of members of a class, one for each category:

\begin{definition}\label{def:member}
A {\em concrete member} of a class $C$ is any concrete definition $M$ in
some class $C_i \in \lin C$, except if there is a preceding class $C_j
\in \lin C$ where $j < i$ which directly defines a concrete member $M'$ matching $M$.  

An {\em abstract member} of a class $C$ is any abstract definition $M$
in some class $C_i \in \lin C$, except if $C$ contains already a
concrete member $M'$ matching $M$, or if there is a preceding class
$C_j \in \lin C$ where $j < i$ which directly defines an abstract member $M'$ matching
$M$.
\end{definition}
This definition also determines the overriding relationships between
matching members of a class $C$ and its parents (\sref{sec:overriding}).  
First, a concrete definition always overrides an abstract definition.  Second, for
definitions $M$ and $M$' which are both concrete or both abstract, $M$
overrides $M'$ if $M$ appears in a class that precedes (in the
linearization of $C$) the class in which $M'$ is defined.

It is an error if a template directly defines two matching members. It
is also an error if a template contains two members (directly defined
or inherited) with the same name and the same erased type (\sref{sec:erasure}).
Finally, a template is not allowed to contain two methods (directly
defined or inherited) with the same name which both define default arguments.


\comment{
The type of a member $m$ is determined as follows: If $m$ is defined
in $\stats$, then its type is the type as given in the member's
declaration or definition. Otherwise, if $m$ is inherited from the
base class ~\lstinline@$B$[$T_1$, $\ldots$. $T_n$]@, $B$'s class declaration has formal
parameters ~\lstinline@[$a_1 \commadots a_n$]@, and $M$'s type in $B$ is $U$, then
$M$'s type in $C$ is ~\lstinline@$U$[$a_1$ := $T_1 \commadots a_n$ := $T_n$]@.

\ifqualified{
Members of templates have internally qualified names $Q\qex x$ where
$x$ is a simple name and $Q$ is either the empty name $\epsilon$, or
is a qualified name referencing the module or class that first
introduces the member. A basic declaration or definition of $x$ in a
module or class $M$ introduces a member with the following qualified
name:
\begin{enumerate}
\item
If the binding is labeled with an ~\lstinline@override $Q$@\notyet{Override
  with qualifier} modifier,
where $Q$ is a fully qualified name of a base class of $M$, then the
qualified name is the qualified expansion (\sref{sec:names}) of $x$ in
$Q$.
\item
If the binding is labeled with an \code{override} modifier without a
base class name, then the qualified name is the qualified expansion
of $x$ in $M$'s least proper supertype (\sref{sec:templates}).
\item
An implicit \code{override} modifier is added and case (2) also
applies if $M$'s least proper supertype contains an abstract member
with simple name $x$.
\item
If no \code{override} modifier is given or implied, then if $M$ is
labeled \code{qualified}, the qualified name is $M\qex x$. If $M$ is
not labeled \code{qualified}, the qualified name is $\epsilon\qex x$.
\end{enumerate}
}
}

\example Consider the trait definitions:

\begin{lstlisting}
trait A { def f: Int }
trait B extends A { def f: Int = 1 ; def g: Int = 2 ; def h: Int = 3 }
trait C extends A { override def f: Int = 4 ; def g: Int }
trait D extends B with C { def h: Int }
\end{lstlisting}

Then trait \code{D} has a directly defined abstract member \code{h}. It
inherits member \code{f} from trait \code{C} and member \code{g} from
trait \code{B}.

\subsection{Overriding}
\label{sec:overriding}

\todo{Explain that classes cannot override each other}

A member $M$ of class $C$ that matches (\sref{sec:members}) 
a non-private member $M'$ of a
base class of $C$ is said to {\em override} that member.  In this case
the binding of the overriding member $M$ must subsume
(\sref{sec:conformance}) the binding of the overridden member $M'$.
Furthermore, the following restrictions on modifiers apply to $M$ and
$M'$:
\begin{itemize}
\item
$M'$ must not be labeled \code{final}.
\item
$M$ must not be \code{private} (\sref{sec:modifiers}).
\item
If $M$ is labeled ~\lstinline@private[$C$]@~ for some enclosing class or package $C$,
then $M'$ must be labeled ~\lstinline@private[$C'$]@~ for some class or package $C'$ where
$C'$ equals $C$ or $C'$ is contained in $C$. \todo{check whether this is accurate}
\item
If $M$ is labeled \code{protected}, then $M'$ must also be
labeled \code{protected}.
\item
If $M'$ is not an abstract member, then
$M$ must be labeled \code{override}.
Furthermore, one of two possibilities must hold:
\begin{itemize}
\item either $M$ is defined in a subclass of the class where is $M'$ is defined, 
\item or both $M$ and $M'$ override a third member $M''$ which is defined
      in a base class of both the classes containing $M$ and $M'$ 
\end{itemize}
\item
If $M'$ is incomplete (\sref{sec:modifiers}) in $C$ then $M$ must be
labeled \code{abstract override}.
\item 
If $M$ and $M'$ are both concrete value definitions, then either none
of them is marked \code{lazy} or both must be marked \code{lazy}.
\end{itemize}
A special rule concerns parameterless methods. If a paramterless
method defined as \lstinline@def $f$: $T$ = ...@ or 
\lstinline@def $f$ = ...@ overrides a method of
type $()T'$ which has an empty parameter list, then $f$ is also
assumed to have an empty parameter list.

Another restriction applies to abstract type members: An abstract type
member with a volatile type (\sref{sec:volatile-types}) as its upper
bound may not override an abstract type member which does not have a
volatile upper bound.

An overriding method inherits all default arguments from the definition
in the superclass. By specifying default arguments in the overriding method
it is possible to add new defaults (if the corresponding parameter in the
superclass does not have a default) or to override the defaults of the
superclass (otherwise).

\example\label{ex:compound-a}
Consider the definitions:
\begin{lstlisting}
trait Root { type T <: Root }
trait A extends Root { type T <: A }
trait B extends Root { type T <: B }
trait C extends A with B 
\end{lstlisting}
Then the class definition \code{C} is not well-formed because the
binding of \code{T} in \code{C} is
~\lstinline@type T <: B@,
which fails to subsume the binding ~\lstinline@type T <: A@~ of \code{T}
in type \code{A}. The problem can be solved by adding an overriding 
definition of type \code{T} in class \code{C}:
\begin{lstlisting}
class C extends A with B { type T <: C }
\end{lstlisting}

\subsection{Inheritance Closure}\label{sec:inheritance-closure}

Let $C$ be a class type. The {\em inheritance closure} of $C$ is the
smallest set $\SS$ of types such that
\begin{itemize}
\item
If $T$ is in $\SS$, then every type $T'$ which forms syntactically
a part of $T$ is also in $\SS$.
\item
If $T$ is a class type in $\SS$, then all parents (\sref{sec:templates})
of $T$ are also in $\SS$.
\end{itemize}
It is a static error if the inheritance closure of a class type
consists of an infinite number of types. (This restriction is
necessary to make subtyping decidable
\cite{kennedy-pierce:decidable}).

\subsection{Early Definitions}\label{sec:early-defs}

\syntax\begin{lstlisting}
  EarlyDefs         ::= `{' [EarlyDef {semi EarlyDef}] `}' `with'
  EarlyDef          ::=  {Annotation} {Modifier} PatVarDef
\end{lstlisting}

A template may start with an {\em early field definition} clause,
which serves to define certain field values before the supertype
constructor is called. In a template
\begin{lstlisting}
{ val $p_1$: $T_1$ = $e_1$
  ...
  val $p_n$: $T_n$ = $e_n$
} with $sc$ with $mt_1$ with $mt_n$ {$\stats\,$}
\end{lstlisting}
The initial pattern definitions of $p_1 \commadots p_n$ are called
{\em early definitions}. They define fields 
which form part of the template. Every early definition must define
at least one variable. 

An early definition is type-checked and evaluated in the scope which
is in effect just before the template being defined, augmented by any
type parameters of the enclosing class and by any early definitions
preceding the one being defined. In particular, any reference to
\lstinline@this@ in the right-hand side of an early definition refers
to the identity of \lstinline@this@ just outside the template. Consequently, it
is impossible that an early definition refers to the object being
constructed by the template, or refers to one of its fields and
methods, except for any other preceding early definition in the same
section. Furthermore, references to preceding early definitions
always refer to the value that's defined there, and do not take into account
overriding definitions. In other words, a block of early definitions
is evaluated exactly as if it was a local bock containing a number of value
definitions.
 

Early definitions are evaluated in the order they are being defined
before the superclass constructor of the template is called.

\example Early definitions are particularly useful for
traits, which do not have normal constructor parameters. Example:
\begin{lstlisting}
trait Greeting {
  val name: String
  val msg = "How are you, "+name
}
class C extends {
  val name = "Bob"
} with Greeting {
  println(msg)
}
\end{lstlisting}
In the code above, the field \code{name} is initialized before the
constructor of \code{Greeting} is called. Therefore, field \lstinline@msg@ in
class \code{Greeting} is properly initialized to \code{"How are you, Bob"}.

If \code{name} had been initialized instead in \code{C}'s normal class
body, it would be initialized after the constructor of
\code{Greeting}. In that case, \lstinline@msg@ would be initialized to
\code{"How are you, <null>"}.
  
 
\section{Modifiers}
\label{sec:modifiers}

\syntax\begin{lstlisting}
  Modifier          ::=  LocalModifier 
                      |  AccessModifier
                      |  `override'
  LocalModifier     ::=  `abstract'
                      |  `final'
                      |  `sealed'
                      |  `implicit'
                      |  `lazy'
  AccessModifier    ::=  (`private' | `protected') [AccessQualifier]
  AccessQualifier   ::=  `[' (id | `this') `]'
\end{lstlisting}

Member definitions may be preceded by modifiers which affect the
accessibility and usage of the identifiers bound by them.  If several
modifiers are given, their order does not matter, but the same
modifier may not occur more than once.  Modifiers preceding a repeated
definition apply to all constituent definitions.  The rules governing
the validity and meaning of a modifier are as follows.
\begin{itemize}
\item
The \code{private} modifier can be used with any definition or
declaration in a template.  Such members can be accessed only from
within the directly enclosing template and its companion module or 
companion class (\sref{def:companion}).  They
are not inherited by subclasses and they may not override definitions
in parent classes.

The modifier can be {\em qualified} with an identifier $C$ (e.g.
~\lstinline@private[$C$]@) that must denote a class or package
enclosing the definition.  Members labeled with such a modifier are
accessible respectively only from code inside the package $C$ or only
from code inside the class $C$ and its companion module
(\sref{sec:object-defs}). Such members are also inherited only from
templates inside $C$.

An different form of qualification is \code{private[this]}. A member
$M$ marked with this modifier can be accessed only from within
the object in which it is defined. That is, a selection $p.M$ is only
legal if the prefix is \code{this} or \lstinline@$O$.this@, for some
class $O$ enclosing the reference. In addition, the restrictions for
unqualified \code{private} apply.

Members marked private without a qualifier are called {\em
class-private}, whereas members labeled with \lstinline@private[this]@
are called {\em object-private}.  A member {\em is private} if it is
either class-private or object-private, but not if it is marked
\lstinline@private[$C$]@ where $C$ is an identifier; in the latter
case the member is called {\em qualified private}.

Class-private or object-private members may not be abstract, and may
not have \code{protected} or \code{override} modifiers.
\item
The \code{protected} modifier applies to class member definitions.
Protected members of a class can be accessed from within
\begin{itemize}
\item the template of the defining class, 
\item all templates that have the defining class as a base class,
\item the companion module of any of those classes.
\end{itemize}
A \code{protected} modifier can be qualified with an
identifier $C$ (e.g.  ~\lstinline@protected[$C$]@) that must denote a
class or package enclosing the definition.  Members labeled with such
a modifier are also accessible respectively from all code inside the
package $C$ or from all code inside the class $C$ and its companion
module (\sref{sec:object-defs}).

A protected identifier $x$ may be used as a member name in a selection
\lstinline@$r$.$x$@ only if one of the following applies:
\begin{itemize}
\item The access is within the template defining the member, or, if
  a qualification $C$ is given, inside the package $C$,
  or the class $C$, or its companion module, or
\item $r$ is one of the reserved words \code{this} and
  \code{super}, or
\item $r$'s type conforms to a type-instance of the
  class which contains the access.
\end{itemize}

A different form of qualification is \code{protected[this]}. A member
$M$ marked with this modifier can be accessed only from within
the object in which it is defined. That is, a selection $p.M$ is only
legal if the prefix is \code{this} or \lstinline@$O$.this@, for some
class $O$ enclosing the reference. In addition, the restrictions for
unqualified \code{protected} apply.

\item
The \code{override} modifier applies to class member definitions or declarations.  It
is mandatory for member definitions or declarations that override some other concrete
member definition in a parent class. If an \code{override}
modifier is given, there must be at least one overridden member
definition or declaration (either concrete or abstract).  
\item
The \code{override} modifier has an additional significance when
combined with the \code{abstract} modifier.  That modifier combination
is only allowed for value members of traits.  

We call a member $M$ of a template {\em incomplete} if it is either
abstract (i.e.\ defined by a declaration), or it is labeled
\code{abstract} and \code{override} and 
every member overridden by $M$ is again incomplete. 

Note that the \code{abstract override} modifier combination does not
influence the concept whether a member is concrete or abstract. A
member is {\em abstract} if only a declaration is given for it; it is
{\em concrete} if a full definition is given.
\item
The \code{abstract} modifier is used in class definitions. It is
redundant for traits, and mandatory for all other classes which have
incomplete members.  Abstract classes cannot be instantiated
(\sref{sec:inst-creation}) with a constructor invocation unless
followed by mixins and/or a refinement which override all
incomplete members of the class. Only abstract classes and traits can have
abstract term members.

The \code{abstract} modifier can also be used in conjunction with
\code{override} for class member definitions. In that case the
previous discussion applies.
\item
The \code{final} modifier applies to class member definitions and to
class definitions. A \code{final} class member definition may not be
overridden in subclasses. A \code{final} class may not be inherited by
a template. \code{final} is redundant for object definitions.  Members
of final classes or objects are implicitly also final, so the
\code{final} modifier is generally redundant for them, too. Note, however, that
constant value definitions (\sref{sec:valdef}) do require an explicit \code{final} modifier,
even if they are defined in a final class or object.
\code{final} may
not be applied to incomplete members, and it may not be combined in one
modifier list with \code{sealed}.
\item
The \code{sealed} modifier applies to class definitions. A
\code{sealed} class may not be directly inherited, except if the inheriting 
template is defined in the same source file as the inherited class.
However, subclasses of a sealed class can be inherited anywhere.
\item
The \code{lazy} modifier applies to value definitions. A \code{lazy}
value is initialized the first time it is accessed (which might never
happen at all). Attempting to access a lazy value during its
initialization might lead to looping behavior. If an exception is
thrown during initialization, the value is considered uninitialized,
and a later access will retry to evaluate its right hand side.
\end{itemize}

\example The following code illustrates the use of qualified private:
\begin{lstlisting}
package outerpkg.innerpkg
class Outer {
  class Inner {
    private[Outer] def f()
    private[innerpkg] def g()
    private[outerpkg] def h()
  }
}
\end{lstlisting}
Here, accesses to the method \lstinline@f@ can appear anywhere within
\lstinline@OuterClass@, but not outside it. Accesses to method
\lstinline@g@ can appear anywhere within the package
\lstinline@outerpkg.innerpkg@, as would be the case for
package-private methods in Java. Finally, accesses to method
\lstinline@h@ can appear anywhere within package \lstinline@outerpkg@,
including packages contained in it. 

\example A useful idiom to prevent clients of a class from
constructing new instances of that class is to declare the class
\code{abstract} and \code{sealed}:

\begin{lstlisting}
object m {
  abstract sealed class C (x: Int) {
    def nextC = new C(x + 1) {}
  }
  val empty = new C(0) {}
}
\end{lstlisting}
For instance, in the code above clients can create instances of class
\lstinline@m.C@ only by calling the \code{nextC} method of an existing \lstinline@m.C@
object; it is not possible for clients to create objects of class
\lstinline@m.C@ directly. Indeed the following two lines are both in error:

\begin{lstlisting}
  new m.C(0)    // **** error: C is abstract, so it cannot be instantiated.
  new m.C(0) {} // **** error: illegal inheritance from sealed class.
\end{lstlisting}

A similar access restriction can be achieved by marking the primary
constructor \code{private} (see \ref{ex:private-constr}).

\section{Class Definitions}
\label{sec:class-defs}

\syntax\begin{lstlisting} 
  TmplDef           ::=  `class' ClassDef 
  ClassDef          ::=  id [TypeParamClause] {Annotation} 
                         [AccessModifier] ClassParamClauses ClassTemplateOpt 
  ClassParamClauses ::=  {ClassParamClause} 
                         [[nl] `(' implicit ClassParams `)']
  ClassParamClause  ::=  [nl] `(' [ClassParams] ')'
  ClassParams       ::=  ClassParam {`,' ClassParam}
  ClassParam        ::=  {Annotation} [{Modifier} (`val' | `var')] 
                         id [`:' ParamType] [`=' Expr]
  ClassTemplateOpt  ::=  `extends' ClassTemplate | [[`extends'] TemplateBody]
\end{lstlisting}

The most general form of class definition is 
\begin{lstlisting}
class $c$[$\tps\,$] $as$ $m$($\ps_1$)$\ldots$($\ps_n$) extends $t$    $\gap(n \geq 0)$.
\end{lstlisting}
Here,
\begin{itemize}
\item[]
$c$ is the name of the class to be defined.
\item[] $\tps$ is a non-empty list of type parameters of the class
being defined.  The scope of a type parameter is the whole class
definition including the type parameter section itself.  It is
illegal to define two type parameters with the same name.  The type
parameter section \lstinline@[$\tps\,$]@ may be omitted. A class with a type
parameter section is called {\em polymorphic}, otherwise it is called
{\em monomorphic}.
\item[] $as$ is a possibly empty sequence of annotations
  (\sref{sec:annotations}). If any annotations are given, 
they apply to the primary constructor of the class.
\item[] $m$ is an access modifier (\sref{sec:modifiers}) such as
\code{private} or \code{protected}, possibly with a qualification.  If
such an access modifier is given it applies to the primary constructor
to the class.
\item[] 
$(\ps_1)\ldots(\ps_n)$ are formal value parameter clauses for the {\em primary
constructor} of the class. The scope of a formal value parameter includes
all subsequent parameter sections and the template $t$. However, a formal value
parameter may not form
part of the types of any of the parent classes or members of the class
template $t$.
It is illegal to define two formal value parameters with the same name.
If no formal parameter sections are given, 
an empty parameter section \lstinline@()@ is assumed.

If a formal parameter declaration $x: T$ is preceded by a \code{val}
or \code{var} keyword, an accessor (getter) definition
(\sref{sec:vardef}) for this parameter is implicitly added to the
class. The getter introduces a value member $x$ of class $c$ that is
defined as an alias of the parameter. If the introducing keyword is
\code{var}, a setter accessor \lstinline@$x$_=@ (\sref{sec:vardef}) is also
implicitly added to the class. In invocation of that setter \lstinline@$x$_=($e$)@
changes the value of the parameter to the result of evaluating $e$.
The formal parameter declaration may contain modifiers, which then
carry over to the accessor definition(s). A formal parameter prefixed
by \code{val} or \code{var} may not at the same time be a call-by-name
parameter (\sref{sec:by-name-params}).
\item[] 
$t$ is a
template (\sref{sec:templates}) of the form
\begin{lstlisting}
$sc$ with $mt_1$ with $\ldots$ with $mt_m$ { $\stats$ }   $\gap(m \geq 0)$
\end{lstlisting}
which defines the base classes, behavior and initial state of objects of
the class. The extends clause ~\lstinline@extends $sc$ with $mt_1$ with $\ldots$ with $mt_m$@~ 
can be omitted, in which case
~\lstinline@extends scala.AnyRef@~ is assumed.  The class body
~\lstinline@{$\stats\,$}@~ may also be omitted, in which case the empty body
\lstinline@{}@ is assumed.
\end{itemize}
This class definition defines a type \lstinline@$c$[$\tps\,$]@ and a constructor
which when applied to parameters conforming to types $\ps$
initializes instances of type \lstinline@$c$[$\tps\,$]@ by evaluating the template
$t$.

\example The following example illustrates \code{val} and \code{var}
parameters of a class \code{C}:
\begin{lstlisting}
class C(x: Int, val y: String, var z: List[String])
val c = new C(1, "abc", List())
c.z = c.y :: c.z
\end{lstlisting}

\example\label{ex:private-constr}
The following class can be created only from its companion
module.
\begin{lstlisting}
object Sensitive {
  def makeSensitive(credentials: Certificate): Sensitive = 
    if (credentials == Admin) new Sensitive() 
    else throw new SecurityViolationException
}
class Sensitive private () {
  ...
}
\end{lstlisting}

\comment{
For any index $i$ let $\csig_i$ be a class signature consisting of a class
name and optional type parameter and value parameter sections. Let $ct$
be a class template.
Then a class definition 
~\lstinline@class $\csig_1 \commadots \csig_n$ $ct$@~ 
is a shorthand for the sequence of class definitions
~\lstinline@class $\csig_1$ $ct$; ...; class $\csig_n$ $ct$@.
A class definition 
~\lstinline@class $\csig_1 \commadots \csig_n: T$ $ct$@~ 
is a shorthand for the sequence of class definitions
~\lstinline@class $\csig_1: T$ $ct$; ...; class $\csig_n: T$ $ct$@.
}

\subsection{Constructor Definitions}\label{sec:constr-defs}

\syntax\begin{lstlisting}
  FunDef         ::= `this' ParamClause ParamClauses 
                     (`=' ConstrExpr | [nl] ConstrBlock)
  ConstrExpr     ::= SelfInvocation
                  |  ConstrBlock
  ConstrBlock    ::= `{' SelfInvocation {semi BlockStat} `}'
  SelfInvocation ::= `this' ArgumentExprs {ArgumentExprs}
\end{lstlisting}

A class may have additional constructors besides the primary
constructor.  These are defined by constructor definitions of the form
~\lstinline@def this($\ps_1$)$\ldots$($\ps_n$) = $e$@.  Such a
definition introduces an additional constructor for the enclosing
class, with parameters as given in the formal parameter lists $\ps_1
\commadots \ps_n$, and whose evaluation is defined by the constructor
expression $e$.  The scope of each formal parameter is the subsequent
parameter sections and the constructor
expression $e$.  A constructor expression is either a self constructor
invocation \lstinline@this($\args_1$)$\ldots$($\args_n$)@ or a block
which begins with a self constructor invocation. The self constructor
invocation must construct a generic instance of the class. I.e.\ if the
class in question has name $C$ and type parameters
\lstinline@[$\tps\,$]@, then a self constructor invocation must
generate an instance of \lstinline@$C$[$\tps\,$]@; it is not permitted
to instantiate formal type parameters.

The signature and the self constructor invocation of a constructor
definition are type-checked and evaluated in the scope which is in
effect at the point of the enclosing class definition, augmented by
any type parameters of the enclosing class and by any early
definitions (\sref{sec:early-defs}) of the enclosing template.
The rest of the
constructor expression is type-checked and evaluated as a function
body in the current class.
  
If there are auxiliary constructors of a class $C$, they form together
with $C$'s primary constructor (\sref{sec:class-defs})
an overloaded constructor
definition. The usual rules for overloading resolution
(\sref{sec:overloading-resolution}) apply for constructor invocations of $C$,
including for the self constructor invocations in the constructor
expressions themselves. However, unlike other methods, constructors
are never inherited.  To prevent infinite cycles of constructor
invocations, there is the restriction that every self constructor
invocation must refer to a constructor definition which precedes it
(i.e.\ it must refer to either a preceding auxiliary constructor or the
primary constructor of the class).  

\example Consider the class definition

\begin{lstlisting}
class LinkedList[A]() {
  var head = _ 
  var tail = null 
  def isEmpty = tail != null   
  def this(head: A) = { this(); this.head = head }
  def this(head: A, tail: List[A]) = { this(head); this.tail = tail }
}
\end{lstlisting}
This defines a class \code{LinkedList} with three constructors.  The
second constructor constructs an singleton list, while the
third one constructs a list with a given head and tail.

Case Classes
------------

\label{sec:case-classes}

\syntax\begin{lstlisting} 
  TmplDef  ::=  `case' `class' ClassDef
\end{lstlisting}

If a class definition is prefixed with \code{case}, the class is said
to be a {\em case class}.  

The formal parameters in the first parameter section of a case class
are called {\em elements}; they are treated
specially. First, the value of such a parameter can be extracted as a
field of a constructor pattern. Second, a \code{val} prefix is
implicitly added to such a parameter, unless the parameter carries
already a \code{val} or \code{var} modifier. Hence, an accessor
definition for the parameter is generated (\sref{sec:class-defs}).

A case class definition of ~\lstinline@$c$[$\tps\,$]($\ps_1\,$)$\ldots$($\ps_n$)@~ with type
parameters $\tps$ and value parameters $\ps$ implicitly
generates an extractor object (\sref{sec:extractor-patterns}) which is
defined as follows:
\begin{lstlisting}
  object $c$ {
    def apply[$\tps\,$]($\ps_1\,$)$\ldots$($\ps_n$): $c$[$\tps\,$] = new $c$[$\Ts\,$]($\xs_1\,$)$\ldots$($\xs_n$)
    def unapply[$\tps\,$]($x$: $c$[$\tps\,$]) =
      if (x eq null) scala.None
      else scala.Some($x.\xs_{11}\commadots x.\xs_{1k}$)
  }
\end{lstlisting}
Here,
 $\Ts$ stands for the vector of types defined in the type
parameter section $\tps$,
each $\xs_i$ denotes the parameter names of the parameter
section $\ps_i$, and
$\xs_{11}\commadots \xs_{1k}$ denote the names of all parameters
in the first parameter section $\xs_1$.
If a type parameter section is missing in the
class, it is also missing in the \lstinline@apply@ and
\lstinline@unapply@ methods.
The definition of \lstinline@apply@ is omitted if class $c$ is
\lstinline@abstract@.

If the case class definition contains an empty value parameter list, the
\lstinline@unapply@ method returns a \lstinline@Boolean@ instead of an \lstinline@Option@ type and
is defined as follows:
\begin{lstlisting}
    def unapply[$\tps\,$]($x$: $c$[$\tps\,$]) = x ne null
\end{lstlisting}
The name of the \lstinline@unapply@ method is changed to \lstinline@unapplySeq@ if the first
parameter section $\ps_1$ of $c$ ends in a repeated parameter of (\sref{sec:repeated-params}). 
If a companion object $c$ exists already, no new object is created,
but the \lstinline@apply@ and \lstinline@unapply@ methods are added to the existing
object instead.

A method named \code{copy} is implicitly added to every case class unless the
class already has a member (directly defined or inherited) with that name. The
method is defined as follows:
\begin{lstlisting}
  def copy[$\tps\,$]($\ps'_1\,$)$\ldots$($\ps'_n$): $c$[$\tps\,$] = new $c$[$\Ts\,$]($\xs_1\,$)$\ldots$($\xs_n$)
\end{lstlisting}
Again, $\Ts$ stands for the vector of types defined in the type parameter section $\tps$
and each $\xs_i$ denotes the parameter names of the parameter section $\ps'_i$. Every value
parameter $\ps'_{i,j}$ of the \code{copy} method has the form \lstinline@$x_{i,j}$:$T_{i,j}$=this.$x_{i,j}$@,
where $x_{i,j}$ and $T_{i,j}$ refer to the name and type of the corresponding class parameter
$\ps_{i,j}$.

Every case class implicitly overrides some method definitions of class
\lstinline@scala.AnyRef@ (\sref{sec:cls-object}) unless a definition of the same
method is already given in the case class itself or a concrete
definition of the same method is given in some base class of the case
class different from \code{AnyRef}. In particular:
\begin{itemize}
\item[] Method ~\lstinline@equals: (Any)Boolean@~ is structural equality, where two
instances are equal if they both belong to the case class in question and they
have equal (with respect to \code{equals}) constructor arguments.
\item[] 
Method ~\lstinline@hashCode: Int@ computes a hash-code. If the hashCode methods
of the data structure members map equal (with respect to equals)
values to equal hash-codes, then the case class hashCode method does
too.
\item[] Method ~\lstinline@toString: String@~ returns a string representation which
contains the name of the class and its elements.
\end{itemize}

\example Here is the definition of abstract syntax for lambda
calculus:

\begin{lstlisting}
class Expr 
case class Var   (x: String)          extends Expr
case class Apply (f: Expr, e: Expr)   extends Expr
case class Lambda(x: String, e: Expr) extends Expr 
\end{lstlisting}
This defines a class \code{Expr} with case classes
\code{Var}, \code{Apply} and \code{Lambda}. A call-by-value evaluator for lambda
expressions could then be written as follows.

\begin{lstlisting}
type Env = String => Value 
case class Value(e: Expr, env: Env) 

def eval(e: Expr, env: Env): Value = e match {
  case Var (x) =>
    env(x)
  case Apply(f, g) =>
    val Value(Lambda (x, e1), env1) = eval(f, env) 
    val v = eval(g, env) 
    eval (e1, (y => if (y == x) v else env1(y)))
  case Lambda(_, _) =>
    Value(e, env)
}
\end{lstlisting}

It is possible to define further case classes that extend type
\code{Expr} in other parts of the program, for instance
\begin{lstlisting}
case class Number(x: Int) extends Expr 
\end{lstlisting}

This form of extensibility can be excluded by declaring the base class
\code{Expr} \code{sealed}; in this case, all classes that
directly extend \code{Expr} must be in the same source file as
\code{Expr}.

\subsection{Traits}
\label{sec:traits}

\syntax\begin{lstlisting}
  TmplDef          ::=  `trait' TraitDef
  TraitDef         ::=  id [TypeParamClause] TraitTemplateOpt
  TraitTemplateOpt ::=  `extends' TraitTemplate | [[`extends'] TemplateBody]
\end{lstlisting}

A trait is a class that is meant to be added to some other class
as a mixin. Unlike normal classes, traits cannot have
constructor parameters. Furthermore, no constructor arguments are
passed to the superclass of the trait. This is not necessary as traits are
initialized after the superclass is initialized.

Assume a trait $D$ defines some aspect of an instance $x$ of
type $C$ (i.e.\ $D$ is a base class of $C$). Then the {\em actual
supertype} of $D$ in $x$ is the compound type consisting of all the
base classes in $\lin C$ that succeed $D$.  The actual supertype gives
the context for resolving a \code{super} reference in a trait
(\sref{sec:this-super}). Note that the actual supertype depends 
on the type to which the trait is added in a mixin composition; it is not
statically known at the time the trait is defined.

If $D$ is not a trait, then its actual supertype is simply its
least proper supertype (which is statically known).

\example\label{ex:comparable} The following trait defines the property
of being comparable to objects of some type. It contains an abstract
method \lstinline@<@ and default implementations of the other
comparison operators \lstinline@<=@, \lstinline@>@, and
\lstinline@>=@. 

\begin{lstlisting}
trait Comparable[T <: Comparable[T]] { self: T =>
  def < (that: T): Boolean
  def <=(that: T): Boolean = this < that || this == that
  def > (that: T): Boolean = that < this 
  def >=(that: T): Boolean = that <= this
}
\end{lstlisting}

\example Consider an abstract class \code{Table} that implements maps
from a type of keys \code{A} to a type of values \code{B}. The class
has a method \code{set} to enter a new key / value pair into the table,
and a method \code{get} that returns an optional value matching a
given key. Finally, there is a method \code{apply} which is like
\code{get}, except that it returns a given default value if the table
is undefined for the given key. This class is implemented as follows.
\begin{lstlisting}
abstract class Table[A, B](defaultValue: B) {
  def get(key: A): Option[B]
  def set(key: A, value: B)
  def apply(key: A) = get(key) match {
    case Some(value) => value
    case None => defaultValue
  }
}
\end{lstlisting}
Here is a concrete implementation of the \code{Table} class.
\begin{lstlisting}
class ListTable[A, B](defaultValue: B) extends Table[A, B](defaultValue) {
  private var elems: List[(A, B)]
  def get(key: A) = elems.find(._1.==(key)).map(._2)
  def set(key: A, value: B) = { elems = (key, value) :: elems }
}
\end{lstlisting}
Here is a trait that prevents concurrent access to the
\code{get} and \code{set} operations of its parent class:
\begin{lstlisting}
trait SynchronizedTable[A, B] extends Table[A, B] {
  abstract override def get(key: A): B = 
    synchronized { super.get(key) }
  abstract override def set((key: A, value: B) = 
    synchronized { super.set(key, value) }
}

\end{lstlisting}
Note that \code{SynchronizedTable} does not pass an argument to
its superclass, \code{Table}, even  though \code{Table} is defined with a
formal parameter. Note also that the \code{super} calls
in \code{SynchronizedTable}'s \code{get} and \code{set} methods
statically refer to abstract methods in class \code{Table}. This is
legal, as long as the calling method is labeled 
\code{abstract override} (\sref{sec:modifiers}).

Finally, the following mixin composition creates a synchronized list table
with strings as keys and integers as values and with a default value \code{0}:
\begin{lstlisting}
object MyTable extends ListTable[String, Int](0) with SynchronizedTable
\end{lstlisting}
The object \code{MyTable} inherits its \code{get} and \code{set}
method from \code{SynchronizedTable}.  The \code{super} calls in these
methods are re-bound to refer to the corresponding implementations in
\code{ListTable}, which is the actual supertype of \code{SynchronizedTable} 
in \code{MyTable}. 

\section{Object Definitions}
\label{sec:object-defs}
\label{def:companion}

\syntax\begin{lstlisting}
  ObjectDef       ::=  id ClassTemplate
\end{lstlisting}

An object definition defines a single object of a new class. Its 
most general form is
~\lstinline@object $m$ extends $t$@. Here,
$m$ is the name of the object to be defined, and 
$t$ is a template (\sref{sec:templates}) of the form
\begin{lstlisting}
$sc$ with $mt_1$ with $\ldots$ with $mt_n$ { $\stats$ }
\end{lstlisting}
which defines the base classes, behavior and initial state of $m$.
The extends clause ~\lstinline@extends $sc$ with $mt_1$ with $\ldots$ with $mt_n$@~ 
can be omitted, in which case
~\lstinline@extends scala.AnyRef@~ is assumed.  The class body
~\lstinline@{$\stats\,$}@~ may also be omitted, in which case the empty body
\lstinline@{}@ is assumed.

The object definition defines a single object (or: {\em module})
conforming to the template $t$.  It is roughly equivalent to the
following definition of a lazy value:
\begin{lstlisting}
lazy val $m$ = new $sc$ with $mt_1$ with $\ldots$ with $mt_n$ { this: $m.type$ => $\stats$ }
\end{lstlisting}
Note that the value defined by an object definition is instantiated
lazily.  The ~\lstinline@new $m\Dollar$cls@~ constructor is evaluated
not at the point of the object definition, but is instead evaluated
the first time $m$ is dereferenced during execution of the program
(which might be never at all). An attempt to dereference $m$ again in
the course of evaluation of the constructor leads to a infinite loop
or run-time error.  
Other threads trying to dereference $m$ while the
constructor is being evaluated block until evaluation is complete.

The expansion given above is not accurate for top-level objects. It
cannot be because variable and method definition cannot appear on the
top-level outside of a package object (\sref{sec:pkg-obj}).  Instead,
top-level objects are translated to static fields.

\example
Classes in Scala do not have static members; however, an equivalent
effect can be achieved by an accompanying object definition
E.g.
\begin{lstlisting}
abstract class Point {
  val x: Double 
  val y: Double 
  def isOrigin = (x == 0.0 && y == 0.0) 
}
object Point {
  val origin = new Point() { val x = 0.0; val y = 0.0 }
}
\end{lstlisting}
This defines a class \code{Point} and an object \code{Point} which
contains \code{origin} as a member.  Note that the double use of the
name \code{Point} is legal, since the class definition defines the
name \code{Point} in the type name space, whereas the object
definition defines a name in the term namespace. 

This technique is applied by the Scala compiler when interpreting a
Java class with static members. Such a class $C$ is conceptually seen
as a pair of a Scala class that contains all instance members of $C$
and a Scala object that contains all static members of $C$.

Generally, a {\em companion module} of a class is an object which has
the same name as the class and is defined in the same scope and
compilation unit. Conversely, the class is called the {\em companion class}
of the module.


\comment{
Let $ct$ be a class template. 
Then an object definition 
~\lstinline@object $x_1 \commadots x_n$ $ct$@~ 
is a shorthand for the sequence of object definitions
~\lstinline@object $x_1$ $ct$; ...; object $x_n$ $ct$@.
An object definition 
~\lstinline@object $x_1 \commadots x_n: T$ $ct$@~ 
is a shorthand for the sequence of object definitions
~\lstinline@object $x_1: T$ $ct$; ...; object $x_n: T$ $ct$@.
}

\comment{
\example Here's an outline of a module definition for a file system.

\begin{lstlisting}
object FileSystem {
  private type FileDirectory 
  private val dir: FileDirectory

  trait File {
    def read(xs: Array[Byte])
    def close: Unit
  }

  private class FileHandle extends File { $\ldots$ }

  def open(name: String): File = $\ldots$
}
\end{lstlisting}
}

