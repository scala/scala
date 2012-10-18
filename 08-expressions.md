Expressions
===========

\syntax\begin{lstlisting}
  Expr              ::=  (Bindings | id | `_') `=>' Expr
                      |  Expr1
  Expr1             ::=  `if' `(' Expr `)' {nl} Expr [[semi] else Expr]
                      |  `while' `(' Expr `)' {nl} Expr
                      |  `try' `{' Block `}' [`catch'  `{' CaseClauses `}'] 
                         [`finally' Expr]
                      |  `do' Expr [semi] `while' `(' Expr ')'
                      |  `for' (`(' Enumerators `)' | `{' Enumerators `}') 
                         {nl} [`yield'] Expr
                      |  `throw' Expr
                      |  `return' [Expr]
                      |  [SimpleExpr `.'] id `=' Expr
                      |  SimpleExpr1 ArgumentExprs `=' Expr
                      |  PostfixExpr
                      |  PostfixExpr Ascription
                      |  PostfixExpr `match' `{' CaseClauses `}'
  PostfixExpr       ::=  InfixExpr [id [nl]]
  InfixExpr         ::=  PrefixExpr
                      |  InfixExpr id [nl] InfixExpr
  PrefixExpr        ::=  [`-' | `+' | `~' | `!'] SimpleExpr 
  SimpleExpr        ::=  `new' (ClassTemplate | TemplateBody)
                      |  BlockExpr
                      |  SimpleExpr1 [`_']
  SimpleExpr1       ::=  Literal
                      |  Path
                      |  `_'
                      |  `(' [Exprs] `)'
                      |  SimpleExpr `.' id s
                      |  SimpleExpr TypeArgs
                      |  SimpleExpr1 ArgumentExprs
                      |  XmlExpr
  Exprs             ::=  Expr {`,' Expr}
  BlockExpr         ::=  `{' CaseClauses `}'
                      |  `{' Block `}'
  Block             ::=  {BlockStat semi} [ResultExpr]
  ResultExpr        ::=  Expr1
                      |  (Bindings | ([`implicit'] id | `_') `:' CompoundType) `=>' Block
  Ascription        ::=  `:' InfixType
                      |  `:' Annotation {Annotation} 
                      |  `:' `_' `*'
\end{lstlisting}

Expressions are composed of operators and operands. Expression forms are
discussed subsequently in decreasing order of precedence. 

Expression Typing
-----------------

\label{sec:expr-typing}

The typing of expressions is often relative to some {\em expected
type} (which might be undefined).  
When we write ``expression $e$ is expected to conform to
type $T$'', we mean: (1) the expected type of $e$ is
$T$, and (2) the type of expression $e$ must conform to
$T$.

The following skolemization rule is applied universally for every
expression: If the type of an expression would be an existential type
$T$, then the type of the expression is assumed instead to be a
skolemization (\sref{sec:existential-types}) of $T$.

Skolemization is reversed by type packing. Assume an expression $e$ of
type $T$ and let $t_1[\tps_1] >: L_1 <: U_1 \commadots t_n[\tps_n] >: L_n <: U_n$ be
all the type variables created by skolemization of some part of $e$ which are free in $T$.
Then the {\em packed type} of $e$ is
\begin{lstlisting}
$T$ forSome { type $t_1[\tps_1] >: L_1 <: U_1$; $\ldots$; type $t_n[\tps_n] >: L_n <: U_n$ }.
\end{lstlisting}

\section{Literals}\label{sec:literal-exprs}

\syntax\begin{lstlisting}
  SimpleExpr    ::=  Literal
\end{lstlisting}

Typing of literals is as described [here](#literals); their
evaluation is immediate.


\section{The {\em Null} Value}

The \code{null} value is of type \lstinline@scala.Null@, and is thus
compatible with every reference type.  It denotes a reference value
which refers to a special ``\lstinline@null@'' object. This object
implements methods in class \lstinline@scala.AnyRef@ as follows:
\begin{itemize}
\item
\lstinline@eq($x\,$)@ and \lstinline@==($x\,$)@ return \code{true} iff the
argument $x$ is also the ``null'' object.
\item
\lstinline@ne($x\,$)@ and \lstinline@!=($x\,$)@ return true iff the 
argument x is not also the ``null'' object.
\item
\lstinline@isInstanceOf[$T\,$]@ always returns \code{false}.
\item
\lstinline@asInstanceOf[$T\,$]@ returns the ``null'' object itself if
$T$ conforms to \lstinline@scala.AnyRef@, and throws a
\lstinline@NullPointerException@ otherwise.
\end{itemize}
A reference to any other member of the ``null'' object causes a
\code{NullPointerException} to be thrown. 

\section{Designators}
\label{sec:designators}

\syntax\begin{lstlisting}
  SimpleExpr  ::=  Path
                |  SimpleExpr `.' id
\end{lstlisting}

A designator refers to a named term. It can be a {\em simple name} or
a {\em selection}. 

A simple name $x$ refers to a value as specified in \sref{sec:names}.
If $x$ is bound by a definition or declaration in an enclosing class
or object $C$, it is taken to be equivalent to the selection
\lstinline@$C$.this.$x$@ where $C$ is taken to refer to the class containing $x$
even if the type name $C$ is shadowed (\sref{sec:names}) at the
occurrence of $x$.

If $r$ is a stable identifier
(\sref{sec:stable-ids}) of type $T$, the selection $r.x$ refers
statically to a term member $m$ of $r$ that is identified in $T$ by
the name $x$. \comment{There might be several such members, in which
case overloading resolution (\sref{overloading-resolution}) is applied
to pick a unique one.}  

For other expressions $e$, $e.x$ is typed as
if it was ~\lstinline@{ val $y$ = $e$; $y$.$x$ }@, for some fresh name
$y$.  

The expected type of a designator's prefix is always undefined.  The
type of a designator is the type $T$ of the entity it refers to, with
the following exception: The type of a path (\sref{sec:paths}) $p$
which occurs in a context where a stable type
(\sref{sec:singleton-types}) is required is the singleton type
\lstinline@$p$.type@.

The contexts where a stable type is required are those that satisfy
one of the following conditions:
\begin{enumerate}
\item
The path $p$ occurs as the prefix of a selection and it does not
designate a constant, or
\item
The expected type $\proto$ is a stable type, or
\item
The expected type $\proto$ is an abstract type with a stable type as lower
bound, and the type $T$ of the entity referred to by $p$ does not
conform to $\proto$, or
\item
The path $p$ designates a module.
\end{enumerate}

The selection $e.x$ is evaluated by first evaluating the qualifier
expression $e$, which yields an object $r$, say. The selection's
result is then the member of $r$ that is either defined by $m$ or defined
by a definition overriding $m$. 
If that member has a type which
conforms to \lstinline@scala.NotNull@, the member's value must be initialized
to a value different from \lstinline@null@, otherwise a \lstinline@scala.UnitializedError@
is thrown.
 

\section{This and Super}
\label{sec:this-super}

\syntax\begin{lstlisting}
  SimpleExpr  ::=  [id `.'] `this'
                |  [id '.'] `super' [ClassQualifier] `.' id
\end{lstlisting}

The expression \code{this} can appear in the statement part of a
template or compound type. It stands for the object being defined by
the innermost template or compound type enclosing the reference. If
this is a compound type, the type of \code{this} is that compound type.
If it is a template of a
class or object definition with simple name $C$, the type of this
is the same as the type of \lstinline@$C$.this@.

The expression \lstinline@$C$.this@ is legal in the statement part of an
enclosing class or object definition with simple name $C$. It
stands for the object being defined by the innermost such definition.
If the expression's expected type is a stable type, or
\lstinline@$C$.this@ occurs as the prefix of a selection, its type is
\lstinline@$C$.this.type@, otherwise it is the self type of class $C$.

A reference ~\lstinline@super.$m$@~ refers statically to a method or type $m$
in the least proper supertype of the innermost template containing the
reference.  It evaluates to the member $m'$ in the actual supertype of
that template which is equal to $m$ or which overrides $m$.  The
statically referenced member $m$ must be a type or a
method. 
%explanation: so that we need not create several fields for overriding vals
If it is
a method, it must be concrete, or the template
containing the reference must have a member $m'$ which overrides $m$
and which is labeled \code{abstract override}.  

A reference ~\lstinline@$C$.super.$m$@~ refers statically to a method
or type $m$ in the least proper supertype of the innermost enclosing class or
object definition named $C$ which encloses the reference. It evaluates
to the member $m'$ in the actual supertype of that class or object
which is equal to $m$ or which overrides $m$. The
statically referenced member $m$ must be a type or a
method.  If the statically
referenced member $m$ is a method, it must be concrete, or the innermost enclosing
class or object definition named $C$ must have a member $m'$ which
overrides $m$ and which is labeled \code{abstract override}.

The \code{super} prefix may be followed by a trait qualifier
\lstinline@[$T\,$]@, as in \lstinline@$C$.super[$T\,$].$x$@. This is
called a {\em static super reference}.  In this case, the reference is
to the type or method of $x$ in the parent trait of $C$ whose simple
name is $T$. That member must be uniquely defined. If it is a method,
it must be concrete.

\example\label{ex:super}
Consider the following class definitions

\begin{lstlisting}
class Root { def x = "Root" }
class A extends Root { override def x = "A" ; def superA = super.x }
trait B extends Root { override def x = "B" ; def superB = super.x }
class C extends Root with B { 
  override def x = "C" ; def superC = super.x
}
class D extends A with B {
  override def x = "D" ; def superD = super.x
}
\end{lstlisting}
The linearization of class \code{C} is ~\lstinline@{C, B, Root}@~ and
the linearization of class \code{D} is ~\lstinline@{D, B, A, Root}@.
Then we have:
\begin{lstlisting}
(new A).superA == "Root", 
                          (new C).superB = "Root", (new C).superC = "B",
(new D).superA == "Root", (new D).superB = "A",    (new D).superD = "B",
\end{lstlisting}
Note that the \code{superB} function returns different results
depending on whether \code{B} is mixed in with class \code{Root} or \code{A}.

\comment{
\example Consider the following class definitions:
\begin{lstlisting}
class Shape {
  override def equals(other: Any) = $\ldots$ 
  $\ldots$
}
trait Bordered extends Shape {
  val thickness: Int 
  override def equals(other: Any) = other match {
    case that: Bordered => 
      super equals other && this.thickness == that.thickness
    case _ => false
  }
  $\ldots$
}
trait Colored extends Shape {
  val color: Color 
  override def equals(other: Any) = other match {
    case that: Colored => 
      super equals other && this.color == that.color
    case _ => false
  }
  $\ldots$
}
\end{lstlisting}

Both definitions of \code{equals} are combined in the class
below.
\begin{lstlisting}
trait BorderedColoredShape extends Shape with Bordered with Colored {
  override def equals(other: Any) = 
    super[Bordered].equals(that) && super[Colored].equals(that)
}
\end{lstlisting}
}

\section{Function Applications}
\label{sec:apply}

\syntax\begin{lstlisting}
  SimpleExpr    ::=  SimpleExpr1 ArgumentExprs
  ArgumentExprs ::=  `(' [Exprs] `)'
                  |  `(' [Exprs `,'] PostfixExpr `:' `_' `*' ')'
                  |  [nl] BlockExpr
  Exprs         ::=  Expr {`,' Expr}
\end{lstlisting}

An application \lstinline@$f$($e_1 \commadots e_m$)@ applies the
function $f$ to the argument expressions $e_1 \commadots e_m$. If $f$
has a method type \lstinline@($p_1$:$T_1 \commadots p_n$:$T_n$)$U$@, the type of
each argument expression $e_i$ is typed with the
corresponding parameter type $T_i$ as expected type. Let $S_i$ be type
type of argument $e_i$ $(i = 1 \commadots m)$. If $f$ is a polymorphic method,
local type inference (\sref{sec:local-type-inf}) is used to determine
type arguments for $f$. If $f$ has some value type, the application is taken to
be equivalent to \lstinline@$f$.apply($e_1 \commadots e_m$)@,
i.e.\ the application of an \code{apply} method defined by $f$.

The function $f$ must be {\em applicable} to its arguments $e_1
\commadots e_n$ of types $S_1 \commadots S_n$.

If $f$ has a method type $(p_1:T_1 \commadots p_n:T_n)U$
we say that an argument expression $e_i$ is a {\em named} argument if
it has the form $x_i=e'_i$ and $x_i$ is one of the parameter names
$p_1 \commadots p_n$. The function $f$ is applicable if all of the follwing conditions
hold:

\begin{itemize}
\item For every named argument $x_i=e'_i$ the type $S_i$
  is compatible with the parameter type $T_j$ whose name $p_j$ matches $x_i$.
\item For every positional argument $e_i$ the type $S_i$
is compatible with $T_i$.
%\item Every parameter $p_j:T_j$ which is not specified by either a positional
%  or a named argument has a default argument.
%\item The named arguments form a suffix of the argument list $e_1 \commadots e_m$,
%  i.e.\ no positional argument follows a named one.
%\item The names $x_i$ of all named arguments are pairwise distinct and no named
%  argument defines a parameter which is already specified by a
%  positional argument.
%\item Every formal parameter $p_j:T_j$ which is not specified by either a positional
%  or a named argument has a default argument.
\item If the expected type is defined, the result type $U$ is
compatible to it.
\end{itemize}

If $f$ is a polymorphic method it is applicable if local type
inference (\sref{sec:local-type-inf}) can
determine type arguments so that the instantiated method is applicable. If
$f$ has some value type it is applicable if it has a method member named
\code{apply} which is applicable.


%Class constructor functions
%(\sref{sec:class-defs}) can only be applied in constructor invocations
%(\sref{sec:constr-invoke}), never in expressions.

Evaluation of \lstinline@$f$($e_1 \commadots e_n$)@ usually entails evaluation of
$f$ and $e_1 \commadots e_n$ in that order. Each argument expression
is converted to the type of its corresponding formal parameter.  After
that, the application is rewritten to the function's right hand side,
with actual arguments substituted for formal parameters.  The result
of evaluating the rewritten right-hand side is finally converted to
the function's declared result type, if one is given.

The case of a formal parameter with a parameterless
method type ~\lstinline@=>$T$@~ is treated specially. In this case, the
corresponding actual argument expression $e$ is not evaluated before the
application. Instead, every use of the formal parameter on the
right-hand side of the rewrite rule entails a re-evaluation of $e$. 
In other words, the evaluation order for
\code{=>}-parameters is {\em call-by-name} whereas the evaluation
order for normal parameters is {\em call-by-value}.
Furthermore, it is required that $e$'s packed type (\sref{sec:expr-typing})
conforms to the parameter type $T$.
The behavior of by-name parameters is preserved if the application is
transformed into a block due to named or default arguments. In this case,
the local value for that parameter has the form \lstinline@val $y_i$ = () => $e$@
and the argument passed to the function is \lstinline@$y_i$()@.

The last argument in an application may be marked as a sequence
argument, e.g.\ \lstinline@$e$: _*@. Such an argument must correspond
to a repeated parameter (\sref{sec:repeated-params}) of type
\lstinline@$S$*@ and it must be the only argument matching this
parameter (i.e.\ the number of formal parameters and actual arguments
must be the same). Furthermore, the type of $e$ must conform to
~\lstinline@scala.Seq[$T$]@, for some type $T$ which conforms to
$S$. In this case, the argument list is transformed by replacing the
sequence $e$ with its elements. When the application uses named
arguments, the vararg parameter has to be specified exactly once.

A function application usually allocates a new frame on the program's
run-time stack. However, if a local function or a final method calls
itself as its last action, the call is executed using the stack-frame
of the caller.

\example Assume the following function which computes the sum of a
variable number of arguments:
\begin{lstlisting}
def sum(xs: Int*) = (0 /: xs) ((x, y) => x + y)
\end{lstlisting}
Then
\begin{lstlisting}
sum(1, 2, 3, 4)
sum(List(1, 2, 3, 4): _*)
\end{lstlisting}
both yield \code{10} as result. On the other hand, 
\begin{lstlisting}
sum(List(1, 2, 3, 4))
\end{lstlisting}
would not typecheck.

\subsection{Named and Default Arguments}
\label{sec:named-default}

If an application might uses named arguments $p = e$ or default
arguments, the following conditions must hold.
\begin{itemize}
\item The named arguments form a suffix of the argument list $e_1 \commadots e_m$,
  i.e.\ no positional argument follows a named one.
\item The names $x_i$ of all named arguments are pairwise distinct and no named
  argument defines a parameter which is already specified by a
  positional argument.
\item Every formal parameter $p_j:T_j$ which is not specified by either a positional
  or a named argument has a default argument.
\end{itemize}

If the application uses named or default
arguments the following transformation is applied to convert it into
an application without named or default arguments. 

If the function $f$
has the form \lstinline@$p.m$[$\targs$]@ it is transformed into the
block
\begin{lstlisting}
{ val q = $p$
  q.$m$[$\targs$]
}
\end{lstlisting}
If the function $f$ is itself an application expression the transformation
is applied recursively on $f$. The result of transforming $f$ is a block of
the form
\begin{lstlisting}
{ val q = $p$
  val $x_1$ = expr$_1$
  $\ldots$
  val $x_k$ = expr$_k$
  q.$m$[$\targs$]($\args_1$)$\commadots$($\args_l$)
}
\end{lstlisting}
where every argument in $(\args_1) \commadots (\args_l)$ is a reference to
one of the values $x_1 \commadots x_k$. To integrate the current application
into the block, first a value definition using a fresh name $y_i$ is created
for every argument in $e_1 \commadots e_m$, which is initialised to $e_i$ for
positional arguments and to $e'_i$ for named arguments of the form
\lstinline@$x_i=e'_i$@. Then, for every parameter which is not specified
by the argument list, a value definition using a fresh name $z_i$ is created,
which is initialized using the method computing the default argument of
this parameter (\sref{sec:funsigs}).

Let $\args$ be a permutation of the generated names $y_i$ and $z_i$ such such
that the position of each name matches the position of its corresponding
parameter in the method type \lstinline@($p_1:T_1 \commadots p_n:T_n$)$U$@.
The final result of the transformation is a block of the form
\begin{lstlisting}
{ val q = $p$
  val $x_1$ = expr$_1$
  $\ldots$
  val $x_l$ = expr$_k$
  val $y_1$ = $e_1$
  $\ldots$
  val $y_m$ = $e_m$
  val $z_1$ = q.$m\Dollar$default$\Dollar$i[$\targs$]($\args_1$)$\commadots$($\args_l$)
  $\ldots$
  val $z_d$ = q.$m\Dollar$default$\Dollar$j[$\targs$]($\args_1$)$\commadots$($\args_l$)
  q.$m$[$\targs$]($\args_1$)$\commadots$($\args_l$)($\args$)
}
\end{lstlisting}


\section{Method Values}\label{sec:meth-vals}

\syntax\begin{lstlisting}
  SimpleExpr    ::=  SimpleExpr1 `_'
\end{lstlisting}

The expression ~~\lstinline@$e$ _@~~ is well-formed if $e$ is of method
type or if $e$ is a call-by-name parameter.  If $e$ is a method with
parameters, \lstinline@$e$ _@~~ represents $e$ converted to a function
type by eta expansion (\sref{sec:eta-expand}). If $e$ is a
parameterless method or call-by-name parameter of type 
\lstinline@=>$T$@, ~\lstinline@$e$ _@~~ represents the function of type 
\lstinline@() => $T$@, which evaluates $e$ when it is applied to the empty
parameterlist \lstinline@()@.

\example The method values in the left column are each equivalent to the 
anonymous functions (\sref{sec:closures}) on their right.

\begin{lstlisting}
Math.sin _              x => Math.sin(x)
Array.range _           (x1, x2) => Array.range(x1, x2)
List.map2 _             (x1, x2) => (x3) => List.map2(x1, x2)(x3)
List.map2(xs, ys)_      x => List.map2(xs, ys)(x)
\end{lstlisting}

Note that a space is necessary between a method name and the trailing underscore
because otherwise the underscore would be considered part of the name.  

\section{Type Applications}
\label{sec:type-app}
\syntax\begin{lstlisting}
  SimpleExpr    ::=  SimpleExpr TypeArgs
\end{lstlisting}

A type application \lstinline@$e$[$T_1 \commadots T_n$]@ instantiates
a polymorphic value $e$ of type ~\lstinline@[$a_1$ >: $L_1$ <: $U_1
\commadots a_n$ >: $L_n$ <: $U_n$]$S$@~ with argument types
\lstinline@$T_1 \commadots T_n$@.  Every argument type $T_i$ must obey
the corresponding bounds $L_i$ and $U_i$.  That is, for each $i = 1
\commadots n$, we must have $\sigma L_i \conforms T_i \conforms \sigma
U_i$, where $\sigma$ is the substitution $[a_1 := T_1 \commadots a_n
:= T_n]$.  The type of the application is $\sigma S$.

If the function part $e$ is of some value type, the type application
is taken to be equivalent to 
~\lstinline@$e$.apply[$T_1 \commadots$ T$_n$]@, i.e.\ the application of an \code{apply} method defined by
$e$.

Type applications can be omitted if local type inference
(\sref{sec:local-type-inf}) can infer best type parameters for a
polymorphic functions from the types of the actual function arguments
and the expected result type.

\section{Tuples}
\label{sec:tuples}

\syntax\begin{lstlisting}
  SimpleExpr   ::=  `(' [Exprs] `)'
\end{lstlisting}

A tuple expression \lstinline@($e_1 \commadots e_n$)@ is an alias
for the class instance creation 
~\lstinline@scala.Tuple$n$($e_1 \commadots e_n$)@, where $n \geq 2$.  
The empty tuple
\lstinline@()@ is the unique value of type \lstinline@scala.Unit@.

\section{Instance Creation Expressions}
\label{sec:inst-creation}

\syntax\begin{lstlisting}
  SimpleExpr     ::=  `new' (ClassTemplate | TemplateBody)
\end{lstlisting}

A simple instance creation expression is of the form 
~\lstinline@new $c$@~ 
where $c$ is a constructor invocation
(\sref{sec:constr-invoke}).  Let $T$ be the type of $c$. Then $T$ must
denote a (a type instance of) a non-abstract subclass of
\lstinline@scala.AnyRef@. Furthermore, the {\em concrete self type} of the
expression must conform to the self type of the class denoted by $T$
(\sref{sec:templates}). The concrete self type is normally
$T$, except if the expression ~\lstinline@new $c$@~ appears as the
right hand side of a value definition
\begin{lstlisting}
val $x$: $S$ = new $c$
\end{lstlisting}
(where the type annotation ~\lstinline@: $S$@~ may be missing).
In the latter case, the concrete self type of the expression is the
compound type ~\lstinline@$T$ with $x$.type@.

The expression is evaluated by creating a fresh
object of type $T$ which is is initialized by evaluating $c$. The
type of the expression is $T$.

A general instance creation expression is of the form 
~\lstinline@new $t$@~ for some class template $t$ (\sref{sec:templates}).
Such an expression is equivalent to the block
\begin{lstlisting}
{ class $a$ extends $t$; new $a$ }
\end{lstlisting}
where $a$ is a fresh name of an {\em anonymous class} which is
inaccessible to user programs.

There is also a shorthand form for creating values of structural
types: If ~\lstinline@{$D$}@ is a class body, then 
~\lstinline@new {$D$}@~ is equivalent to the general instance creation expression
~\lstinline@new AnyRef{$D$}@.

\example Consider the following structural instance creation
expression:
\begin{lstlisting}
new { def getName() = "aaron" }
\end{lstlisting}
This is a shorthand for the general instance creation expression
\begin{lstlisting}
new AnyRef{ def getName() = "aaron" }
\end{lstlisting}
The latter is in turn a shorthand for the block
\begin{lstlisting}
{ class anon$\Dollar$X extends AnyRef{ def getName() = "aaron" }; new anon$\Dollar$X }
\end{lstlisting}
where \lstinline@anon$\Dollar$X@ is some freshly created name.

\section{Blocks}
\label{sec:blocks}

\syntax\begin{lstlisting}
  BlockExpr   ::=  `{' Block `}'
  Block       ::=  {BlockStat semi} [ResultExpr]
\end{lstlisting}

A block expression ~\lstinline@{$s_1$; $\ldots$; $s_n$; $e\,$}@~ is
constructed from a sequence of block statements $s_1 \commadots s_n$
and a final expression $e$.  The statement sequence may not contain
two definitions or declarations that bind the same name in the same
namespace.  The final expression can be omitted, in which
case the unit value \lstinline@()@ is assumed.

%Whether or not the scope includes the statement itself
%depends on the kind of definition.

The expected type of the final expression $e$ is the expected
type of the block. The expected type of all preceding statements is
undefined.

The type of a block ~\lstinline@$s_1$; $\ldots$; $s_n$; $e$@~ is
\lstinline@$T$ forSome {$\,Q\,$}@, where $T$ is the type of $e$ and $Q$ 
contains existential clauses (\sref{sec:existential-types})
for every value or type name which is free in $T$ 
and which is defined locally in one of the statements $s_1 \commadots s_n$.
We say the existential clause {\em binds} the occurrence of the value or type name.
Specifically, 
\begin{itemize}
\item
A locally defined type definition  ~\lstinline@type$\;t = T$@~
is bound by the existential clause ~\lstinline@type$\;t >: T <: T$@.
It is an error if $t$ carries type parameters. 
\item
A locally defined value definition~ \lstinline@val$\;x: T = e$@~ is
bound by the existential clause ~\lstinline@val$\;x: T$@.
\item
A locally defined class definition ~\lstinline@class$\;c$ extends$\;t$@~
is bound by the existential clause ~\lstinline@type$\;c <: T$@~ where
$T$ is the least class type or refinement type which is a proper
supertype of the type $c$. It is an error if $c$ carries type parameters. 
\item
A locally defined object definition ~\lstinline@object$\;x\;$extends$\;t$@~
is bound by the existential clause \lstinline@val$\;x: T$@ where
$T$ is the least class type or refinement type which is a proper supertype of the type 
\lstinline@$x$.type@.
\end{itemize}
Evaluation of the block entails evaluation of its
statement sequence, followed by an evaluation of the final expression
$e$, which defines the result of the block.

\example
Assuming a class \lstinline@Ref[T](x: T)@, the block
\begin{lstlisting}
{ class C extends B {$\ldots$} ; new Ref(new C) }
\end{lstlisting}
has the type ~\lstinline@Ref[_1] forSome { type _1 <: B }@.
The block
\begin{lstlisting}
{ class C extends B {$\ldots$} ; new C }
\end{lstlisting}
simply has type \code{B}, because with the rules in
(\sref{sec:ex-simpl} the existentially quantified type 
~\lstinline@_1 forSome { type _1 <: B }@~ can be simplified to \code{B}.


Prefix, Infix, and Postfix Operations
-------------------------------------

\label{sec:infix-operations}

\syntax\begin{lstlisting}
  PostfixExpr     ::=  InfixExpr [id [nl]]
  InfixExpr       ::=  PrefixExpr
                    |  InfixExpr id [nl] InfixExpr
  PrefixExpr      ::=  [`-' | `+' | `!' | `~'] SimpleExpr 
\end{lstlisting}

Expressions can be constructed from operands and operators. 

\subsection{Prefix Operations}

A prefix operation $\op;e$ consists of a prefix operator $\op$, which
must be one of the identifiers `\lstinline@+@', `\lstinline@-@',
`\lstinline@!@' or `\lstinline@~@'. The expression $\op;e$ is
equivalent to the postfix method application
\lstinline@e.unary_$\op$@.

\todo{Generalize to arbitrary operators}

Prefix operators are different from normal function applications in
that their operand expression need not be atomic. For instance, the
input sequence \lstinline@-sin(x)@ is read as \lstinline@-(sin(x))@, whereas the
function application \lstinline@negate sin(x)@ would be parsed as the
application of the infix operator \code{sin} to the operands
\code{negate} and \lstinline@(x)@.

\subsection{Postfix Operations}

A postfix operator can be an arbitrary identifier. The postfix
operation $e;\op$ is interpreted as $e.\op$. 

\subsection{Infix Operations}

An infix operator can be an arbitrary identifier. Infix operators have
precedence and associativity defined as follows:

The {\em precedence} of an infix operator is determined by the operator's first
character. Characters are listed below in increasing order of
precedence, with characters on the same line having the same precedence.
\begin{lstlisting}
        $\mbox{\rm\sl(all letters)}$
        |
        ^
        &
        < >
        = !
        :
        + -
        * / %
        $\mbox{\rm\sl(all other special characters)}$
\end{lstlisting}
That is, operators starting with a letter have lowest precedence,
followed by operators starting with `\lstinline@|@', etc.

There's one exception to this rule, which concerns
{\em assignment operators}(\sref{sec:assops}).
The precedence of an assigment operator is the same as the one
of simple assignment \code{(=)}. That is, it is lower than the
precedence of any other operator. 

The {\em associativity} of an operator is determined by the operator's
last character.  Operators ending in a colon `\lstinline@:@' are
right-associative. All other operators are left-associative.

Precedence and associativity of operators determine the grouping of
parts of an expression as follows.
\begin{itemize}
\item If there are several infix operations in an
expression, then operators with higher precedence bind more closely
than operators with lower precedence.
\item If there are consecutive infix
operations $e_0; \op_1; e_1; \op_2 \ldots \op_n; e_n$ 
with operators $\op_1 \commadots \op_n$ of the same precedence, 
then all these operators must
have the same associativity. If all operators are left-associative,
the sequence is interpreted as
$(\ldots(e_0;\op_1;e_1);\op_2\ldots);\op_n;e_n$. 
Otherwise, if all operators are right-associative, the
sequence is interpreted as
$e_0;\op_1;(e_1;\op_2;(\ldots \op_n;e_n)\ldots)$.
\item
Postfix operators always have lower precedence than infix
operators. E.g.\ $e_1;\op_1;e_2;\op_2$ is always equivalent to
$(e_1;\op_1;e_2);\op_2$.
\end{itemize}
The right-hand operand of a left-associative operator may consist of
several arguments enclosed in parentheses, e.g. $e;\op;(e_1,\ldots,e_n)$.
This expression is then interpreted as $e.\op(e_1,\ldots,e_n)$.

A left-associative binary
operation $e_1;\op;e_2$ is interpreted as $e_1.\op(e_2)$. If $\op$ is
right-associative, the same operation is interpreted as
~\lstinline@{ val $x$=$e_1$; $e_2$.$\op$($x\,$) }@, where $x$ is a fresh
name. 

\subsection{Assignment Operators} \label{sec:assops}

An assignment operator is an operator symbol (syntax category
\lstinline@op@ in [Identifiers](#identifiers)) that ends in an equals character
``\code{=}'', with the exception of operators for which one of 
the following conditions holds:
\begin{itemize}
\item[(1)] the operator also starts with an equals character, or
\item[(2)] the operator is one of \code{(<=)}, \code{(>=)},
  \code{(!=)}.
\end{itemize}

Assignment operators are treated specially in that they
can be expanded to assignments if no other interpretation is valid.

Let's consider an assignment operator such as \code{+=} in an infix
operation ~\lstinline@$l$ += $r$@, where $l$, $r$ are expressions.  
This operation can be re-interpreted as an operation which corresponds 
to the assignment
\begin{lstlisting}
$l$ = $l$ + $r$
\end{lstlisting}
except that the operation's left-hand-side $l$ is evaluated only once.

The re-interpretation occurs if the following two conditions are fulfilled.
\begin{enumerate}
\item 
The left-hand-side $l$ does not have a member named
\code{+=}, and also cannot be converted by an implicit conversion (\sref{sec:impl-conv})
to a value with a member named \code{+=}.
\item
The assignment \lstinline@$l$ = $l$ + $r$@ is type-correct. 
In particular this implies that $l$ refers to a variable or object 
that can be assigned to, and that is convertible to a value with a member named \code{+}.
\end{enumerate}

\section{Typed Expressions}

\syntax\begin{lstlisting}
  Expr1              ::=  PostfixExpr `:' CompoundType
\end{lstlisting}

The typed expression $e: T$ has type $T$. The type of
expression $e$ is expected to conform to $T$. The result of
the expression is the value of $e$ converted to type $T$.

\example Here are examples of well-typed and illegally typed expressions.

\begin{lstlisting}
  1: Int               // legal, of type Int
  1: Long              // legal, of type Long
  // 1: string         // ***** illegal
\end{lstlisting}



\section{Annotated Expressions}

\syntax\begin{lstlisting}
  Expr1              ::=  PostfixExpr `:' Annotation {Annotation} 
\end{lstlisting}

An annotated expression ~\lstinline^$e$: @$a_1$ $\ldots$ @$a_n$^
attaches annotations $a_1 \commadots a_n$ to the expression $e$
(\sref{sec:annotations}).

\section{Assignments}\label{sec:assigments}

\syntax\begin{lstlisting}
  Expr1        ::=  [SimpleExpr `.'] id `=' Expr
                 |  SimpleExpr1 ArgumentExprs `=' Expr
\end{lstlisting}

The interpretation of an assignment to a simple variable ~\lstinline@$x$ = $e$@~
depends on the definition of $x$. If $x$ denotes a mutable
variable, then the assignment changes the current value of $x$ to be
the result of evaluating the expression $e$. The type of $e$ is
expected to conform to the type of $x$. If $x$ is a parameterless
function defined in some template, and the same template contains a
setter function \lstinline@$x$_=@ as member, then the assignment
~\lstinline@$x$ = $e$@~ is interpreted as the invocation
~\lstinline@$x$_=($e\,$)@~ of that setter function.  Analogously, an
assignment ~\lstinline@$f.x$ = $e$@~ to a parameterless function $x$
is interpreted as the invocation ~\lstinline@$f.x$_=($e\,$)@.

An assignment ~\lstinline@$f$($\args\,$) = $e$@~ with a function application to the
left of the `\lstinline@=@' operator is interpreted as 
~\lstinline@$f.$update($\args$, $e\,$)@, i.e.\
the invocation of an \code{update} function defined by $f$.

\example
Here are some assignment expressions and their equivalent expansions.
\begin{lstlisting}
x.f = e                 x.f_=(e)
x.f() = e               x.f.update(e)
x.f(i) = e              x.f.update(i, e)
x.f(i, j) = e           x.f.update(i, j, e)
\end{lstlisting}

\example \label{ex:imp-mat-mul}
Here is the usual imperative code for matrix multiplication.

\begin{lstlisting}
def matmul(xss: Array[Array[Double]], yss: Array[Array[Double]]) = {
  val zss: Array[Array[Double]] = new Array(xss.length, yss(0).length) 
  var i = 0 
  while (i < xss.length) {
    var j = 0 
    while (j < yss(0).length) {
      var acc = 0.0 
      var k = 0 
      while (k < yss.length) {
        acc = acc + xss(i)(k) * yss(k)(j) 
        k += 1
      }
      zss(i)(j) = acc 
      j += 1
    }
    i += 1
  }
  zss
}
\end{lstlisting}
Desugaring the array accesses and assignments yields the following
expanded version:
\begin{lstlisting}
def matmul(xss: Array[Array[Double]], yss: Array[Array[Double]]) = {
  val zss: Array[Array[Double]] = new Array(xss.length, yss.apply(0).length) 
  var i = 0 
  while (i < xss.length) {
    var j = 0 
    while (j < yss.apply(0).length) {
      var acc = 0.0 
      var k = 0 
      while (k < yss.length) {
        acc = acc + xss.apply(i).apply(k) * yss.apply(k).apply(j) 
        k += 1
      }
      zss.apply(i).update(j, acc) 
      j += 1
    }
    i += 1
  }
  zss
}
\end{lstlisting}

Conditional Expressions
-----------------------

\label{sec:cond}

\syntax\begin{lstlisting}
  Expr1          ::=  `if' `(' Expr `)' {nl} Expr [[semi] `else' Expr]
\end{lstlisting}

The conditional expression ~\lstinline@if ($e_1$) $e_2$ else $e_3$@~ chooses
one of the values of $e_2$ and $e_3$, depending on the
value of $e_1$. The condition $e_1$ is expected to
conform to type \code{Boolean}.  The then-part $e_2$ and the
else-part $e_3$ are both expected to conform to the expected
type of the conditional expression. The type of the conditional
expression is the weak least upper bound (\sref{sec:weakconformance})
of the types of $e_2$ and
$e_3$.  A semicolon preceding the \code{else} symbol of a
conditional expression is ignored.

The conditional expression is evaluated by evaluating first
$e_1$. If this evaluates to \code{true}, the result of
evaluating $e_2$ is returned, otherwise the result of
evaluating $e_3$ is returned.

A short form of the conditional expression eliminates the
else-part. The conditional expression ~\lstinline@if ($e_1$) $e_2$@~ is
evaluated as if it was ~\lstinline@if ($e_1$) $e_2$ else ()@.  

While Loop Expressions
----------------------

\label{sec:while}

\syntax\begin{lstlisting}
  Expr1          ::=  `while' `(' Expr ')' {nl} Expr
\end{lstlisting}

The while loop expression ~\lstinline@while ($e_1$) $e_2$@~ is typed and
evaluated as if it was an application of ~\lstinline@whileLoop ($e_1$) ($e_2$)@~ where
the hypothetical function \code{whileLoop} is defined as follows.

\begin{lstlisting}
  def whileLoop(cond: => Boolean)(body: => Unit): Unit  =
    if (cond) { body ; whileLoop(cond)(body) } else {}
\end{lstlisting}

\comment{
\example The loop 
\begin{lstlisting}
  while (x != 0) { y = y + 1/x ; x -= 1 }
\end{lstlisting}
Is equivalent to the application
\begin{lstlisting}
  whileLoop (x != 0) { y = y + 1/x ; x -= 1 }
\end{lstlisting}
Note that this application will never produce a division-by-zero 
error at run-time, since the
expression ~\lstinline@(y = 1/x)@~ will be evaluated in the body of
\code{while} only if the condition parameter is false.
}

\section{Do Loop Expressions}

\syntax\begin{lstlisting}
  Expr1          ::=  `do' Expr [semi] `while' `(' Expr ')'
\end{lstlisting}

The do loop expression ~\lstinline@do $e_1$ while ($e_2$)@~ is typed and
evaluated as if it was the expression ~\lstinline@($e_1$ ; while ($e_2$) $e_1$)@.
A semicolon preceding the \code{while} symbol of a do loop expression is ignored.


For Comprehensions and For Loops
--------------------------------

\label{sec:for-comprehensions}

\syntax\begin{lstlisting}
  Expr1          ::=  `for' (`(' Enumerators `)' | `{' Enumerators `}') 
                         {nl} [`yield'] Expr
  Enumerators    ::=  Generator {semi Enumerator}
  Enumerator     ::=  Generator 
                   |  Guard
                   |  `val' Pattern1 `=' Expr
  Generator      ::=  Pattern1 `<-' Expr [Guard]
  Guard          ::=  `if' PostfixExpr
\end{lstlisting}

A for loop ~\lstinline@for ($\enums\,$) $e$@~ executes expression $e$
for each binding generated by the enumerators $\enums$.  A for
comprehension ~\lstinline@for ($\enums\,$) yield $e$@~ evaluates
expression $e$ for each binding generated by the enumerators $\enums$
and collects the results. An enumerator sequence always starts with a
generator; this can be followed by further generators, value
definitions, or guards.  A {\em generator} ~\lstinline@$p$ <- $e$@~
produces bindings from an expression $e$ which is matched in some way
against pattern $p$. A {\em value definition} ~\lstinline@$p$ = $e$@~ 
binds the value name $p$ (or several names in a pattern $p$) to
the result of evaluating the expression $e$.  A {\em guard}
~\lstinline@if $e$@ contains a boolean expression which restricts
enumerated bindings. The precise meaning of generators and guards is
defined by translation to invocations of four methods: \code{map},
\code{withFilter}, \code{flatMap}, and \code{foreach}. These methods can
be implemented in different ways for different carrier types.
\comment{As an example, an implementation of these methods for lists
  is given in \sref{cls-list}.}

The translation scheme is as follows.  In a first step, every
generator ~\lstinline@$p$ <- $e$@, where $p$ is not irrefutable (\sref{sec:patterns})
for the type of $e$ is replaced by
\begin{lstlisting}
$p$ <- $e$.withFilter { case $p$ => true; case _ => false }
\end{lstlisting}

Then, the following rules are applied repeatedly until all
comprehensions have been eliminated.
\begin{itemize}
\item
A for comprehension 
~\lstinline@for ($p$ <- $e\,$) yield $e'$@~ 
is translated to
~\lstinline@$e$.map { case $p$ => $e'$ }@.

\item
A for loop
~\lstinline@for ($p$ <- $e\,$) $e'$@~ 
is translated to
~\lstinline@$e$.foreach { case $p$ => $e'$ }@.

\item
A for comprehension
\begin{lstlisting}
for ($p$ <- $e$; $p'$ <- $e'; \ldots$) yield $e''$ ,
\end{lstlisting}
where \lstinline@$\ldots$@ is a (possibly empty)
sequence of generators, definitions, or guards,
is translated to
\begin{lstlisting}
$e$.flatMap { case $p$ => for ($p'$ <- $e'; \ldots$) yield $e''$ } .
\end{lstlisting}
\item
A for loop
\begin{lstlisting}
for ($p$ <- $e$; $p'$ <- $e'; \ldots$) $e''$ .
\end{lstlisting}
where \lstinline@$\ldots$@ is a (possibly empty)
sequence of generators, definitions, or guards,
is translated to
\begin{lstlisting}
$e$.foreach { case $p$ => for ($p'$ <- $e'; \ldots$) $e''$ } .
\end{lstlisting}
\item
A generator ~\lstinline@$p$ <- $e$@~ followed by a guard
~\lstinline@if $g$@~ is translated to a single generator 
~\lstinline@$p$ <- $e$.withFilter(($x_1 \commadots x_n$) => $g\,$)@~ where
$x_1 \commadots x_n$ are the free variables of $p$.
\item
A generator ~\lstinline@$p$ <- $e$@~ followed by a value definition 
~\lstinline@$p'$ = $e'$@ is translated to the following generator of pairs of values, where
$x$ and $x'$ are fresh names:
\begin{lstlisting}
($p$, $p'$) <- for ($x @ p$ <- $e$) yield { val $x' @ p'$ = $e'$; ($x$, $x'$) }
\end{lstlisting}
\end{itemize}

\example
The following code produces all pairs of numbers
between $1$ and $n-1$ whose sums are prime.
\begin{lstlisting}
for  { i <- 1 until n 
       j <- 1 until i 
       if isPrime(i+j)
} yield (i, j)
\end{lstlisting}
The for comprehension is translated to:
\begin{lstlisting}
(1 until n)
  .flatMap {
     case i => (1 until i)
       .withFilter { j => isPrime(i+j) }
       .map { case j => (i, j) } }
\end{lstlisting}

\comment{
\example
\begin{lstlisting}
class List[A] {
  def map[B](f: A => B): List[B] = match {
    case <> => <>
    case x :: xs => f(x) :: xs.map(f)
  }
  def withFilter(p: A => Boolean) = match {
    case <> => <>
    case x :: xs => if p(x) then x :: xs.withFilter(p) else xs.withFilter(p)
  }
  def flatMap[B](f: A => List[B]): List[B] =
    if (isEmpty) Nil
    else f(head) ::: tail.flatMap(f) 
  def foreach(f: A => Unit): Unit =
    if (isEmpty) ()
    else (f(head); tail.foreach(f)) 
}
\end{lstlisting}

\example
\begin{lstlisting}
abstract class Graph[Node] {
  type Edge = (Node, Node)
  val nodes: List[Node]
  val edges: List[Edge]
  def succs(n: Node) = for ((p, s) <- g.edges, p == n) s
  def preds(n: Node) = for ((p, s) <- g.edges, s == n) p
}
def topsort[Node](g: Graph[Node]): List[Node] = {
  val sources = for (n <- g.nodes, g.preds(n) == <>) n
  if (g.nodes.isEmpty) <>
  else if (sources.isEmpty) new Error(``topsort of cyclic graph'') throw
  else sources :+: topsort(new Graph[Node] {
    val nodes = g.nodes diff sources
    val edges = for ((p, s) <- g.edges, !(sources contains p)) (p, s)
  })
}
\end{lstlisting}
}

\example For comprehensions can be used to express vector 
and matrix algorithms concisely. 
For instance, here is a function to compute the transpose of a given matrix:
% see test/files/run/t0421.scala
\begin{lstlisting}
def transpose[A](xss: Array[Array[A]]) = {
  for (i <- Array.range(0, xss(0).length)) yield
    for (xs <- xss) yield xs(i)
}
\end{lstlisting}

Here is a function to compute the scalar product of two vectors:
\begin{lstlisting}
def scalprod(xs: Array[Double], ys: Array[Double]) = {
  var acc = 0.0 
  for ((x, y) <- xs zip ys) acc = acc + x * y  
  acc
}
\end{lstlisting}

Finally, here is a function to compute the product of two matrices. 
Compare with the imperative version of \ref{ex:imp-mat-mul}.
\begin{lstlisting}
def matmul(xss: Array[Array[Double]], yss: Array[Array[Double]]) = {
  val ysst = transpose(yss) 
  for (xs <- xss) yield
    for (yst <- ysst) yield 
      scalprod(xs, yst)
}
\end{lstlisting}
The code above makes use of the fact that \code{map}, \code{flatMap},
\code{withFilter}, and \code{foreach} are defined for instances of class
\lstinline@scala.Array@.

\section{Return Expressions}

\syntax\begin{lstlisting}
  Expr1      ::=  `return' [Expr]
\end{lstlisting}

A return expression ~\lstinline@return $e$@~ must occur inside the body of some
enclosing named method or function. The innermost enclosing named
method or function in a source program, $f$, must have an explicitly declared result type,
and the type of $e$ must conform to it.  
The return expression
evaluates the expression $e$ and returns its value as the result of
$f$. The evaluation of any statements or
expressions following the return expression is omitted. The type of 
a return expression is \code{scala.Nothing}.

The expression $e$ may be omitted.  The return expression
~\lstinline@return@~ is type-checked and evaluated as if it was ~\lstinline@return ()@.

An \lstinline@apply@ method which is generated by the compiler as an
expansion of an anonymous function does not count as a named function
in the source program, and therefore is never the target of a return
expression.

Returning from a nested anonymous function is implemented by throwing
and catching a \lstinline@scala.runtime.NonLocalReturnException@.  Any
exception catches between the point of return and the enclosing
methods might see the exception.  A key comparison makes sure that
these exceptions are only caught by the method instance which is
terminated by the return.

If the return expression is itself part of an anonymous function, it
is possible that the enclosing instance of $f$ has already returned
before the return expression is executed. In that case, the thrown
\lstinline@scala.runtime.NonLocalReturnException@ will not be caught,
and will propagate up the call stack.



\section{Throw Expressions}

\syntax\begin{lstlisting}
  Expr1      ::=  `throw' Expr
\end{lstlisting}

A throw expression ~\lstinline@throw $e$@~ evaluates the expression
$e$. The type of this expression must conform to
\code{Throwable}.  If $e$ evaluates to an exception
reference, evaluation is aborted with the thrown exception. If $e$
evaluates to \code{null}, evaluation is instead aborted with a
\code{NullPointerException}. If there is an active
\code{try} expression (\sref{sec:try}) which handles the thrown
exception, evaluation resumes with the handler; otherwise the thread
executing the \code{throw} is aborted.  The type of a throw expression
is \code{scala.Nothing}.

\section{Try Expressions}\label{sec:try}

\syntax\begin{lstlisting}
  Expr1 ::=  `try' `{' Block `}' [`catch' `{' CaseClauses `}'] 
             [`finally' Expr]
\end{lstlisting}

A try expression is of the form ~\lstinline@try { $b$ } catch $h$@~
where the handler $h$ is a pattern matching anonymous function 
(\sref{sec:pattern-closures})
\begin{lstlisting}
 { case $p_1$ => $b_1$ $\ldots$ case $p_n$ => $b_n$ } .
\end{lstlisting}
This expression is evaluated by evaluating the block
$b$.  If evaluation of $b$ does not cause an exception to be
thrown, the result of $b$ is returned. Otherwise the 
handler $h$ is applied to the thrown exception.  
If the handler contains a case matching the thrown exception,
the first such case is invoked. If the handler contains
no case matching the thrown exception, the exception is 
re-thrown. 

Let $\proto$ be the expected type of the try expression.  The block
$b$ is expected to conform to $\proto$.  The handler $h$
is expected conform to type
~\lstinline@scala.PartialFunction[scala.Throwable, $\proto\,$]@.  The
type of the try expression is the weak least upper bound (\sref{sec:weakconformance})
of the type of $b$
and the result type of $h$.

A try expression ~\lstinline@try { $b$ } finally $e$@~ evaluates the block
$b$.  If evaluation of $b$ does not cause an exception to be
thrown, the expression $e$ is evaluated. If an exception is thrown
during evaluation of $e$, the evaluation of the try expression is
aborted with the thrown exception. If no exception is thrown during
evaluation of $e$, the result of $b$ is returned as the
result of the try expression. 

If an exception is thrown during evaluation of $b$, the finally block
$e$ is also evaluated. If another exception $e$ is thrown
during evaluation of $e$, evaluation of the try expression is
aborted with the thrown exception. If no exception is thrown during
evaluation of $e$, the original exception thrown in $b$ is
re-thrown once evaluation of $e$ has completed.  The block
$b$ is expected to conform to the expected type of the try
expression. The finally expression $e$ is expected to conform to
type \code{Unit}.

A try expression ~\lstinline@try { $b$ } catch $e_1$ finally $e_2$@~ 
is a shorthand
for  ~\lstinline@try { try { $b$ } catch $e_1$ } finally $e_2$@.

\section{Anonymous Functions}
\label{sec:closures}

\syntax\begin{lstlisting}
  Expr            ::=  (Bindings | [`implicit'] id | `_') `=>' Expr
  ResultExpr      ::=  (Bindings | ([`implicit'] id | `_') `:' CompoundType) `=>' Block
  Bindings        ::=  `(' Binding {`,' Binding} `)'
  Binding         ::=  (id | `_') [`:' Type]
\end{lstlisting}

The anonymous function ~\lstinline@($x_1$: $T_1 \commadots x_n$: $T_n$) => e@~ 
maps parameters $x_i$ of types $T_i$ to a result given
by expression $e$. The scope of each formal parameter
$x_i$ is $e$. Formal parameters must have pairwise distinct names.

If the expected type of the anonymous function is of the form
~\lstinline@scala.Function$n$[$S_1 \commadots S_n$, $R\,$]@, the
expected type of $e$ is $R$ and the type $T_i$ of any of the
parameters $x_i$ can be omitted, in which
case~\lstinline@$T_i$ = $S_i$@ is assumed.
If the expected type of the anonymous function is
some other type, all formal parameter types must be explicitly given,
and the expected type of $e$ is undefined. The type of the anonymous
function
is~\lstinline@scala.Function$n$[$S_1 \commadots S_n$, $T\,$]@,
where $T$ is the packed type (\sref{sec:expr-typing}) 
of $e$. $T$ must be equivalent to a
type which does not refer to any of the formal parameters $x_i$.

The anonymous function is evaluated as the instance creation expression
\begin{lstlisting}
new scala.Function$n$[$T_1 \commadots T_n$, $T$] {
  def apply($x_1$: $T_1 \commadots x_n$: $T_n$): $T$ = $e$
}
\end{lstlisting}
In the case of a single untyped formal parameter, 
~\lstinline@($x\,$) => $e$@~ 
can be abbreviated to ~\lstinline@$x$ => $e$@. If an
anonymous function ~\lstinline@($x$: $T\,$) => $e$@~ with a single
typed parameter appears as the result expression of a block, it can be
abbreviated to ~\lstinline@$x$: $T$ => e@.

A formal parameter may also be a wildcard represented by an underscore \lstinline@_@. 
In that case, a fresh name for the parameter is chosen arbitrarily.

A named parameter of an anonymous function may be optionally preceded
by an \lstinline@implicit@ modifier. In that case the parameter is
labeled \lstinline@implicit@ (\sref{sec:implicits}); however the
parameter section itself does not count as an implicit parameter
section in the sense of (\sref{sec:impl-params}). Hence, arguments to
anonymous functions always have to be given explicitly.

\example Examples of anonymous functions:

\begin{lstlisting}
  x => x                             // The identity function

  f => g => x => f(g(x))             // Curried function composition

  (x: Int,y: Int) => x + y           // A summation function

  () => { count += 1; count }        // The function which takes an
                                     // empty parameter list $()$, 
                                     // increments a non-local variable 
                                     // `count' and returns the new value.

  _ => 5                             // The function that ignores its argument
                                     // and always returns 5.
\end{lstlisting}

\subsection*{Placeholder Syntax for Anonymous Functions}\label{sec:impl-anon-fun}

\syntax\begin{lstlisting}
  SimpleExpr1  ::=  `_'
\end{lstlisting}

An expression (of syntactic category \lstinline@Expr@)
may contain embedded underscore symbols \code{_} at places where identifiers
are legal. Such an expression represents an anonymous function where subsequent
occurrences of underscores denote successive parameters.

Define an {\em underscore section} to be an expression of the form
\lstinline@_:$T$@ where $T$ is a type, or else of the form \code{_},
provided the underscore does not appear as the expression part of a
type ascription \lstinline@_:$T$@.

An expression $e$ of syntactic category \code{Expr} {\em binds} an underscore section
$u$, if the following two conditions hold: (1) $e$ properly contains $u$, and
(2) there is no other expression of syntactic category \code{Expr} 
which is properly contained in $e$ and which itself properly contains $u$.

If an expression $e$ binds underscore sections $u_1 \commadots u_n$, in this order, it is equivalent to 
the anonymous function ~\lstinline@($u'_1$, ... $u'_n$) => $e'$@~
where each $u_i'$ results from $u_i$ by replacing the underscore with a fresh identifier and
$e'$ results from $e$ by replacing each underscore section $u_i$ by $u_i'$.

\example The anonymous functions in the left column use placeholder
syntax. Each of these is equivalent to the anonymous function on its right.

\begin{lstlisting}
_ + 1                  x => x + 1
_ * _                  (x1, x2) => x1 * x2
(_: Int) * 2           (x: Int) => (x: Int) * 2
if (_) x else y        z => if (z) x else y
_.map(f)               x => x.map(f)
_.map(_ + 1)           x => x.map(y => y + 1)
\end{lstlisting}

\section{Constant Expressions}\label{sec:constant-expression}

Constant expressions are expressions that the Scala compiler can evaluate to a constant.
The definition of ``constant expression'' depends on the platform, but they
include at least the expressions of the following forms:
\begin{itemize}
\item A literal of a value class, such as an integer
\item A string literal
\item A class constructed with \code{Predef.classOf} (\sref{cls:predef})
\item An element of an enumeration from the underlying platform
\item A literal array, of the form
      \lstinline^Array$(c_1 \commadots c_n)$^,
      where all of the $c_i$'s are themselves constant expressions
\item An identifier defined by a constant value definition (\sref{sec:valdef}).
\end{itemize}


\section{Statements}
\label{sec:statements}

\syntax\begin{lstlisting}
  BlockStat    ::=  Import
                 |  {Annotation} [`implicit'] Def
                 |  {Annotation} {LocalModifier} TmplDef
                 |  Expr1
                 | 
  TemplateStat ::=  Import
                 |  {Annotation} {Modifier} Def
                 |  {Annotation} {Modifier} Dcl
                 |  Expr
                 | 
\end{lstlisting}

Statements occur as parts of blocks and templates.  A statement can be
an import, a definition or an expression, or it can be empty.
Statements used in the template of a class definition can also be
declarations.  An expression that is used as a statement can have an
arbitrary value type. An expression statement $e$ is evaluated by
evaluating $e$ and discarding the result of the evaluation. 
\todo{Generalize to implicit coercion?}

Block statements may be definitions which bind local names in the
block. The only modifier allowed in all block-local definitions is
\code{implicit}. When prefixing a class or object definition,
modifiers \code{abstract}, \code{final}, and \code{sealed} are also
permitted.

Evaluation of a statement sequence entails evaluation of the
statements in the order they are written.


Implicit Conversions
--------------------

\label{sec:impl-conv}

Implicit conversions can be applied to expressions whose type does not
match their expected type, to qualifiers in selections, and to unapplied methods. The
available implicit conversions are given in the next two sub-sections.

We say, a type $T$ is {\em compatible} to a type $U$ if $T$ conforms
to $U$ after applying eta-expansion (\sref{sec:eta-expand}) and view applications
(\sref{sec:views}).

\subsection{Value Conversions}

The following five implicit conversions can be applied to an
expression $e$ which has some value type $T$ and which is type-checked with
some expected type $\proto$.

\paragraph{\em Overloading Resolution} 
If an expression denotes several possible members of a class, 
overloading resolution (\sref{sec:overloading-resolution})
is applied to pick a unique member.

\paragraph{\em Type Instantiation}  
An expression $e$ of polymorphic type
\begin{lstlisting}
[$a_1$ >: $L_1$ <: $U_1 \commadots a_n$ >: $L_n$ <: $U_n$]$T$
\end{lstlisting}
which does not appear as the function part of
a type application is converted to a type instance of $T$
by determining with local type inference
(\sref{sec:local-type-inf}) instance types ~\lstinline@$T_1 \commadots T_n$@~ 
for the type variables ~\lstinline@$a_1 \commadots a_n$@~ and
implicitly embedding $e$ in the type application
~\lstinline@$e$[$T_1 \commadots T_n$]@~ (\sref{sec:type-app}).

\paragraph{\em Numeric Widening}
If $e$ has a primitive number type which weakly conforms
(\sref{sec:weakconformance}) to the expected type, it is widened to
the expected type using one of the numeric conversion methods
\code{toShort}, \code{toChar}, \code{toInt}, \code{toLong},
\code{toFloat}, \code{toDouble} defined in \sref{cls:numeric-value}.

\paragraph{\em Numeric Literal Narrowing}
If the expected type is \code{Byte}, \code{Short} or \code{Char}, and
the expression $e$ is an integer literal fitting in the range of that
type, it is converted to the same literal in that type.

\paragraph{\em Value Discarding}
If $e$ has some value type and the expected type is \code{Unit},
$e$ is converted to the expected type by embedding it in the 
term ~\lstinline@{ $e$; () }@.

\paragraph{\em View Application}
If none of the previous conversions applies, and $e$'s type
does not conform to the expected type $\proto$, it is attempted to convert
$e$ to the expected type with a view (\sref{sec:views}).\bigskip

\paragraph{\em Dynamic Member Selection}
If none of the previous conversions applies, and $e$ is a prefix
of a selection $e.x$, and $e$'s type conforms to class \code{scala.Dynamic},
then the selection is rewritten according to the rules for dynamic
member selection (\sref{sec:dyn-mem-sel}).

\subsection{Method Conversions}

The following four implicit conversions can be applied to methods
which are not applied to some argument list.

\paragraph{\em Evaluation}
A parameterless method $m$ of type \lstinline@=> $T$@ is always converted to
type $T$ by evaluating the expression to which $m$ is bound.

\paragraph{\em Implicit Application}
  If the method takes only implicit parameters, implicit
  arguments are passed following the rules of \sref{sec:impl-params}.

\paragraph{\em Eta Expansion}
  Otherwise, if the method is not a constructor, 
  and the expected type $\proto$ is a function type
  $(\Ts') \Arrow T'$, eta-expansion
  (\sref{sec:eta-expand}) is performed on the
  expression $e$.

\paragraph{\em Empty Application}
  Otherwise, if $e$ has method type $()T$, it is implicitly applied to the empty
  argument list, yielding $e()$.

\subsection{Overloading Resolution}
\label{sec:overloading-resolution}

If an identifier or selection $e$ references several members of a
class, the context of the reference is used to identify a unique
member.  The way this is done depends on whether or not $e$ is used as
a function. Let $\AA$ be the set of members referenced by $e$.

Assume first that $e$ appears as a function in an application, as in
\lstinline@$e$($e_1 \commadots e_m$)@.  

One first determines the set of functions that is potentially
applicable based on the {\em shape} of the arguments.

\newcommand{\shape}{\mbox{\sl shape}}

The shape of an argument expression $e$, written  $\shape(e)$, is
a type that is defined as follows:
\begin{itemize}
\item 
For a function expression \lstinline@($p_1$: $T_1 \commadots p_n$: $T_n$) => $b$@:
\lstinline@(Any $\commadots$ Any) => $\shape(b)$@, where \lstinline@Any@ occurs $n$ times
in the argument type.
\item
For a named argument \lstinline@$n$ = $e$@: $\shape(e)$.
\item
For all other expressions: \lstinline@Nothing@.
\end{itemize}

Let $\BB$ be the set of alternatives in $\AA$ that are {\em applicable} (\sref{sec:apply})
to expressions $(e_1 \commadots e_n)$ of types
$(\shape(e_1) \commadots \shape(e_n))$.
If there is precisely one
alternative in $\BB$, that alternative is chosen.

Otherwise, let $S_1 \commadots S_m$ be the vector of types obtained by
typing each argument with an undefined expected type.  For every
member $m$ in $\BB$ one determines whether it is 
applicable to expressions ($e_1 \commadots e_m$) of types $S_1
\commadots S_m$.
It is an error if none of the members in $\BB$ is applicable. If there is one
single applicable alternative, that alternative is chosen. Otherwise, let $\CC$
be the set of applicable alternatives which don't employ any default argument
in the application to $e_1 \commadots e_m$. It is again an error if $\CC$ is empty.
Otherwise, one chooses the {\em most specific} alternative among the alternatives
in $\CC$, according to the following definition of being ``as specific as'', and
``more specific than'':

%% question: given
%%  def f(x: Int)
%%  val f: { def apply(x: Int) }
%%  f(1) // the value is chosen in our current implementation

%% why?
%%  - method is as specific as value, because value is applicable to method's argument types (item 1)
%%  - value is as specific as method (item 3, any other type is always as specific..)
%% so the method is not more specific than the value.

\begin{itemize} 
\item
A parameterized method $m$ of type \lstinline@($p_1:T_1\commadots p_n:T_n$)$U$@ is {\em as specific as} some other
member $m'$ of type $S$ if $m'$ is applicable to arguments
\lstinline@($p_1 \commadots p_n\,$)@ of
types $T_1 \commadots T_n$.
\item
A polymorphic method of type
~\lstinline@[$a_1$ >: $L_1$ <: $U_1 \commadots a_n$ >: $L_n$ <: $U_n$]$T$@~ is
as specific as some other member of type $S$ if $T$ is as 
specific as $S$ under the assumption that for
$i = 1 \commadots n$ each $a_i$ is an abstract type name
bounded from below by $L_i$ and from above by $U_i$.
\item
A member of any other type is always as specific as a parameterized method
or a polymorphic method.
\item 
Given two members of types $T$ and $U$ which are 
neither parameterized nor polymorphic method types, the member of type $T$ is as specific as
the member of type $U$ if the existential dual of $T$ conforms to the existential dual of $U$. 
Here, the existential dual of a polymorphic type 
~\lstinline@[$a_1$ >: $L_1$ <: $U_1 \commadots a_n$ >: $L_n$ <: $U_n$]$T$@~ is
~\lstinline@$T$ forSome { type $a_1$ >: $L_1$ <: $U_1$ $\commadots$ type $a_n$ >: $L_n$ <: $U_n$}@.
The existential dual of every other type is the type itself.
\end{itemize}

The {\em relative weight} of an alternative $A$ over an alternative $B$ is a
number from 0 to 2, defined as the sum of
\begin{itemize}
\item 1 if $A$ is as specific as $B$, 0 otherwise, and
\item 1 if $A$ is defined in a class or object which is derived
      from the class or object defining $B$, 0 otherwise.
\end{itemize}
A class or object $C$ is {\em derived} from a class or object $D$ if one of
the following holds:
\begin{itemize}
\item $C$ is a subclass of $D$, or
\item $C$ is a companion object of a class derived from $D$, or
\item $D$ is a companion object of a class from which $C$ is derived.
\end{itemize}

An alternative $A$ is {\em more specific} than an alternative $B$ if
the relative weight of $A$ over $B$ is greater than the relative
weight of $B$ over $A$.

It is an error if there is no alternative in $\CC$ which is more
specific than all other alternatives in $\CC$.

Assume next that $e$ appears as a function in a type application, as
in \lstinline@$e$[$\targs\,$]@. Then all alternatives in
$\AA$ which take the same number of type parameters as there are type
arguments in $\targs$ are chosen. It is an error if no such alternative exists.
If there are several such alternatives, overloading resolution is
applied again to the whole expression \lstinline@$e$[$\targs\,$]@.  

Assume finally that $e$ does not appear as a function in either
an application or a type application. If an expected type is given,
let $\BB$ be the set of those alternatives in $\AA$ which are
compatible (\sref{sec:impl-conv}) to it. Otherwise, let $\BB$ be the same as $\AA$.
We choose in this case the most specific alternative among all
alternatives in $\BB$. It is an error if there is no 
alternative in $\BB$ which is more specific than all other
alternatives in $\BB$.

\example Consider the following definitions:

\begin{lstlisting}
  class A extends B {}
  def f(x: B, y: B) = $\ldots$
  def f(x: A, y: B) = $\ldots$
  val a: A 
  val b: B
\end{lstlisting}
Then the application \lstinline@f(b, b)@ refers to the first
definition of $f$ whereas the application \lstinline@f(a, a)@
refers to the second.  Assume now we add a third overloaded definition
\begin{lstlisting}
  def f(x: B, y: A) = $\ldots$
\end{lstlisting}
Then the application \lstinline@f(a, a)@ is rejected for being ambiguous, since
no most specific applicable signature exists.

\subsection{Local Type Inference}
\label{sec:local-type-inf}

Local type inference infers type arguments to be passed to expressions
of polymorphic type. Say $e$ is of type [$a_1$ >: $L_1$ <: $U_1
\commadots a_n$ >: $L_n$ <: $U_n$]$T$ and no explicit type parameters
are given. 

Local type inference converts this expression to a type
application ~\lstinline@$e$[$T_1 \commadots T_n$]@. The choice of the
type arguments $T_1 \commadots T_n$ depends on the context in which
the expression appears and on the expected type $\proto$. 
There are three cases.

\paragraph{\em Case 1: Selections}
If the expression appears as the prefix of a selection with a name
$x$, then type inference is {\em deferred} to the whole expression
$e.x$. That is, if $e.x$ has type $S$, it is now treated as having
type [$a_1$ >: $L_1$ <: $U_1 \commadots a_n$ >: $L_n$ <: $U_n$]$S$,
and local type inference is applied in turn to infer type arguments 
for $a_1 \commadots a_n$, using the context in which $e.x$ appears.

\paragraph{\em Case 2: Values}
If the expression $e$ appears as a value without being applied to
value arguments, the type arguments are inferred by solving a
constraint system which relates the expression's type $T$ with the
expected type $\proto$. Without loss of generality we can assume that
$T$ is a value type; if it is a method type we apply eta-expansion
(\sref{sec:eta-expand}) to convert it to a function type.  Solving
means finding a substitution $\sigma$ of types $T_i$ for the type
parameters $a_i$ such that
\begin{itemize}
\item
None of inferred types $T_i$ is a singleton type \sref{sec:singleton-types}
\item 
All type parameter bounds are respected, i.e.\ 
$\sigma L_i <: \sigma a_i$ and $\sigma a_i <: \sigma U_i$ for $i = 1 \commadots n$.
\item 
The expression's type conforms to the expected type, i.e.\ 
$\sigma T <: \sigma \proto$.
\end{itemize}
It is a compile time error if no such substitution exists.  
If several substitutions exist, local-type inference will choose for
each type variable $a_i$ a minimal or maximal type $T_i$ of the
solution space.  A {\em maximal} type $T_i$ will be chosen if the type
parameter $a_i$ appears contravariantly (\sref{sec:variances}) in the
type $T$ of the expression.  A {\em minimal} type $T_i$ will be chosen
in all other situations, i.e.\ if the variable appears covariantly,
non-variantly or not at all in the type $T$. We call such a substitution
an {\em optimal solution} of the given constraint system for the type $T$.

\paragraph{\em Case 3: Methods} The last case applies if the expression
$e$ appears in an application $e(d_1 \commadots d_m)$. In that case
$T$ is a method type $(p_1:R_1 \commadots p_m:R_m)T'$. Without loss of
generality we can assume that the result type $T'$ is a value type; if
it is a method type we apply eta-expansion (\sref{sec:eta-expand}) to
convert it to a function type.  One computes first the types $S_j$ of
the argument expressions $d_j$, using two alternative schemes.  Each
argument expression $d_j$ is typed first with the expected type $R_j$,
in which the type parameters $a_1 \commadots a_n$ are taken as type
constants.  If this fails, the argument $d_j$ is typed instead with an
expected type $R_j'$ which results from $R_j$ by replacing every type
parameter in $a_1 \commadots a_n$ with {\sl undefined}.

In a second step, type arguments are inferred by solving a constraint
system which relates the method's type with the expected type
$\proto$ and the argument types $S_1 \commadots S_m$. Solving the
constraint system means
finding a substitution $\sigma$ of types $T_i$ for the type parameters
$a_i$ such that
\begin{itemize}
\item
None of inferred types $T_i$ is a singleton type \sref{sec:singleton-types}
\item 
All type parameter bounds are respected, i.e.\ 
$\sigma L_i <: \sigma a_i$ and $\sigma a_i <: \sigma U_i$ for $i = 1 \commadots n$.
\item 
The method's result type $T'$ conforms to the expected type, i.e.\ 
$\sigma T' <: \sigma \proto$.
\item
Each argument type weakly conforms (\sref{sec:weakconformance}) 
to the corresponding formal parameter
type, i.e.\ 
$\sigma S_j \conforms_w \sigma R_j$ for $j = 1 \commadots m$.
\end{itemize}
It is a compile time error if no such substitution exists.  If several
solutions exist, an optimal one for the type $T'$ is chosen.

All or parts of an expected type $\proto$ may be undefined. The rules for
conformance (\sref{sec:conformance}) are extended to this case by adding
the rule that for any type $T$ the following two statements are always
true:
\[
   \mbox{\sl undefined} <: T \tab\mbox{and}\tab T <: \mbox{\sl undefined} .
\]

It is possible that no minimal or maximal solution for a type variable
exists, in which case a compile-time error results. Because $<:$ is a
pre-order, it is also possible that a solution set has several optimal
solutions for a type. In that case, a Scala compiler is free to pick
any one of them.

\example Consider the two methods:
\begin{lstlisting}
def cons[A](x: A, xs: List[A]): List[A] = x :: xs
def nil[B]: List[B] = Nil
\end{lstlisting}
and the definition
\begin{lstlisting}
val xs = cons(1, nil) .
\end{lstlisting}
The application of \code{cons} is typed with an undefined expected
type. This application is completed by local type inference to 
~\lstinline@cons[Int](1, nil)@. 
Here, one uses the following
reasoning to infer the type argument \lstinline@Int@ for the type
parameter \code{a}:

First, the argument expressions are typed. The first argument \code{1}
has type \code{Int} whereas the second argument \lstinline@nil@ is
itself polymorphic. One tries to type-check \lstinline@nil@ with an
expected type \code{List[a]}. This leads to the constraint system
\begin{lstlisting}
List[b?] <: List[a]
\end{lstlisting}
where we have labeled \lstinline@b?@ with a question mark to indicate
that it is a variable in the constraint system.
Because class \lstinline@List@ is covariant, the optimal
solution of this constraint is
\begin{lstlisting}
b = scala.Nothing .
\end{lstlisting}

In a second step, one solves the following constraint system for
the type parameter \code{a} of \code{cons}:
\begin{lstlisting}
Int <: a?
List[scala.Nothing] <: List[a?]
List[a?] <: $\mbox{\sl undefined}$
\end{lstlisting}
The optimal solution of this constraint system is
\begin{lstlisting}
a = Int ,
\end{lstlisting}
so \code{Int} is the type inferred for \code{a}.

\example Consider now the definition  
\begin{lstlisting}
val ys = cons("abc", xs)
\end{lstlisting}
where \code{xs} is defined of type \code{List[Int]} as before.
In this case local type inference proceeds as follows.

First, the argument expressions are typed. The first argument
\code{"abc"} has type \code{String}. The second argument \code{xs} is
first tried to be typed with expected type \code{List[a]}. This fails,
as \code{List[Int]} is not a subtype of \code{List[a]}. Therefore, 
the second strategy is tried; \code{xs} is now typed with expected type
\lstinline@List[$\mbox{\sl undefined}$]@. This succeeds and yields the argument type
\code{List[Int]}.

In a second step, one solves the following constraint system for
the type parameter \code{a} of \code{cons}:
\begin{lstlisting}
String <: a?
List[Int] <: List[a?]
List[a?] <: $\mbox{\sl undefined}$
\end{lstlisting}
The optimal solution of this constraint system is
\begin{lstlisting}
a = scala.Any ,
\end{lstlisting}
so \code{scala.Any} is the type inferred for \code{a}.

\subsection{Eta Expansion}\label{sec:eta-expand}

  {\em Eta-expansion} converts an expression of method type to an
  equivalent expression of function type. It proceeds in two steps.

  First, one identifes the maximal sub-expressions of $e$; let's
  say these are $e_1 \commadots e_m$. For each of these, one creates a
  fresh name $x_i$. Let $e'$ be the expression resulting from
  replacing every maximal subexpression $e_i$ in $e$ by the
  corresponding fresh name $x_i$. Second, one creates a fresh name $y_i$
  for every argument type $T_i$ of the method ($i = 1 \commadots
  n$). The result of eta-conversion is then:
\begin{lstlisting}
  { val $x_1$ = $e_1$; 
    $\ldots$ 
    val $x_m$ = $e_m$; 
    ($y_1: T_1 \commadots y_n: T_n$) => $e'$($y_1 \commadots y_n$) 
  }
\end{lstlisting}

\subsection{Dynamic Member Selection}\label{sec:dyn-mem-sel}

The standard Scala library defines a trait \lstinline@scala.Dynamic@ which defines a member
\@invokeDynamic@ as follows:
\begin{lstlisting}
package scala
trait Dynamic {
  def applyDynamic (name: String, args: Any*): Any
  ...
}
\end{lstlisting}
Assume a selection of the form $e.x$ where the type of $e$ conforms to \lstinline@scala.Dynamic@.
Further assuming the selection is not followed by any function arguments, such an expression can be rewitten under the conditions given in \sref{sec:impl-conv} to:
\begin{lstlisting}
$e$.applyDynamic("$x$")
\end{lstlisting}
If the selection is followed by some arguments, e.g.\ $e.x(\args)$, then that expression
is rewritten to
\begin{lstlisting}
$e$.applyDynamic("$x$", $\args$)
\end{lstlisting}

