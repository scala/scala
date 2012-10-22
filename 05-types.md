Types
=====

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.grammar}
  Type              ::=  FunctionArgTypes ‘=>’ Type
                      |  InfixType [ExistentialClause]
  FunctionArgTypes  ::= InfixType
                      | ‘(’ [ ParamType {‘,’ ParamType } ] ‘)’
  ExistentialClause ::=  ‘forSome’ ‘{’ ExistentialDcl {semi ExistentialDcl} ‘}’
  ExistentialDcl    ::=  ‘type’ TypeDcl 
                      |  ‘val’ ValDcl
  InfixType         ::=  CompoundType {id [nl] CompoundType}
  CompoundType      ::=  AnnotType {‘with’ AnnotType} [Refinement]
                      |  Refinement
  AnnotType         ::=  SimpleType {Annotation}
  SimpleType        ::=  SimpleType TypeArgs
                      |  SimpleType ‘#’ id
                      |  StableId
                      |  Path ‘.’ ‘type’
                      |  ‘(’ Types ‘)’
  TypeArgs          ::=  ‘[’ Types ‘]’
  Types             ::=  Type {‘,’ Type}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We distinguish between first-order types and type constructors, which
take type parameters and yield types. A subset of first-order types
called _value types_ represents sets of (first-class) values.
Value types are either _concrete_ or _abstract_. 

Every concrete value type can be represented as a _class type_, i.e. a
[type designator](#type-designators) that refers to a
a [class or a trait](#class-definitions) [^1], or as a
[compound type](#compound-types) representing an
intersection of types, possibly with a [refinement](#compound-types)
that further constrains the types of its members.
<!-- 
A shorthand exists for denoting function types (\sref{sec:function-types}). 
-->
Abstract value types are introduced by [type parameters](#type-parameters)
and [abstract type bindings](#type-declarations-and-type-aliases). 
Parentheses in types can be used for grouping.

[^1]: We assume that objects and packages also implicitly
      define a class (of the same name as the object or package, but
      inaccessible to user programs).

Non-value types capture properties of identifiers that 
[are not values](#non-value-types). For example, a 
[type constructor](#type-constructors) does not directly specify a type of 
values. However, when a type constructor is applied to the correct type 
arguments, it yields a first-order type, which may be a value type. 

Non-value types are expressed indirectly in Scala. E.g., a method type is 
described by writing down a method signature, which in itself is not a real 
type, although it  gives rise to a corresponding [method type](#method-types). 
Type constructors are another example, as one can write 
`type Swap[m[_, _], a,b] = m[b, a]`{.scala}, but there is no syntax to write 
the corresponding anonymous type function directly.


Paths
-----

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.grammar}
Path            ::=  StableId
                  |  [id ‘.’] this
StableId        ::=  id
                  |  Path ‘.’ id
                  |  [id ‘.’] ‘super’ [ClassQualifier] ‘.’ id
ClassQualifier  ::= ‘[’ id ‘]’
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Paths are not types themselves, but they can be a part of named types
and in that function form a central role in Scala's type system.

A path is one of the following.

- The empty path ε (which cannot be written explicitly in user programs).
- `C.this`, where _C_ references a class. 
  The path `this` is taken as a shorthand for `C.this` where 
  _C_ is the name of the class directly enclosing the reference. 
- `p.x` where _p_ is a path and _x_ is a stable member of _p_.
  _Stable members_ are packages or members introduced by object definitions or 
  by value definitions of [non-volatile types](#volatile-types).
- `C.super.x` or `C.super[M].x`
  where _C_ references a class and _x_ references a 
  stable member of the super class or designated parent class _M_ of _C_. 
  The prefix `super`{.scala} is taken as a shorthand for `C.super` where 
  _C_ is the name of the class directly enclosing the reference. 


A _stable identifier_ is a path which ends in an identifier.

Value Types
-----------

Every value in Scala has a type which is of one of the following
forms.

### Singleton Types

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.grammar}
SimpleType  ::=  Path ‘.’ type
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A singleton type is of the form `p.type`{.scala}, where _p_ is a
path pointing to a value expected to [conform](#expression-typing)
to `scala.AnyRef`{.scala}. The type denotes the set of values
consisting of `null`{.scala} and the value denoted by _p_. 

A _stable type_ is either a singleton type or a type which is
declared to be a subtype of trait `scala.Singleton`{.scala}.

### Type Projection

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.grammar}
SimpleType  ::=  SimpleType ‘#’ id
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A type projection `T#x`{.scala} references the type member named
_x_ of type _T_. 

<!--
The following is no longer necessary:
If $x$ references an abstract type member, then $T$ must be a stable type (\sref{sec:singleton-types}). 
-->

### Type Designators

~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
SimpleType  ::=  StableId
~~~~~~~~~~~~~~~~~~~~~~~~~~

A type designator refers to a named value type. It can be simple or
qualified. All such type designators are shorthands for type projections.

Specifically, the unqualified type name _t_ where _t_ is bound in some
class, object, or package _C_ is taken as a shorthand for
`C.this.type#t`{.scala}. If _t_ is
not bound in a class, object, or package, then _t_ is taken as a
shorthand for `ε.type#t`.

A qualified type designator has the form `p.t` where `p` is
a [path](#paths) and _t_ is a type name. Such a type designator is
equivalent to the type projection `p.type#t`{.scala}.

(@) Some type designators and their expansions are listed below. We assume
    a local type parameter _t_, a value `maintable`
    with a type member `Node` and the standard class `scala.Int`, 

    --------------------  --------------------------
    t                     $\epsilon$.type#t
    Int                   scala.type#Int
    scala.Int             scala.type#Int
    data.maintable.Node   data.maintable.type#Node
    --------------------  --------------------------


### Parameterized Types

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.grammar}
SimpleType      ::=  SimpleType TypeArgs
TypeArgs        ::=  ‘[’ Types ‘]’
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A parameterized type T[ U~1~ , … , U~n~ ] consists of a type
designator _T_ and type parameters _U~1~ , … , U~n~_ where 
_n ≥ 1_. _T_ must refer to a type constructor which takes _n_ type
parameters _a~1~ , … , s a~n~_.

Say the type parameters have lower bounds _L~1~ , … , L~n~_ and
upper bounds _U~1~ … U~n~_.  The parameterized type is
well-formed if each actual type parameter
_conforms to its bounds_, i.e. _σ L~i~ <: T~i~ <: σ U~i~_ where σ is the
substitution [ _a~1~_ := _T~1~_ , … , _a~n~_ := _T~n~_ ].

(@param-types) Given the partial type definitions:

    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    class TreeMap[A <: Comparable[A], B] { … }
    class List[A] { … }
    class I extends Comparable[I] { … }
    
    class F[M[_], X] { … }
    class S[K <: String] { … }
    class G[M[ Z <: I ], I] { … }
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    the following parameterized types are well formed:

    ~~~~~~~~~~~~~~~~~~~~~~ {.scala}
      TreeMap[I, String]
      List[I]
      List[List[Boolean]]
      
      F[List, Int]
      G[S, String]
    ~~~~~~~~~~~~~~~~~~~~~~

(@) Given the type definitions of (@param-types),
    the following types are ill-formed:

    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    TreeMap[I]            // illegal: wrong number of parameters
    TreeMap[List[I], Int] // illegal: type parameter not within bound
    
    F[Int, Boolean]       // illegal: Int is not a type constructor
    F[TreeMap, Int]       // illegal: TreeMap takes two parameters,
                          //   F expects a constructor taking one
    G[S, Int]             // illegal: S constrains its parameter to
                          //   conform to String,
                          // G expects type constructor with a parameter
                          //   that conforms to Int
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Tuple Types

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.grammar}
SimpleType    ::=   ‘(’ Types ‘)’
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A tuple type (T~1~ , … , T~n~) is an alias for the
class `scala.Tuple`~n~`[`T~1~`, … , `T~n~`]`, where _n ≥ 2_.

Tuple classes are case classes whose fields can be accessed using
selectors `_1` , … , `_n`. Their functionality is
abstracted in a corresponding `Product` trait. The _n_-ary tuple
class and product trait are defined at least as follows in the
standard Scala library (they might also add other methods and
implement other traits).

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
case class Tuple_n[+T1, … , +Tn](_1: T1, … , _n: Tn) 
extends Product_n[T1, … , Tn] {}

trait Product_n[+T1, … , +Tn] {
  override def arity = n
  def _1: T1
  …
  def _n: Tn
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Annotated Types

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.grammar}
AnnotType  ::=  SimpleType {Annotation}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

An annotated type _T a~1~ , … , a~n~_
attaches [annotations](#user-defined-annotations) 
_a~1~ , … , a~n~_ to the type _T_.

(@) The following type adds the `@suspendable`{.scala} annotation to the type
    `String`{.scala}:

    ~~~~~~~~~~~~~~~~~~~~ {.scala}
    String @suspendable
    ~~~~~~~~~~~~~~~~~~~~


### Compound Types

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.grammar}
CompoundType    ::=  AnnotType {‘with’ AnnotType} [Refinement]
                  |  Refinement
Refinement      ::=  [nl] ‘{’ RefineStat {semi RefineStat} ‘}’
RefineStat      ::=  Dcl
                  |  ‘type’ TypeDef
                  |
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A compound type `T1 with … with Tn { R }`
represents objects with members as given in the component types 
_T1 , … , Tn_ and the refinement `{ R }`. A refinement
`{ R }` contains declarations and type definitions. 
If a declaration or definition overrides a declaration or definition in
one of the component types _T1 , … , T_n_, the usual rules for
[overriding](#overriding) apply; otherwise the declaration
or definition is said to be “structural” [^2].

[^2]: A reference to a structurally defined member (method call or access 
to a value or variable) may generate binary code that is significantly 
slower than an equivalent code to a non-structural member.

Within a method declaration in a structural refinement, the type of
any value parameter may only refer to type parameters or abstract
types that are contained inside the refinement. That is, it must refer
either to a type parameter of the method itself, or to a type
definition within the refinement. This restriction does not apply to
the function's result type.

If no refinement is given, the empty refinement is implicitly added,
i.e.\  `T1 with … with Tn`{.scala} is a shorthand for
`T1 with … with Tn {}`{.scala}.

A compound type may also consist of just a refinement
`{ R }` with no preceding component types. Such a type is
equivalent to `AnyRef{ R }`{.scala}.

(@) The following example shows how to declare and use a function which 
    parameter's type contains a refinement with structural declarations.

    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    case class Bird (val name: String) extends Object {
    	def fly(height: Int) = …
  	…
    }
    case class Plane (val callsign: String) extends Object {
    	def fly(height: Int) = …
  	…
    }
    def takeoff(
    	    runway: Int,
          r: { val callsign: String; def fly(height: Int) }) = {
      tower.print(r.callsign + " requests take-off on runway " + runway)
      tower.read(r.callsign + " is clear for take-off")
      r.fly(1000)
    }
    val bird = new Bird("Polly the parrot"){ val callsign = name }
    val a380 = new Plane("TZ-987")
    takeoff(42, bird)
    takeoff(89, a380)
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    Although `Bird` and `Plane` do not share any parent class other than 
    `Object`, the parameter _r_ of function `takeoff` is defined using a 
    refinement with structural declarations to accept any object that declares 
    a value `callsign` and a `fly` function.

 
### Infix Types

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.grammar}
InfixType     ::=  CompoundType {id [nl] CompoundType}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

An infix type _T~1~ op T~2~_ consists of an infix
operator _op_ which gets applied to two type operands _T~1~_ and
_T~2~_.  The type is equivalent to the type application 
`op[T₁, T₂]`.  The infix operator _op_ may be an arbitrary identifier,
except for `*`, which is reserved as a postfix modifier 
denoting a [repeated parameter type](#repeated-parameters). 

All type infix operators have the same precedence; parentheses have to
be used for grouping. The [associativity](#prefix-infix-and-postfix-operations)
of a type operator is determined as for term operators: type operators
ending in a colon ‘:’ are right-associative; all other
operators are left-associative.

In a sequence of consecutive type infix operations 
$t_0 \, op \, t_1 \, op_2 \, … \, op_n \, t_n$,
all operators $\op_1 , … , \op_n$ must have the same
associativity. If they are all left-associative, the sequence is
interpreted as `(… (t_0 op_1 t_1) op_2 …) op_n t_n`,
otherwise it is interpreted as $t_0 op_1 (t_1 op_2 ( … op_n t_n) …)$.

### Function Types

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
Type              ::=  FunctionArgs ‘=>’ Type
FunctionArgs      ::=  InfixType
                    |  ‘(’ [ ParamType {‘,’ ParamType } ] ‘)’
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The type $(T_1 , \ldots , T_n) \Rightarrow U$ represents the set of function
values that take arguments of types $T1 , \ldots , Tn$ and yield
results of type $U$.  In the case of exactly one argument type
$T \Rightarrow U$ is a shorthand for $(T) \Rightarrow U$.  
An argument type of the form $\Rightarrow T$
represents a [call-by-name parameter](#by-name-parameters) of type $T$.

Function types associate to the right, e.g.
`S => T => U` is the same as `S => (T => U)`.

Function types are shorthands for class types that define `apply`
functions.  Specifically, the $n$-ary function type 
`(T1 , … , Tn) => U` is a shorthand for the class type
`Function_n[T1 , … , Tn, U]`. Such class
types are defined in the Scala library for $n$ between 0 and 9 as follows.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
package scala 
trait Function_n[-T1 , … , -Tn, +R] {
  def apply(x1: T1 , … , xn: Tn): R 
  override def toString = "<function>" 
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Hence, function types are [covariant](#variance-annotations) in their
result type and contravariant in their argument types.

### Existential Types

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.grammar}
Type               ::= InfixType ExistentialClauses
ExistentialClauses ::= ‘forSome’ ‘{’ ExistentialDcl 
                       {semi ExistentialDcl} ‘}’
ExistentialDcl     ::= ‘type’ TypeDcl 
                    |  ‘val’ ValDcl
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

An existential type has the form `T forSome { Q }`
where _Q_ is a sequence of 
[type declarations](#type-declarations-and-type-aliases).

Let $t_1[\mathit{tps}_1] >: L_1 <: U_1 , \ldots , t_n[\mathit{tps}_n] >: L_n <: U_n$ 
be the types declared in $Q$ (any of the 
type parameter sections [ _tps~i~_ ] might be missing).
The scope of each type _t~i~_ includes the type _T_ and the existential clause _Q_. 
The type variables _t~i~_ are said to be _bound_ in the type `T forSome { Q }`.
Type variables which occur in a type _T_ but which are not bound in _T_ are said
to be _free_ in _T_.

%%% iainmcgin: to here

A _type instance_ of ~\lstinline@$T$ forSome {$\,Q\,$}@ 
is a type $\sigma T$ where $\sigma$ is a substitution over $t_1 \commadots t_n$
such that, for each $i$, $\sigma L_i \conforms \sigma t_i \conforms \sigma U_i$.
The set of values denoted by the existential type ~\lstinline@$T$ forSome {$\,Q\,$}@~
is the union of the set of values of all its type instances.

A _skolemization_ of ~\lstinline@$T$ forSome {$\,Q\,$}@~ is
a type instance $\sigma T$, where $\sigma$ is the substitution
$[t'_1/t_1 \commadots t'_n/t_n]$ and each $t'_i$ is a fresh abstract type
with lower bound $\sigma L_i$ and upper bound $\sigma U_i$.

#### Simplification Rules

Existential types obey the following four equivalences:

#. Multiple for-clauses in an existential type can be merged. E.g.,
~\lstinline@$T$ forSome {$\,Q\,$} forSome {$\,Q'\,$}@~
is equivalent to
~\lstinline@$T$ forSome {$\,Q\,$;$\,Q'\,$}@.
#. Unused quantifications can be dropped. E.g.,
~\lstinline@$T$ forSome {$\,Q\,$;$\,Q'\,$}@~
where none of the types defined in $Q'$ are referred to by $T$ or $Q$,
is equivalent to
~\lstinline@$T$ forSome {$\,Q\,$}@.
#. An empty quantification can be dropped. E.g.,
~\lstinline@$T$ forSome { }@~ is equivalent to ~\lstinline@$T$@.
#. An existential type ~\lstinline@$T$ forSome {$\,Q\,$}@~ where $Q$ contains
a clause ~\lstinline@type $t[\tps] >: L <: U$@ is equivalent 
to the type ~\lstinline@$T'$ forSome {$\,Q\,$}@~ where $T'$ results from $T$ by replacing every
covariant occurrence (\sref{sec:variances}) of $t$ in $T$ by $U$ and by replacing every
contravariant occurrence of $t$ in $T$ by $L$.


#### Existential Quantification over Values

As a syntactic convenience, the bindings clause
in an existential type may also contain
value declarations \lstinline@val $x$: $T$@. 
An existential type ~\lstinline@$T$ forSome { $Q$; val $x$: $S\,$;$\,Q'$ }@~
is treated as a shorthand for the type
~\lstinline@$T'$ forSome { $Q$; type $t$ <: $S$ with Singleton; $Q'$ }@, where $t$ is a fresh
type name and $T'$ results from $T$ by replacing every occurrence of 
\lstinline@$x$.type@ with $t$.

#### Placeholder Syntax for Existential Types

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.grammar}
WildcardType   ::=  ‘_’ TypeBounds
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Scala supports a placeholder syntax for existential types.
A _wildcard type_ is of the form ~\lstinline@_$\;$>:$\,L\,$<:$\,U$@. Both bound
clauses may be omitted. If a lower bound clause \lstinline@>:$\,L$@ is missing, 
\lstinline@>:$\,$scala.Nothing@~
is assumed. If an upper bound clause ~\lstinline@<:$\,U$@ is missing, 
\lstinline@<:$\,$scala.Any@~ is assumed. A wildcard type is a shorthand for an 
existentially quantified type variable, where the existential quantification is implicit.

A wildcard type must appear as type argument of a parameterized type.
Let $T = p.c[\targs,T,\targs']$ be a parameterized type where $\targs, \targs'$ may be empty and
$T$ is a wildcard type ~\lstinline@_$\;$>:$\,L\,$<:$\,U$@. Then $T$ is equivalent to the existential
type

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
$p.c[\targs,t,\targs']$ forSome { type $t$ >: $L$ <: $U$ }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

where $t$ is some fresh type variable. 
Wildcard types may also appear as parts of infix types
(\sref{sec:infix-types}), function types (\sref{sec:function-types}),
or tuple types (\sref{sec:tuple-types}).
Their expansion is then the expansion in the equivalent parameterized
type.

(@) Assume the class definitions
    
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    class Ref[T]
    abstract class Outer { type T } .
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    Here are some examples of existential types:
    
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    Ref[T] forSome { type T <: java.lang.Number }
    Ref[x.T] forSome { val x: Outer }
    Ref[x_type # T] forSome { type x_type <: Outer with Singleton }
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    The last two types in this list are equivalent.
    An alternative formulation of the first type above using wildcard syntax is:
    
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    Ref[_ <: java.lang.Number]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~

(@) The type `List[List[_]]` is equivalent to the existential type
    
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    List[List[t] forSome { type t }] . 
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(@) Assume a covariant type
    
    ~~~~~~~~~~~~~~~ {.scala}
    class List[+T]
    ~~~~~~~~~~~~~~~

    The type
    
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    List[T] forSome { type T <: java.lang.Number }
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    is equivalent (by simplification rule 4 above) to

    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    List[java.lang.Number] forSome { type T <: java.lang.Number }
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    which is in turn equivalent (by simplification rules 2 and 3 above) to
    `List[java.lang.Number]`.


Non-Value Types
---------------

The types explained in the following do not denote sets of values, nor
do they appear explicitly in programs. They are introduced in this
report as the internal types of defined identifiers.


### Method Types

A method type is denoted internally as $(\Ps)U$, where $(\Ps)$ is a
sequence of parameter names and types $(p_1:T_1 \commadots p_n:T_n)$
for some $n \geq 0$ and $U$ is a (value or method) type.  This type
represents named methods that take arguments named $p_1 \commadots p_n$ 
of types $T_1 \commadots T_n$
and that return a result of type $U$.

Method types associate to the right: $(\Ps_1)(\Ps_2)U$ is
treated as $(\Ps_1)((\Ps_2)U)$.

A special case are types of methods without any parameters. They are
written here \lstinline@=> T@. Parameterless methods name expressions
that are re-evaluated each time the parameterless method name is
referenced.

Method types do not exist as types of values. If a method name is used
as a value, its type is implicitly converted to a corresponding
function type (\sref{sec:impl-conv}).

(@) The declarations
    
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    def a: Int
    def b (x: Int): Boolean
    def c (x: Int) (y: String, z: String): String
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    produce the typings
    
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    a: => Int
    b: (Int) Boolean
    c: (Int) (String, String) String
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Polymorphic Method Types


A polymorphic method type is denoted internally as ~\lstinline@[$\tps\,$]$T$@~ where
\lstinline@[$\tps\,$]@ is a type parameter section 
~\lstinline@[$a_1$ >: $L_1$ <: $U_1 \commadots a_n$ >: $L_n$ <: $U_n$]@~ 
for some $n \geq 0$ and $T$ is a
(value or method) type.  This type represents named methods that
take type arguments ~\lstinline@$S_1 \commadots S_n$@~ which
conform (\sref{sec:param-types}) to the lower bounds
~\lstinline@$L_1 \commadots L_n$@~ and the upper bounds
~\lstinline@$U_1 \commadots U_n$@~ and that yield results of type $T$.

(@) The declarations

    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    def empty[A]: List[A]
    def union[A <: Comparable[A]] (x: Set[A], xs: Set[A]): Set[A]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    produce the typings

    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    empty : [A >: Nothing <: Any] List[A]
    union : [A >: Nothing <: Comparable[A]] (x: Set[A], xs: Set[A]) Set[A]  .
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Type Constructors

A type constructor is represented internally much like a polymorphic method type.
~\lstinline@[$\pm$ $a_1$ >: $L_1$ <: $U_1 \commadots \pm a_n$ >: $L_n$ <: $U_n$] $T$@~ represents a type that is expected by a type constructor parameter (\sref{sec:type-params}) or an abstract type constructor binding (\sref{sec:typedcl}) with the corresponding type parameter clause.

(@) Consider this fragment of the `Iterable[+X]`{.scala} class:
    
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    trait Iterable[+X] {
      def flatMap[newType[+X] <: Iterable[X], S](f: X => newType[S]): newType[S]
    }
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    Conceptually, the type constructor `Iterable` is a name for the 
    anonymous type `[+X] Iterable[X]`, which may be passed to the 
    `newType` type constructor parameter in `flatMap`.


Base Types and Member Definitions
---------------------------------

Types of class members depend on the way the members are referenced.
Central here are three notions, namely:
#. the notion of the set of base types of a type $T$,
#. the notion of a type $T$ in some class $C$ seen from some 
   prefix type $S$,
#. the notion of the set of member bindings of some type $T$.


These notions are defined mutually recursively as follows.

#. The set of _base types_ of a type is a set of class types, 
   given as follows.

    - The base types of a class type $C$ with parents $T_1 \commadots T_n$ are
      $C$ itself, as well as the base types of the compound type
      ~\lstinline@$T_1$ with $\ldots$ with $T_n$ {$R\,$}@.
    - The base types of an aliased type are the base types of its alias.
    - The base types of an abstract type are the base types of its upper bound.
    - The base types of a parameterized type 
      ~\lstinline@$C$[$T_1 \commadots T_n$]@~ are the base types
      of type $C$, where every occurrence of a type parameter $a_i$ 
      of $C$ has been replaced by the corresponding parameter type $T_i$.
    - The base types of a singleton type \lstinline@$p$.type@ are the base types of
      the type of $p$.
    - The base types of a compound type 
      ~\lstinline@$T_1$ with $\ldots$ with $T_n$ {$R\,$}@~ 
      are the _reduced union_ of the base
      classes of all $T_i$'s. This means: 
      Let the multi-set $\SS$ be the multi-set-union of the
      base types of all $T_i$'s.
      If $\SS$ contains several type instances of the same class, say
      ~\lstinline@$S^i$#$C$[$T^i_1 \commadots T^i_n$]@~ $(i \in I)$, then
      all those instances 
      are replaced by one of them which conforms to all
      others. It is an error if no such instance exists. It follows that the reduced union, if it exists,
      produces a set of class types, where different types are instances of different classes.
    - The base types of a type selection \lstinline@$S$#$T$@ are
      determined as follows. If $T$ is an alias or abstract type, the
      previous clauses apply. Otherwise, $T$ must be a (possibly
      parameterized) class type, which is defined in some class $B$.  Then
      the base types of \lstinline@$S$#$T$@ are the base types of $T$
      in $B$ seen from the prefix type $S$.
    - The base types of an existential type \lstinline@$T$ forSome {$\,Q\,$}@ are
      all types \lstinline@$S$ forSome {$\,Q\,$}@ where $S$ is a base type of $T$.

2. The notion of a type $T$
{\em in class $C$ seen from some prefix type
$S\,$} makes sense only if the prefix type $S$
has a type instance of class $C$ as a base type, say
~\lstinline@$S'$#$C$[$T_1 \commadots T_n$]@. Then we define as follows.
\begin{itemize}
 \item 
  If \lstinline@$S$ = $\epsilon$.type@, then $T$ in $C$ seen from $S$ is $T$ itself.
 \item
  Otherwise, if $S$ is an existential type ~\lstinline@$S'$ forSome {$\,Q\,$}@, and
  $T$ in $C$ seen from $S'$ is $T'$, 
  then $T$ in $C$ seen from $S$ is ~\lstinline@$T'$ forSome {$\,Q\,$}@.
 \item 
   Otherwise, if $T$ is the $i$'th type parameter of some class $D$, then
   \begin{itemize}
   \item
   If $S$ has a base type ~\lstinline@$D$[$U_1 \commadots U_n$]@, for some type parameters
   ~\lstinline@[$U_1 \commadots U_n$]@, then $T$ in $C$ seen from $S$ is $U_i$.
   \item
   Otherwise, if $C$ is defined in a class $C'$, then
   $T$ in $C$ seen from $S$ is the same as $T$ in $C'$ seen from $S'$.
   \item
   Otherwise, if $C$ is not defined in another class, then  
   $T$ in $C$ seen from $S$ is $T$ itself.
  \end{itemize}
\item
   Otherwise, 
   if $T$ is the singleton type \lstinline@$D$.this.type@ for some class $D$
   then
   \begin{itemize}
   \item
   If $D$ is a subclass of $C$ and 
   $S$ has a type instance of class $D$ among its base types,
   then $T$ in $C$ seen from $S$ is $S$.
   \item
   Otherwise, if $C$ is defined in a class $C'$, then
   $T$ in $C$ seen from $S$ is the same as $T$ in $C'$ seen from $S'$.
   \item
   Otherwise, if $C$ is not defined in another class, then  
   $T$ in $C$ seen from $S$ is $T$ itself.
   \end{itemize}
\item
  If $T$ is some other type, then the described mapping is performed
  to all its type components.
\end{itemize}

If $T$ is a possibly parameterized class type, where $T$'s class
is defined in some other class $D$, and $S$ is some prefix type,
then we use ``$T$ seen from $S$'' as a shorthand for
``$T$ in $D$ seen from $S$''.

3. The _member bindings_ of a type $T$ are (1) all bindings $d$ such that
there exists a type instance of some class $C$ among the base types of $T$
and there exists a definition or declaration $d'$ in $C$
such that $d$ results from $d'$ by replacing every
type $T'$ in $d'$ by $T'$ in $C$ seen from $T$, and (2) all bindings
of the type's refinement (\sref{sec:refinements}), if it has one.

The _definition_ of a type projection \lstinline@$S$#$t$@ is the member
binding $d_t$ of the type $t$ in $S$. In that case, we also say
that \lstinline@$S$#$t$@ _is defined by_ $d_t$.
share a to


Relations between types
-----------------------

We define two relations between types.
\begin{quote}\begin{tabular}{l@{\gap}l@{\gap}l}
\em Type equivalence & $T \equiv U$ & $T$ and $U$ are interchangeable
in all contexts.
\\
\em Conformance & $T \conforms U$ & Type $T$ conforms to type $U$.
\end{tabular}\end{quote}

### Type Equivalence
\label{sec:type-equiv}

Equivalence $(\equiv)$ between types is the smallest congruence\footnote{ A
congruence is an equivalence relation which is closed under formation
of contexts} such that the following holds:
\begin{itemize}
\item 
If $t$ is defined by a type alias ~\lstinline@type $t$ = $T$@, then $t$ is
equivalent to $T$.
\item
If a path $p$ has a singleton type ~\lstinline@$q$.type@, then
~\lstinline@$p$.type $\equiv q$.type@.
\item
If $O$ is defined by an object definition, and $p$ is a path
consisting only of package or object selectors and ending in $O$, then
~\lstinline@$O$.this.type $\equiv p$.type@.
\item
Two compound types (\sref{sec:compound-types}) are equivalent if the sequences of their component
are pairwise equivalent, and occur in the same order, and their
refinements are equivalent. Two refinements are equivalent if they
bind the same names and the modifiers, types and bounds of every
declared entity are equivalent in both refinements.
\item
Two method types (\sref{sec:method-types}) are equivalent if they have equivalent result types,
both have the same number of parameters, and corresponding parameters
have equivalent types.  Note that the names of parameters do not
matter for method type equivalence.
\item 
Two polymorphic method types (\sref{sec:poly-types}) are equivalent if they have the same number of
type parameters, and, after renaming one set of type parameters by
another, the result types as well as lower and upper bounds of
corresponding type parameters are equivalent.
\item 
Two existential types (\sref{sec:existential-types}) 
are equivalent if they have the same number of
quantifiers, and, after renaming one list of type quantifiers by
another, the quantified types as well as lower and upper bounds of
corresponding quantifiers are equivalent.
\item %@M
Two type constructors (\sref{sec:higherkinded-types}) are equivalent if they have the 
same number of type parameters, and, after renaming one list of type parameters by
another, the result types as well as variances, lower and upper bounds of
corresponding type parameters are equivalent.
\end{itemize}

### Conformance
\label{sec:conformance}

The conformance relation $(\conforms)$ is the smallest 
transitive relation that satisfies the following conditions.
\begin{itemize}
\item Conformance includes equivalence. If $T \equiv U$ then $T \conforms U$.
\item For every value type $T$, 
      $\mbox{\code{scala.Nothing}} \conforms T \conforms \mbox{\code{scala.Any}}$. 
\item For every type constructor $T$ (with any number of type parameters), 
      $\mbox{\code{scala.Nothing}} \conforms T \conforms \mbox{\code{scala.Any}}$. %@M
      
\item For every class type $T$ such that $T \conforms
  \mbox{\code{scala.AnyRef}}$ and not $T \conforms \mbox{\code{scala.NotNull}}$
        one has $\mbox{\code{scala.Null}} \conforms T$.
\item A type variable or abstract type $t$ conforms to its upper bound and
      its lower bound conforms to $t$. 
\item A class type or parameterized type conforms to any of its
  base-types.
\item A singleton type \lstinline@$p$.type@ conforms to the type of
  the path $p$.
\item A singleton type \lstinline@$p$.type@ conforms to the type $\mbox{\code{scala.Singleton}}$.
\item A type projection \lstinline@$T$#$t$@ conforms to \lstinline@$U$#$t$@ if 
      $T$ conforms to $U$.
\item A parameterized type ~\lstinline@$T$[$T_1 \commadots T_n$]@~ conforms to 
      ~\lstinline@$T$[$U_1 \commadots U_n$]@~ if
      the following three conditions hold for $i = 1 \commadots n$. 
      \begin{itemize}
      \item
      If the $i$'th type parameter of $T$ is declared covariant, then $T_i \conforms U_i$.
      \item
      If the $i$'th type parameter of $T$ is declared contravariant, then $U_i \conforms T_i$.
      \item
      If the $i$'th type parameter of $T$ is declared neither covariant 
      nor contravariant, then $U_i \equiv T_i$.
      \end{itemize}
\item A compound type ~\lstinline@$T_1$ with $\ldots$ with $T_n$ {$R\,$}@~ conforms to
      each of its component types $T_i$.
\item If $T \conforms U_i$ for $i = 1 \commadots n$ and for every
      binding $d$ of a type or value $x$ in $R$ there exists a member
      binding of $x$ in $T$ which subsumes $d$, then $T$ conforms to the
      compound type ~\lstinline@$U_1$ with $\ldots$ with $U_n$ {$R\,$}@.
\item The existential type ~\lstinline@$T$ forSome {$\,Q\,$}@ conforms to 
      $U$ if its skolemization (\sref{sec:existential-types})
      conforms to $U$.
\item The type $T$ conforms to the existential type ~\lstinline@$U$ forSome {$\,Q\,$}@ 
      if $T$ conforms to one of the type instances (\sref{sec:existential-types}) 
      of ~\lstinline@$U$ forSome {$\,Q\,$}@.
\item If
        $T_i \equiv T'_i$ for $i = 1 \commadots n$ and $U$ conforms to $U'$ 
        then the method type $(p_1:T_1 \commadots p_n:T_n) U$ conforms to
        $(p'_1:T'_1 \commadots p'_n:T'_n) U'$.
\item The polymorphic type
$[a_1 >: L_1 <: U_1 \commadots a_n >: L_n <: U_n] T$ conforms to the polymorphic type
$[a_1 >: L'_1 <: U'_1 \commadots a_n >: L'_n <: U'_n] T'$ if, assuming
$L'_1 \conforms a_1 \conforms U'_1 \commadots L'_n \conforms a_n \conforms U'_n$ 
one has $T \conforms T'$ and $L_i \conforms L'_i$ and $U'_i \conforms U_i$
for $i = 1 \commadots n$.
%@M
\item Type constructors $T$ and $T'$ follow a similar discipline. We characterize $T$ and $T'$ by their type parameter clauses
$[a_1 \commadots a_n]$ and
$[a'_1 \commadots a'_n ]$, where an $a_i$ or $a'_i$ may include a variance annotation, a higher-order type parameter clause, and bounds. Then, $T$ conforms to $T'$ if any list $[t_1 \commadots t_n]$ -- with declared variances, bounds and higher-order type parameter clauses -- of valid type arguments for $T'$ is also a valid list of type arguments for $T$ and $T[t_1 \commadots t_n] \conforms T'[t_1 \commadots t_n]$. Note that this entails that:
      \begin{itemize}
      \item
      The bounds on $a_i$ must be weaker than the corresponding bounds declared for $a'_i$. 
      \item 
      The variance of $a_i$ must match the variance of $a'_i$, where covariance matches covariance, contravariance matches contravariance and any variance matches invariance.
      \item 
      Recursively, these restrictions apply to the corresponding higher-order type parameter clauses of $a_i$ and $a'_i$.
      \end{itemize}

\end{itemize}

A declaration or definition in some compound type of class type $C$
_subsumes_ another
declaration of the same name in some compound type or class type $C'$, if one of the following holds.
\begin{itemize}
\item
A value declaration or definition that defines a name $x$ with type $T$ subsumes 
a value or method declaration that defines $x$ with type $T'$, provided $T \conforms T'$.
\item 
A method declaration or definition that defines a name $x$ with type $T$ subsumes 
a method declaration that defines $x$ with type $T'$, provided $T \conforms T'$.
\item
A type alias
$\TYPE;t[T_1 \commadots T_n]=T$ subsumes a type alias $\TYPE;t[T_1 \commadots T_n]=T'$ if %@M
$T \equiv T'$. 
\item 
A type declaration ~\lstinline@type $t$[$T_1 \commadots T_n$] >: $L$ <: $U$@~ subsumes %@M
a type declaration ~\lstinline@type $t$[$T_1 \commadots T_n$] >: $L'$ <: $U'$@~ if $L' \conforms L$ and %@M
$U \conforms U'$.
\item
A type or class definition that binds a type name $t$ subsumes an abstract
type declaration ~\lstinline@type t[$T_1 \commadots T_n$] >: L <: U@~ if %@M
$L \conforms t \conforms U$.
\end{itemize}

The $(\conforms)$ relation forms pre-order between types,
i.e.\ it is transitive and reflexive. _least upper bounds_ and _greatest lower bounds_ of a set of types
are understood to be relative to that order.

\paragraph{Note} The least upper bound or greatest lower bound 
of a set of types does not always exist. For instance, consider
the class definitions
\begin{lstlisting}
class A[+T] {}
class B extends A[B] 
class C extends A[C] 
\end{lstlisting}
Then the types ~\lstinline@A[Any], A[A[Any]], A[A[A[Any]]], ...@~ form
a descending sequence of upper bounds for \code{B} and \code{C}. The
least upper bound would be the infinite limit of that sequence, which
does not exist as a Scala type. Since cases like this are in general
impossible to detect, a Scala compiler is free to reject a term
which has a type specified as a least upper or greatest lower bound,
and that bound would be more complex than some compiler-set
limit\footnote{The current Scala compiler limits the nesting level
of parameterization in such bounds to be at most two deeper than the maximum
nesting level of the operand types}.

The least upper bound or greatest lower bound might also not be
unique. For instance \code{A with B} and \code{B with A} are both
greatest lower of \code{A} and \code{B}. If there are several
least upper bounds or greatest lower bounds, the Scala compiler is
free to pick any one of them.

### Weak Conformance

In some situations Scala uses a more genral conformance relation. A 
type $S$ _weakly conforms_ 
to a type $T$, written $S \conforms_w
T$, if $S \conforms T$ or both $S$ and $T$ are primitive number types
and $S$ precedes $T$ in the following ordering.
\begin{lstlisting}
Byte $\conforms_w$ Short 
Short $\conforms_w$ Int
Char $\conforms_w$ Int
Int $\conforms_w$ Long
Long $\conforms_w$ Float
Float $\conforms_w$ Double
\end{lstlisting}

A _weak least upper bound_ is a least upper bound with respect to
weak conformance.


Volatile Types
--------------



Type volatility approximates the possibility that a type parameter or abstract type instance
of a type does not have any non-null values.  As explained in
(\sref{sec:paths}), a value member of a volatile type cannot appear in
a path.
  
A type is _volatile_ if it falls into one of four categories:

A compound type ~\lstinline@$T_1$ with $\ldots$ with $T_n$ {$R\,$}@~
is volatile if one of the following two conditions hold.

#. One of $T_2 \commadots T_n$ is a type parameter or abstract type, or
#. $T_1$ is an abstract type and and either the refinement $R$ 
   or a type $T_j$ for $j > 1$ contributes an abstract member
   to the compound type, or
#. one of $T_1 \commadots T_n$ is a singleton type.


Here, a type $S$ _contributes an abstract member_ to a type $T$ if
$S$ contains an abstract member that is also a member of $T$.
A refinement $R$ contributes an abstract member to a type $T$ if $R$
contains an abstract declaration which is also a member of $T$.

A type designator is volatile if it is an alias of a volatile type, or
if it designates a type parameter or abstract type that has a volatile type as its
upper bound.

A singleton type \lstinline@$p$.type@ is volatile, if the underlying
type of path $p$ is volatile.

An existential type ~\lstinline@$T$ forSome {$\,Q\,$}@~ is volatile if
$T$ is volatile.


Type Erasure
------------

A type is called _generic_ if it contains type arguments or type variables.
_Type erasure_ is a mapping from (possibly generic) types to
non-generic types. We write $|T|$ for the erasure of type $T$.
The erasure mapping is defined as follows.

- The erasure of an alias type is the erasure of its right-hand side. %@M
- The erasure of an abstract type is the erasure of its upper bound.
- The erasure of the parameterized type \lstinline@scala.Array$[T_1]$@ is
 \lstinline@scala.Array$[|T_1|]$@.
- The erasure of every other parameterized type $T[T_1 \commadots T_n]$ is $|T|$.
- The erasure of a singleton type \lstinline@$p$.type@ is the 
  erasure of the type of $p$.
- The erasure of a type projection \lstinline@$T$#$x$@ is \lstinline@|$T$|#$x$@.
- The erasure of a compound type 
  ~\lstinline@$T_1$ with $\ldots$ with $T_n$ {$R\,$}@ is the erasure of the intersection dominator of
  $T_1 \commadots T_n$.
- The erasure of an existential type ~\lstinline@$T$ forSome {$\,Q\,$}@ 
      is $|T|$.

The _intersection dominator_ of a list of types $T_1 \commadots
T_n$ is computed as follows.
Let $T_{i_1} \commadots T_{i_m}$ be the subsequence of types $T_i$  
which are not supertypes of some other type $T_j$. 
If this subsequence contains a type designator $T_c$ that refers to a class which is not a trait, 
the intersection dominator is $T_c$. Otherwise, the intersection
dominator is the first element of the subsequence, $T_{i_1}$.

