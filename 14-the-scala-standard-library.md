The Scala Standard Library
==========================

The Scala standard library consists of the package \code{scala} with a
number of classes and modules. Some of these classes are described in
the following.

\begin{figure*}
\centering
\includegraphics[scale=0.40]{classhierarchy}
\vspace*{-1.5mm}
\caption{Class hierarchy of Scala.}
\label{fig:class-hierarchy}
\end{figure*}

\section{Root Classes}
\label{sec:cls-root}
\label{sec:cls-any}
\label{sec:cls-object}

Figure~\ref{fig:class-hierarchy} illustrates Scala's class
hierarchy.
The root of this hierarchy is formed by class \code{Any}.
Every class in a Scala execution environment inherits directly or
indirectly from this class.  Class \code{Any} has two direct
subclasses: \code{AnyRef} and \code{AnyVal}.

The subclass \code{AnyRef} represents all values which are represented
as objects in the underlying host system. Every user-defined Scala
class inherits directly or indirectly from this class. Furthermore,
every user-defined Scala class also inherits the trait
\code{scala.ScalaObject}.  Classes written in other languages still
inherit from \code{scala.AnyRef}, but not from
\code{scala.ScalaObject}.

The class \code{AnyVal} has a fixed number of subclasses, which describe
values which are not implemented as objects in the underlying host
system.

Classes \code{AnyRef} and \code{AnyVal} are required to provide only
the members declared in class \code{Any}, but implementations may add
host-specific methods to these classes (for instance, an
implementation may identify class \code{AnyRef} with its own root
class for objects).

The signatures of these root classes are described by the following
definitions.

\begin{lstlisting}
package scala 
/** The universal root class */
abstract class Any {

  /** Defined equality; abstract here */
  def equals(that: Any): Boolean 

  /** Semantic equality between values */
  final def == (that: Any): Boolean  =  
    if (null eq this) null eq that else this equals that

  /** Semantic inequality between values */
  final def != (that: Any): Boolean  =  !(this == that)

  /** Hash code; abstract here */
  def hashCode: Int = $\ldots$

  /** Textual representation; abstract here */
  def toString: String = $\ldots$

  /** Type test; needs to be inlined to work as given */
  def isInstanceOf[a]: Boolean

  /** Type cast; needs to be inlined to work as given */ */
  def asInstanceOf[A]: A = this match {
    case x: A => x
    case _ => if (this eq null) this
              else throw new ClassCastException()
  }
}

/** The root class of all value types */
final class AnyVal extends Any 

/** The root class of all reference types */
class AnyRef extends Any {
  def equals(that: Any): Boolean      = this eq that 
  final def eq(that: AnyRef): Boolean = $\ldots$ // reference equality
  final def ne(that: AnyRef): Boolean = !(this eq that)

  def hashCode: Int = $\ldots$     // hashCode computed from allocation address
  def toString: String  = $\ldots$ // toString computed from hashCode and class name

  def synchronized[T](body: => T): T // execute `body` in while locking `this`.
}                           

/** A mixin class for every user-defined Scala class */
trait ScalaObject extends AnyRef 
\end{lstlisting}

The type test \lstinline@$x$.isInstanceOf[$T$]@ is equivalent to a typed
pattern match
\begin{lstlisting} 
$x$ match {
  case _: $T'$ => true
  case _ => false
}
\end{lstlisting} 
where the type $T'$ is the same as $T$ except if $T$ is
of the form $D$ or $D[\tps]$ where $D$ is a type member of some outer
class $C$. In this case $T'$ is \lstinline@$C$#$D$@ (or
\lstinline@$C$#$D[tps]$@, respectively), whereas $T$ itself would
expand to \lstinline@$C$.this.$D[tps]$@. In other words, an
\lstinline@isInstanceOf@ test does not check for the   


The test ~\lstinline@$x$.asInstanceOf[$T$]@ is treated specially if $T$ is a
numeric value type (\sref{sec:cls-value}). In this case the cast will
be translated to an application of a conversion method ~\lstinline@x.to$T$@ 
(\sref{cls:numeric-value}). For non-numeric values $x$ the operation will raise a
\code{ClassCastException}.

\section{Value Classes}
\label{sec:cls-value}

Value classes are classes whose instances are not represented as
objects by the underlying host system.  All value classes inherit from
class \code{AnyVal}. Scala implementations need to provide the
value classes \code{Unit}, \code{Boolean}, \code{Double}, \code{Float},
\code{Long}, \code{Int}, \code{Char}, \code{Short}, and \code{Byte}
(but are free to provide others as well).
The signatures of these classes are defined in the following.

\subsection{Numeric Value Types} \label{cls:numeric-value}

Classes \code{Double}, \code{Float},
\code{Long}, \code{Int}, \code{Char}, \code{Short}, and \code{Byte}
are together called {\em numeric value types}. Classes \code{Byte},
\code{Short}, or \code{Char} are called {\em subrange types}.
Subrange types, as well as \code{Int} and \code{Long} are called {\em
integer types}, whereas \code{Float} and \code{Double} are called {\em
floating point types}.

Numeric value types are ranked in the following partial order:

\begin{lstlisting}
Byte - Short 
             \
               Int - Long - Float - Double
             / 
        Char 
\end{lstlisting}
\code{Byte} and \code{Short} are the lowest-ranked types in this order, 
whereas \code{Double} is the highest-ranked.  Ranking does {\em not}
imply a conformance (\sref{sec:conformance}) relationship; for
instance \code{Int} is not a subtype of \code{Long}.  However, object
\code{Predef} (\sref{cls:predef}) defines views (\sref{sec:views}) 
from every numeric value type to all higher-ranked numeric value types. Therefore,
lower-ranked types are implicitly converted to higher-ranked types
when required by the context (\sref{sec:impl-conv}).

Given two numeric value types $S$ and $T$, the {\em operation type} of
$S$ and $T$ is defined as follows: If both $S$ and $T$ are subrange
types then the operation type of $S$ and $T$ is \code{Int}.  Otherwise
the operation type of $S$ and $T$ is the larger of the two types wrt
ranking. Given two numeric values $v$ and $w$ the operation type of
$v$ and $w$ is the operation type of their run-time types.

Any numeric value type $T$ supports the following methods.
\begin{itemize}
\item 
Comparison methods for equals (\code{==}), not-equals (\code{!=}),
less-than (\code{<}), greater-than (\code{>}), less-than-or-equals
(\code{<=}), greater-than-or-equals (\code{>=}), which each exist in 7
overloaded alternatives. Each alternative takes a parameter of some
numeric value type. Its result type is type \code{Boolean}. The
operation is evaluated by converting the receiver and its argument to
their operation type and performing the given comparison operation of
that type.
\item
Arithmetic methods addition (\code{+}), subtraction (\code{-}),
multiplication (\code{*}), division (\code{/}), and remainder
(\lstinline@%@), which each exist in 7 overloaded alternatives. Each
alternative takes a parameter of some numeric value type $U$.  Its
result type is the operation type of $T$ and $U$. The operation is
evaluated by converting the receiver and its argument to their
operation type and performing the given arithmetic operation of that
type.
\item
Parameterless arithmethic methods identity (\code{+}) and negation
(\code{-}), with result type $T$.  The first of these returns the
receiver unchanged, whereas the second returns its negation.
\item
Conversion methods \code{toByte}, \code{toShort}, \code{toChar},
\code{toInt}, \code{toLong}, \code{toFloat}, \code{toDouble} which
convert the receiver object to the target type, using the rules of
Java's numeric type cast operation. The conversion might truncate the
numeric value (as when going from \code{Long} to \code{Int} or from
\code{Int} to \code{Byte}) or it might lose precision (as when going
from \code{Double} to \code{Float} or when converting between
\code{Long} and \code{Float}). 
\end{itemize}

Integer numeric value types support in addition the following operations:
\begin{itemize}
\item 
Bit manipulation methods bitwise-and (\code{&}), bitwise-or
{\code{|}}, and bitwise-exclusive-or (\code{^}), which each exist in 5
overloaded alternatives. Each alternative takes a parameter of some
integer numeric value type. Its result type is the operation type of
$T$ and $U$. The operation is evaluated by converting the receiver and
its argument to their operation type and performing the given bitwise
operation of that type.
\item
A parameterless bit-negation method (\lstinline@~@). Its result type is
the reciver type $T$ or \code{Int}, whichever is larger.
The operation is evaluated by converting the receiver to the result
type and negating every bit in its value.
\item
Bit-shift methods left-shift (\code{<<}), arithmetic right-shift
(\code{>>}), and unsigned right-shift (\code{>>>}). Each of these
methods has two overloaded alternatives, which take a parameter $n$
of type \code{Int}, respectively \code{Long}. The result type of the
operation is the receiver type $T$, or \code{Int}, whichever is larger.
The operation is evaluated by converting the receiver to the result
type and performing the specified shift by $n$ bits.
\end{itemize}

Numeric value types also implement operations \code{equals},
\code{hashCode}, and \code{toString} from class \code{Any}.

The \code{equals} method tests whether the argument is a numeric value
type. If this is true, it will perform the \code{==} operation which
is appropriate for that type. That is, the \code{equals} method of a
numeric value type can be thought of being defined as follows:
\begin{lstlisting}
def equals(other: Any): Boolean = other match {
  case that: Byte   => this == that
  case that: Short  => this == that
  case that: Char   => this == that
  case that: Int    => this == that
  case that: Long   => this == that
  case that: Float  => this == that
  case that: Double => this == that
  case _ => false
}
\end{lstlisting}
The \code{hashCode} method returns an integer hashcode that maps equal
numeric values to equal results. It is guaranteed to be the identity for 
for type \code{Int} and for all subrange types.

The \code{toString} method displays its receiver as an integer or
floating point number.

\example As an example, here is the signature of the numeric value type \code{Int}:

\begin{lstlisting}
package scala 
abstract sealed class Int extends AnyVal {
  def == (that: Double): Boolean  // double equality
  def == (that: Float): Boolean   // float equality
  def == (that: Long): Boolean    // long equality
  def == (that: Int): Boolean     // int equality
  def == (that: Short): Boolean   // int equality
  def == (that: Byte): Boolean    // int equality
  def == (that: Char): Boolean    // int equality
  /* analogous for !=, <, >, <=, >= */

  def + (that: Double): Double    // double addition
  def + (that: Float): Double     // float addition
  def + (that: Long): Long        // long addition
  def + (that: Int): Int          // int addition
  def + (that: Short): Int        // int addition
  def + (that: Byte): Int         // int addition
  def + (that: Char): Int         // int addition
  /* analogous for -, *, /, % */
  
  def & (that: Long): Long        // long bitwise and
  def & (that: Int): Int          // int bitwise and
  def & (that: Short): Int        // int bitwise and
  def & (that: Byte): Int         // int bitwise and
  def & (that: Char): Int         // int bitwise and
  /* analogous for |, ^ */

  def << (cnt: Int): Int          // int left shift
  def << (cnt: Long): Int         // long left shift
  /* analogous for >>, >>> */

  def unary_+ : Int               // int identity
  def unary_- : Int               // int negation
  def unary_~ : Int               // int bitwise negation

  def toByte: Byte                // convert to Byte
  def toShort: Short              // convert to Short
  def toChar: Char                // convert to Char
  def toInt: Int                  // convert to Int
  def toLong: Long                // convert to Long
  def toFloat: Float              // convert to Float
  def toDouble: Double            // convert to Double
}
\end{lstlisting}

\subsection{Class \large{\code{Boolean}}}
\label{sec:cls-boolean}

Class \code{Boolean} has only two values: \code{true} and
\code{false}. It implements operations as given in the following
class definition.
\begin{lstlisting}
package scala 
abstract sealed class Boolean extends AnyVal {
  def && (p: => Boolean): Boolean = // boolean and
    if (this) p else false
  def || (p: => Boolean): Boolean = // boolean or
    if (this) true else p
  def &  (x: Boolean): Boolean =    // boolean strict and
    if (this) x else false
  def |  (x: Boolean): Boolean =    // boolean strict or
    if (this) true else x
  def == (x: Boolean): Boolean =    // boolean equality
    if (this) x else x.unary_!
  def != (x: Boolean): Boolean      // boolean inequality
    if (this) x.unary_! else x
  def unary_!: Boolean              // boolean negation
    if (this) false else true
}
\end{lstlisting}
The class also implements operations \code{equals}, \code{hashCode},
and \code{toString} from class \code{Any}.

The \code{equals} method returns \code{true} if the argument is the
same boolean value as the receiver, \code{false} otherwise.  The
\code{hashCode} method returns a fixed, implementation-specific hash-code when invoked on \code{true}, 
and a different, fixed, implementation-specific hash-code when invoked on \code{false}. The \code{toString} method
returns the receiver converted to a string, i.e.\ either \code{"true"}
or \code{"false"}.

\subsection{Class \large{\code{Unit}}}

Class \code{Unit} has only one value: \code{()}. It implements only
the three methods \code{equals}, \code{hashCode}, and \code{toString}
from class \code{Any}.

The \code{equals} method returns \code{true} if the argument is the
unit value \lstinline@()@, \code{false} otherwise.  The
\code{hashCode} method returns a fixed, implementation-specific hash-code, 
The \code{toString} method returns \code{"()"}.

\section{Standard Reference Classes}
\label{cls:reference}

This section presents some standard Scala reference classes which are
treated in a special way in Scala compiler -- either Scala provides
syntactic sugar for them, or the Scala compiler generates special code
for their operations. Other classes in the standard Scala library are
documented in the Scala library documentation by HTML pages.

\subsection{Class \large{\code{String}}}

Scala's \lstinline@String@ class is usually derived from the standard String
class of the underlying host system (and may be identified with
it). For Scala clients the class is taken to support in each case a
method
\begin{lstlisting}
def + (that: Any): String 
\end{lstlisting}
which concatenates its left operand with the textual representation of its
right operand.

\subsection{The \large{\code{Tuple}} classes}

Scala defines tuple classes \lstinline@Tuple$n$@ for $n = 2 \commadots 9$.
These are defined as follows.

\begin{lstlisting}
package scala 
case class Tuple$n$[+a_1, ..., +a_n](_1: a_1, ..., _$n$: a_$n$) {
  def toString = "(" ++ _1 ++ "," ++ $\ldots$ ++ "," ++ _$n$ ++ ")"
}
\end{lstlisting}

The implicitly imported \code{Predef} object (\sref{cls:predef}) defines
the names \code{Pair} as an alias of \code{Tuple2} and \code{Triple}
as an alias for \code{Tuple3}.

\subsection{The \large{\code{Function}} Classes}
\label{sec:cls-function}

Scala defines function classes \lstinline@Function$n$@ for $n = 1 \commadots 9$.
These are defined as follows.

\begin{lstlisting}
package scala 
trait Function$n$[-a_1, ..., -a_$n$, +b] {
  def apply(x_1: a_1, ..., x_$n$: a_$n$): b 
  def toString = "<function>" 
}
\end{lstlisting}

\comment{
There is also a module \code{Function}, defined as follows.
\begin{lstlisting}
package scala 
object Function {
  def compose[A](fs: List[A => A]): A => A = {
    x => fs match {
      case Nil => x
      case f :: fs1 => compose(fs1)(f(x))
    }
  }
}
\end{lstlisting}
}
A subclass of \lstinline@Function1@ represents partial functions,
which are undefined on some points in their domain. In addition to the
\code{apply} method of functions, partial functions also have a
\code{isDefined} method, which tells whether the function is defined
at the given argument:
\begin{lstlisting}
class PartialFunction[-A, +B] extends Function1[A, B] {
  def isDefinedAt(x: A): Boolean
}
\end{lstlisting}

The implicitly imported \code{Predef} object (\sref{cls:predef}) defines the name 
\code{Function} as an alias of \code{Function1}.

\subsection{Class \large{\code{Array}}}\label{cls:array}

The class of generic arrays is given as follows.

\begin{lstlisting}
final class Array[A](len: Int) extends Seq[A] {
  def length: Int = len
  def apply(i: Int): A = $\ldots$
  def update(i: Int, x: A): Unit = $\ldots$
  def elements: Iterator[A] = $\ldots$
  def subArray(from: Int, end: Int): Array[A] = $\ldots$
  def filter(p: A => Boolean): Array[A] = $\ldots$
  def map[B](f: A => B): Array[B] = $\ldots$
  def flatMap[B](f: A => Array[B]): Array[B] = $\ldots$
}
\end{lstlisting}
If $T$ is not a type parameter or abstract type, the type Array[$T$]
is represented as the native array type \lstinline{[]$T$} in the
underlying host system. In that case \code{length} returns
the length of the array, \code{apply} means subscripting, and
\code{update} means element update. Because of the syntactic sugar for
\code{apply} and
%\code{update} operations (\sref{sec:impl-conv}, \sref{sec:assignments}),
\code{update} operations (\sref{sec:impl-conv},
we have the following correspondences between Scala and Java/C\# code for
operations on an array \code{xs}:

\begin{lstlisting}
$\mbox{\em Scala}$            $\mbox{\em Java/C\#}$
  xs.length        xs.length
  xs(i)            xs[i]
  xs(i) = e        xs[i] = e
\end{lstlisting}

Arrays also implement the sequence trait \code{scala.Seq}
by defining an \code{elements} method which returns
all elements of the array in an \code{Iterator}.

Because of the tension between parametrized types in Scala and the ad-hoc
implementation of arrays in the host-languages, some subtle points
need to be taken into account when dealing with arrays. These are
explained in the following.

First, unlike arrays in Java or C\#, arrays in Scala are {\em not}
co-variant; That is, $S <: T$ does not imply 
~\lstinline@Array[$S$] $<:$ Array[$T$]@ in Scala.  
However, it is possible to cast an array
of $S$ to an array of $T$ if such a cast is permitted in the host
environment.

For instance \code{Array[String]} does not conform to
\code{Array[Object]}, even though \code{String} conforms to \code{Object}.
However, it is possible to cast an expression of type
~\lstinline@Array[String]@~ to ~\lstinline@Array[Object]@, and this
cast will succeed without raising a \code{ClassCastException}. Example:
\begin{lstlisting}
val xs = new Array[String](2)
// val ys: Array[Object] = xs   // **** error: incompatible types
val ys: Array[Object] = xs.asInstanceOf[Array[Object]] // OK
\end{lstlisting}

Second, for {\em polymorphic arrays}, that have a type parameter or
abstract type $T$ as their element type, a representation different
from
\lstinline@[]T@ might be used. However, it is guaranteed that 
\code{isInstanceOf} and \code{asInstanceOf} still work as if the array 
used the standard representation of monomorphic arrays:
\begin{lstlisting}
val ss = new Array[String](2)

def f[T](xs: Array[T]): Array[String] = 
  if (xs.isInstanceOf[Array[String]]) xs.asInstanceOf[Array[String])
  else throw new Error("not an instance")

f(ss)                                     // returns ss
\end{lstlisting}
The representation chosen for polymorphic arrays also guarantees that
polymorphic array creations work as expected. An example is the
following implementation of method \lstinline@mkArray@, which creates
an array of an arbitrary type $T$, given a sequence of $T$'s which
defines its elements.
\begin{lstlisting}
def mkArray[T](elems: Seq[T]): Array[T] = {
  val result = new Array[T](elems.length)
  var i = 0
  for (elem <- elems) {
    result(i) = elem
    i += 1
  }
}
\end{lstlisting}
Note that under Java's erasure model of arrays the method above would
not work as expected -- in fact it would always return an array of
\lstinline@Object@.

Third, in a Java environment there is a method \code{System.arraycopy}
which takes two objects as parameters together with start indices and
a length argument, and copies elements from one object to the other,
provided the objects are arrays of compatible element
types. \code{System.arraycopy} will not work for Scala's polymorphic
arrays because of their different representation. One should instead
use method \code{Array.copy} which is defined in the companion object
of class \lstinline@Array@. This companion object also defines various
constructor methods for arrays, as well as 
the extractor method \code{unapplySeq} (\sref{sec:extractor-patterns})
which enables pattern matching over arrays.
\begin{lstlisting}
package scala
object Array { 
  /** copies array elements from `src' to `dest'. */
  def copy(src: AnyRef, srcPos: Int, 
           dest: AnyRef, destPos: Int, length: Int): Unit = $\ldots$

  /** Concatenate all argument arrays into a single array. */
  def concat[T](xs: Array[T]*): Array[T] = $\ldots$

  /** Create a an array of successive integers. */
  def range(start: Int, end: Int): Array[Int] = $\ldots$

  /** Create an array with given elements. */
  def apply[A <: AnyRef](xs: A*): Array[A] = $\ldots$

  /** Analogous to above. */
  def apply(xs: Boolean*): Array[Boolean] = $\ldots$
  def apply(xs: Byte*)   : Array[Byte]    = $\ldots$
  def apply(xs: Short*)  : Array[Short]   = $\ldots$
  def apply(xs: Char*)   : Array[Char]    = $\ldots$
  def apply(xs: Int*)    : Array[Int]     = $\ldots$
  def apply(xs: Long*)   : Array[Long]    = $\ldots$
  def apply(xs: Float*)  : Array[Float]   = $\ldots$
  def apply(xs: Double*) : Array[Double]  = $\ldots$
  def apply(xs: Unit*)   : Array[Unit]    = $\ldots$

  /** Create an array containing several copies of an element. */
  def make[A](n: Int, elem: A): Array[A] = {

  /** Enables pattern matching over arrays */
  def unapplySeq[A](x: Array[A]): Option[Seq[A]] = Some(x)
}
\end{lstlisting}

\example The following method duplicates a given argument array and returns a pair consisting of the original and the duplicate:
\begin{lstlisting}
def duplicate[T](xs: Array[T]) = {
  val ys = new Array[T](xs.length)
  Array.copy(xs, 0, ys, 0, xs.length)
  (xs, ys)
}
\end{lstlisting}

\section{Class Node}\label{cls:Node}
\begin{lstlisting}
package scala.xml 

trait Node {

  /** the label of this node */
  def label: String               

  /** attribute axis */
  def attribute: Map[String, String] 

  /** child axis (all children of this node) */
  def child: Seq[Node]          

  /** descendant axis (all descendants of this node) */
  def descendant: Seq[Node] = child.toList.flatMap { 
    x => x::x.descendant.asInstanceOf[List[Node]] 
  } 

  /** descendant axis (all descendants of this node) */
  def descendant_or_self: Seq[Node] = this::child.toList.flatMap { 
    x => x::x.descendant.asInstanceOf[List[Node]] 
  } 

  override def equals(x: Any): Boolean = x match {
    case that:Node => 
      that.label == this.label && 
        that.attribute.sameElements(this.attribute) && 
          that.child.sameElements(this.child)
    case _ => false
  } 

 /** XPath style projection function. Returns all children of this node
  *  that are labeled with 'that'. The document order is preserved.
  */
    def \(that: Symbol): NodeSeq = {
      new NodeSeq({
        that.name match {
          case "_" => child.toList  
          case _ =>
            var res:List[Node] = Nil 
            for (x <- child.elements if x.label == that.name) {
              res = x::res 
            }
            res.reverse
        }
      }) 
    }

 /** XPath style projection function. Returns all nodes labeled with the 
  *  name 'that' from the 'descendant_or_self' axis. Document order is preserved.
  */
  def \\(that: Symbol): NodeSeq = {
    new NodeSeq(
      that.name match {
        case "_" => this.descendant_or_self 
        case _ => this.descendant_or_self.asInstanceOf[List[Node]].
        filter(x => x.label == that.name) 
      })
  }

  /** hashcode for this XML node */
  override def hashCode = 
    Utility.hashCode(label, attribute.toList.hashCode, child) 

  /** string representation of this node */
  override def toString = Utility.toXML(this) 

}
\end{lstlisting}

\newpage
\section{The \large{\code{Predef}} Object}\label{cls:predef}

The \code{Predef} object defines standard functions and type aliases
for Scala programs. It is always implicitly imported, so that all its
defined members are available without qualification. Its definition
for the JVM environment conforms to the following signature:

\begin{lstlisting}
package scala
object Predef {

  // classOf ---------------------------------------------------------

  /** Returns the runtime representation of a class type. */
  def classOf[T]: Class[T] = null  
   // this is a dummy, classOf is handled by compiler.

  // Standard type aliases ---------------------------------------------

  type String    = java.lang.String
  type Class[T]  = java.lang.Class[T]

  // Miscellaneous -----------------------------------------------------
  
  type Function[-A, +B] = Function1[A, B]

  type Map[A, +B] = collection.immutable.Map[A, B]
  type Set[A] = collection.immutable.Set[A]

  val Map = collection.immutable.Map
  val Set = collection.immutable.Set

  // Manifest types, companions, and incantations for summoning ---------

  type ClassManifest[T] = scala.reflect.ClassManifest[T]
  type Manifest[T]      = scala.reflect.Manifest[T]
  type OptManifest[T]   = scala.reflect.OptManifest[T]
  val ClassManifest     = scala.reflect.ClassManifest
  val Manifest          = scala.reflect.Manifest
  val NoManifest        = scala.reflect.NoManifest
  
  def manifest[T](implicit m: Manifest[T])           = m
  def classManifest[T](implicit m: ClassManifest[T]) = m
  def optManifest[T](implicit m: OptManifest[T])     = m

  // Minor variations on identity functions -----------------------------
  def identity[A](x: A): A         = x    // @see `conforms` for the implicit version
  def implicitly[T](implicit e: T) = e    // for summoning implicit values from the nether world
  @inline def locally[T](x: T): T  = x    // to communicate intent and avoid unmoored statements

  // Asserts, Preconditions, Postconditions -----------------------------

  def assert(assertion: Boolean) {
    if (!assertion)
      throw new java.lang.AssertionError("assertion failed")
  }

  def assert(assertion: Boolean, message: => Any) {
    if (!assertion)
      throw new java.lang.AssertionError("assertion failed: " + message)
  }

  def assume(assumption: Boolean) {
    if (!assumption)
      throw new IllegalArgumentException("assumption failed")
  }

  def assume(assumption: Boolean, message: => Any) {
    if (!assumption)
      throw new IllegalArgumentException(message.toString)
  }

  def require(requirement: Boolean) {
    if (!requirement)
      throw new IllegalArgumentException("requirement failed")
  }

  def require(requirement: Boolean, message: => Any) {
    if (!requirement)
      throw new IllegalArgumentException("requirement failed: "+ message)
  }
  \end{lstlisting}
\newpage
\begin{lstlisting}

  // tupling ---------------------------------------------------------

  type Pair[+A, +B] = Tuple2[A, B]
  object Pair {
    def apply[A, B](x: A, y: B) = Tuple2(x, y)
    def unapply[A, B](x: Tuple2[A, B]): Option[Tuple2[A, B]] = Some(x)
  }

  type Triple[+A, +B, +C] = Tuple3[A, B, C]
  object Triple {
    def apply[A, B, C](x: A, y: B, z: C) = Tuple3(x, y, z)
    def unapply[A, B, C](x: Tuple3[A, B, C]): Option[Tuple3[A, B, C]] = Some(x)
  }

  // Printing and reading -----------------------------------------------

  def print(x: Any) = Console.print(x)
  def println() = Console.println()
  def println(x: Any) = Console.println(x)
  def printf(text: String, xs: Any*) = Console.printf(text.format(xs: _*))

  def readLine(): String = Console.readLine()
  def readLine(text: String, args: Any*) = Console.readLine(text, args)
  def readBoolean() = Console.readBoolean()
  def readByte() = Console.readByte()
  def readShort() = Console.readShort()
  def readChar() = Console.readChar()
  def readInt() = Console.readInt()
  def readLong() = Console.readLong()
  def readFloat() = Console.readFloat()
  def readDouble() = Console.readDouble()
  def readf(format: String) = Console.readf(format)
  def readf1(format: String) = Console.readf1(format)
  def readf2(format: String) = Console.readf2(format)
  def readf3(format: String) = Console.readf3(format)

  // Implict conversions ------------------------------------------------

  ...
}
\end{lstlisting}

\subsection{Predefined Implicit Definitions}

The \lstinline@Predef@ object also contains a number of implicit definitions, which are available by default (because \lstinline@Predef@ is implicitly imported).
Implicit definitions come in two priorities. High-priority implicits are defined in the \lstinline@Predef@ class itself whereas low priority implicits are defined in a class inherited by \lstinline@Predef@. The rules of 
static overloading resolution (\sref{sec:overloading-resolution})
stipulate that, all other things being equal, implicit resolution 
prefers high-priority implicits over low-priority ones.

The available low-priority implicits include definitions falling into the following categories.

\begin{enumerate}
\item
For every primitive type, a wrapper that takes values of that type
to instances of a \lstinline@runtime.Rich*@ class. For instance, values of type \lstinline@Int@
can be implicitly converted to instances of class \lstinline@runtime.RichInt@.
\item
For every array type with elements of primitive type, a wrapper that
takes the arrays of that type to instances of a \lstinline@runtime.WrappedArray@ class. For instance, values of type \lstinline@Array[Float]@ can be implicitly converted to instances of class \lstinline@runtime.WrappedArray[Float]@.
There are also generic array wrappers that take elements
of type \lstinline@Array[T]@ for arbitrary \lstinline@T@ to \lstinline@WrappedArray@s.
\item
An implicit conversion from \lstinline@String@ to \lstinline@WrappedString@.
\end{enumerate}

The available high-priority implicits include definitions falling into the following categories.

\begin{itemize}
\item
An implicit wrapper that adds \lstinline@ensuring@ methods 
with the following overloaded variants to type \lstinline@Any@.
\begin{lstlisting}
    def ensuring(cond: Boolean): A = { assert(cond); x }
    def ensuring(cond: Boolean, msg: Any): A = { assert(cond, msg); x }
    def ensuring(cond: A => Boolean): A = { assert(cond(x)); x }
    def ensuring(cond: A => Boolean, msg: Any): A = { assert(cond(x), msg); x }
\end{lstlisting}
\item
An implicit wrapper that adds a \lstinline@->@ method with the following implementation
to type \lstinline@Any@.
\begin{lstlisting}
    def -> [B](y: B): (A, B) = (x, y)
\end{lstlisting}
\item
For every array type with elements of primitive type, a wrapper that
takes the arrays of that type to instances of a \lstinline@runtime.ArrayOps@
class. For instance, values of type \lstinline@Array[Float]@ can be implicitly
converted to instances of class \lstinline@runtime.ArrayOps[Float]@.  There are
also generic array wrappers that take elements of type \lstinline@Array[T]@ for
arbitrary \lstinline@T@ to \lstinline@ArrayOps@s.
\item
An implicit wrapper that adds \lstinline@+@ and \lstinline@formatted@ method with the following implementations
to type \lstinline@Any@.
\begin{lstlisting}
  def +(other: String) = String.valueOf(self) + other
  def formatted(fmtstr: String): String = fmtstr format self
\end{lstlisting}
\item
Numeric primitive conversions that implement the transitive closure of the following
mappings:

\begin{minipage}{\linewidth}
\begin{lstlisting}
  Byte  -> Short
  Short -> Int
  Char  -> Int
  Int   -> Long
  Long  -> Float
  Float -> Double
\end{lstlisting}
\end{minipage}

\item
Boxing and unboxing conversions between primitive types and their boxed versions:
\begin{lstlisting}
  Byte    <-> java.lang.Byte
  Short   <-> java.lang.Short
  Char    <-> java.lang.Character
  Int     <-> java.lang.Integer
  Long    <-> java.lang.Long
  Float   <-> java.lang.Float
  Double  <-> java.lang.Double
  Boolean <-> java.lang.Boolean
\end{lstlisting}
\item
An implicit definition that generates instances of type \lstinline@T <:< T@, for
any type \lstinline@T@. Here, \lstinline@<:<@ is a class defined as follows.
\begin{lstlisting}
  sealed abstract class <:<[-From, +To] extends (From => To)
\end{lstlisting}
Implicit parameters of \lstinline@<:<@ types are typically used to implement type constraints.
\end{itemize}


\comment{
\subsection{Base Classes}
\label{sec:base-classes}

For every template, class type and constructor invocation we define
two sets of class types: the {\em base classes} and {\em mixin base
classes}. Their definitions are as follows.

The {\em mixin base classes} of a template 
~\lstinline@$sc$ with $mt_1$ with $\ldots$ with $mt_n$ {$\stats\,$}@~ 
are 
the reduced union (\sref{sec:base-classes-member-defs}) of the base classes of all
mixins $mt_i$. The mixin base classes of a class type $C$ are the
mixin base classes of the template augmented by $C$ itself. The
mixin base classes of a constructor invocation of type $T$ are the
mixin base classes of class $T$.

The {\em base classes} of a template consist are the reduced union of
the base classes of its superclass and the template's mixin base
classes.  The base classes of class \lstinline@scala.Any@ consist of
just the class itself. The base classes of some other class type $C$
are the base classes of the template represented by $C$ augmented by
$C$ itself.  The base classes of a constructor invocation of type $T$
are the base classes of $T$.

The notions of mixin base classes and base classes are extended from
classes to arbitrary types following the definitions of
\sref{sec:base-classes-member-defs}.

\comment{
If two types in the base class sequence of a template refer to the
same class definition, then that definition must define a trait
(\sref{sec:traits}), and the type that comes later in the sequence must
conform to the type that comes first. 
(\sref{sec:base-classes-member-defs}).
}

\example 
Consider the following class definitions:
\begin{lstlisting}
class A 
class B extends A 
trait C extends A 
class D extends A 
class E extends B with C with D 
class F extends B with D with E 
\end{lstlisting} 
The mixin base classes and base classes of classes \code{A-F} are given in
the following table:
\begin{quote}\begin{tabular}{|l|l|l|} \hline
 \ & Mixin base classes & Base classes \\  \hline
A & A & A, ScalaObject, AnyRef, Any \\
B & B & B, A, ScalaObject, AnyRef, Any \\
C & C & C, A, ScalaObject, AnyRef, Any \\
D & D & D, A, ScalaObject, AnyRef, Any \\
E & C, D, E & E, B, C, D, A, ScalaObject, AnyRef, Any \\
F & C, D, E, F & F, B, D, E, C, A, ScalaObject, AnyRef, Any \\ \hline
\end{tabular}\end{quote}
Note that \code{D} is inherited twice by \code{F}, once directly, the
other time indirectly through \code{E}. This is permitted, since
\code{D} is a trait.
}

