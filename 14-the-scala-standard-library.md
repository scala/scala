The Scala Standard Library
==========================

The Scala standard library consists of the package `scala` with a
number of classes and modules. Some of these classes are described in
the following.

![Class hierarchy of Scala](resources/classhierarchy.pdf)


Root Classes
------------

Figure~\ref{fig:class-hierarchy} illustrates Scala's class
hierarchy.
The root of this hierarchy is formed by class `Any`.
Every class in a Scala execution environment inherits directly or
indirectly from this class.  Class `Any` has two direct
subclasses: `AnyRef` and AnyVal`.

The subclass `AnyRef` represents all values which are represented
as objects in the underlying host system. Every user-defined Scala
class inherits directly or indirectly from this class. Furthermore,
every user-defined Scala class also inherits the trait
`scala.ScalaObject`.  Classes written in other languages still
inherit from `scala.AnyRef`, but not from
`scala.ScalaObject`.

The class `AnyVal` has a fixed number of subclasses, which describe
values which are not implemented as objects in the underlying host
system.

Classes `AnyRef` and `AnyVal` are required to provide only
the members declared in class `Any`, but implementations may add
host-specific methods to these classes (for instance, an
implementation may identify class `AnyRef` with its own root
class for objects).

The signatures of these root classes are described by the following
definitions.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
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
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The type test `$x$.isInstanceOf[$T$]` is equivalent to a typed
pattern match

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
$x$ match {
  case _: $T'$ => true
  case _ => false
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

where the type $T'$ is the same as $T$ except if $T$ is
of the form $D$ or $D[\mathit{tps}]$ where $D$ is a type member of some outer
class $C$. In this case $T'$ is `$C$#$D$` (or
`$C$#$D[tps]$`, respectively), whereas $T$ itself would
expand to `$C$.this.$D[tps]$`. In other words, an
`isInstanceOf` test does not check for the   


The test `$x$.asInstanceOf[$T$]` is treated specially if $T$ is a
[numeric value type](#value-classes). In this case the cast will
be translated to an application of a [conversion method](#numeric-value-types) 
`x.to$T$`. For non-numeric values $x$ the operation will raise a
`ClassCastException`.

Value Classes
-------------

Value classes are classes whose instances are not represented as
objects by the underlying host system.  All value classes inherit from
class `AnyVal`. Scala implementations need to provide the
value classes `Unit`, `Boolean`, `Double`, `Float`,
`Long`, `Int`, `Char`, `Short`, and `Byte`
(but are free to provide others as well).
The signatures of these classes are defined in the following.

### Numeric Value Types

Classes `Double`, `Float`,
`Long`, `Int`, `Char`, `Short`, and `Byte`
are together called _numeric value types_. Classes `Byte`,
`Short`, or `Char` are called _subrange types_.
Subrange types, as well as `Int` and `Long` are called _integer types_, whereas `Float` and `Double` are called _floating point types_.

Numeric value types are ranked in the following partial order:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Byte - Short 
             \
               Int - Long - Float - Double
             / 
        Char 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`Byte` and `Short` are the lowest-ranked types in this order, 
whereas `Double` is the highest-ranked.  Ranking does _not_
imply a [conformance relationship](#conformance); for
instance `Int` is not a subtype of `Long`.  However, object
[`Predef`](#the-predef-object) defines [views](#views) 
from every numeric value type to all higher-ranked numeric value types. 
Therefore, lower-ranked types are implicitly converted to higher-ranked types
when required by the [context](#implicit-conversions).

Given two numeric value types $S$ and $T$, the _operation type_ of
$S$ and $T$ is defined as follows: If both $S$ and $T$ are subrange
types then the operation type of $S$ and $T$ is `Int`.  Otherwise
the operation type of $S$ and $T$ is the larger of the two types wrt
ranking. Given two numeric values $v$ and $w$ the operation type of
$v$ and $w$ is the operation type of their run-time types.

Any numeric value type $T$ supports the following methods.

  * Comparison methods for equals (`==`), not-equals (`!=`),
    less-than (`<`), greater-than (`>`), less-than-or-equals
    (`<=`), greater-than-or-equals (`>=`), which each exist in 7
    overloaded alternatives. Each alternative takes a parameter of some
    numeric value type. Its result type is type `Boolean`. The
    operation is evaluated by converting the receiver and its argument to
    their operation type and performing the given comparison operation of
    that type.
  * Arithmetic methods addition (`+`), subtraction (`-`),
    multiplication (`*`), division (`/`), and remainder
    (`%`), which each exist in 7 overloaded alternatives. Each
    alternative takes a parameter of some numeric value type $U$.  Its
    result type is the operation type of $T$ and $U$. The operation is
    evaluated by converting the receiver and its argument to their
    operation type and performing the given arithmetic operation of that
    type.
  * Parameterless arithmethic methods identity (`+`) and negation
    (`-`), with result type $T$.  The first of these returns the
    receiver unchanged, whereas the second returns its negation.
  * Conversion methods `toByte`, `toShort`, `toChar`,
    `toInt`, `toLong`, `toFloat`, `toDouble` which
    convert the receiver object to the target type, using the rules of
    Java's numeric type cast operation. The conversion might truncate the
    numeric value (as when going from `Long` to `Int` or from
    `Int` to `Byte`) or it might lose precision (as when going
    from `Double` to `Float` or when converting between
    `Long` and `Float`). 

Integer numeric value types support in addition the following operations:

  * Bit manipulation methods bitwise-and (`&`), bitwise-or
    {`|`}, and bitwise-exclusive-or (`^`), which each exist in 5
    overloaded alternatives. Each alternative takes a parameter of some
    integer numeric value type. Its result type is the operation type of
    $T$ and $U$. The operation is evaluated by converting the receiver and
    its argument to their operation type and performing the given bitwise
    operation of that type.

  * A parameterless bit-negation method (`~`). Its result type is
    the reciver type $T$ or `Int`, whichever is larger.
    The operation is evaluated by converting the receiver to the result
    type and negating every bit in its value.
  * Bit-shift methods left-shift (`<<`), arithmetic right-shift
    (`>>`), and unsigned right-shift (`>>>`). Each of these
    methods has two overloaded alternatives, which take a parameter $n$
    of type `Int`, respectively `Long`. The result type of the
    operation is the receiver type $T$, or `Int`, whichever is larger.
    The operation is evaluated by converting the receiver to the result
    type and performing the specified shift by $n$ bits.

Numeric value types also implement operations `equals`,
`hashCode`, and `toString` from class `Any`.

The `equals` method tests whether the argument is a numeric value
type. If this is true, it will perform the `==` operation which
is appropriate for that type. That is, the `equals` method of a
numeric value type can be thought of being defined as follows:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
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
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The `hashCode` method returns an integer hashcode that maps equal
numeric values to equal results. It is guaranteed to be the identity for 
for type `Int` and for all subrange types.

The `toString` method displays its receiver as an integer or
floating point number.

(@) As an example, here is the signature of the numeric value type `Int`:

    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
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
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


### Class `Boolean`

Class `Boolean` has only two values: `true` and
`false`. It implements operations as given in the following
class definition.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
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
  def != (x: Boolean): Boolean =    // boolean inequality
    if (this) x.unary_! else x
  def unary_!: Boolean =            // boolean negation
    if (this) false else true
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The class also implements operations `equals`, `hashCode`,
and `toString` from class `Any`.

The `equals` method returns `true` if the argument is the
same boolean value as the receiver, `false` otherwise.  The
`hashCode` method returns a fixed, implementation-specific hash-code when invoked on `true`, 
and a different, fixed, implementation-specific hash-code when invoked on `false`. The `toString` method
returns the receiver converted to a string, i.e.\ either `"true"`
or `"false"`.

### Class `Unit`

Class `Unit` has only one value: `()`. It implements only
the three methods `equals`, `hashCode`, and `toString`
from class `Any`.

The `equals` method returns `true` if the argument is the
unit value `()`, `false` otherwise.  The
`hashCode` method returns a fixed, implementation-specific hash-code, 
The `toString` method returns `"()"`.

Standard Reference Classes
--------------------------

This section presents some standard Scala reference classes which are
treated in a special way in Scala compiler -- either Scala provides
syntactic sugar for them, or the Scala compiler generates special code
for their operations. Other classes in the standard Scala library are
documented in the Scala library documentation by HTML pages.

### Class `String`

Scala's `String` class is usually derived from the standard String
class of the underlying host system (and may be identified with
it). For Scala clients the class is taken to support in each case a
method

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
def + (that: Any): String 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

which concatenates its left operand with the textual representation of its
right operand.

### The `Tuple` classes

Scala defines tuple classes `Tuple$n$` for $n = 2 , \ldots , 9$.
These are defined as follows.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
package scala 
case class Tuple$n$[+A_1, ..., +A_n](_1: A_1, ..., _$n$: A_$n$) {
  def toString = "(" ++ _1 ++ "," ++ $\ldots$ ++ "," ++ _$n$ ++ ")"
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The implicitly imported [`Predef`](#the-predef-object) object defines
the names `Pair` as an alias of `Tuple2` and `Triple`
as an alias for `Tuple3`.

### The `Function` Classes

Scala defines function classes `Function$n$` for $n = 1 , \ldots , 9$.
These are defined as follows.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
package scala 
trait Function$n$[-A_1, ..., -A_$n$, +B] {
  def apply(x_1: A_1, ..., x_$n$: A_$n$): B 
  def toString = "<function>" 
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A subclass of `Function1` represents partial functions,
which are undefined on some points in their domain. In addition to the
`apply` method of functions, partial functions also have a
`isDefined` method, which tells whether the function is defined
at the given argument:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
class PartialFunction[-A, +B] extends Function1[A, B] {
  def isDefinedAt(x: A): Boolean
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The implicitly imported [`Predef`](#the-predef-object) object defines the name 
`Function` as an alias of `Function1`.

### Class `Array`

The class of generic arrays is given as follows.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
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
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If $T$ is not a type parameter or abstract type, the type Array[$T$]
is represented as the native array type `[]$T$` in the
underlying host system. In that case `length` returns
the length of the array, `apply` means subscripting, and
`update` means element update. Because of the syntactic sugar for
`apply` and
`update` [operations](#implicit-conversions),
we have the following correspondences between Scala and Java/C\# code for
operations on an array `xs`:

------------------   ----------------------
_Scala_              _Java/C#_
`xs.length`          `xs.length`
`xs(i)`              `xs[i]`
`xs(i) = e`          `xs[i] = e`
------------------   ----------------------

Arrays also implement the sequence trait `scala.Seq`
by defining an `elements` method which returns
all elements of the array in an `Iterator`.

Because of the tension between parametrized types in Scala and the ad-hoc
implementation of arrays in the host-languages, some subtle points
need to be taken into account when dealing with arrays. These are
explained in the following.

First, unlike arrays in Java or C\#, arrays in Scala are _not_
co-variant; That is, $S <: T$ does not imply 
`Array[$S$] $<:$ Array[$T$]` in Scala.  
However, it is possible to cast an array
of $S$ to an array of $T$ if such a cast is permitted in the host
environment.

For instance `Array[String]` does not conform to
`Array[Object]`, even though `String` conforms to `Object`.
However, it is possible to cast an expression of type
`Array[String]` to `Array[Object]`, and this
cast will succeed without raising a `ClassCastException`. Example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
val xs = new Array[String](2)
// val ys: Array[Object] = xs   // **** error: incompatible types
val ys: Array[Object] = xs.asInstanceOf[Array[Object]] // OK
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Second, for _polymorphic arrays_, that have a type parameter or
abstract type $T$ as their element type, a representation different
from
`[]T` might be used. However, it is guaranteed that 
`isInstanceOf` and `asInstanceOf` still work as if the array 
used the standard representation of monomorphic arrays:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
val ss = new Array[String](2)

def f[T](xs: Array[T]): Array[String] = 
  if (xs.isInstanceOf[Array[String]]) xs.asInstanceOf[Array[String])
  else throw new Error("not an instance")

f(ss)                                     // returns ss
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The representation chosen for polymorphic arrays also guarantees that
polymorphic array creations work as expected. An example is the
following implementation of method `mkArray`, which creates
an array of an arbitrary type $T$, given a sequence of $T$'s which
defines its elements.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
def mkArray[T](elems: Seq[T]): Array[T] = {
  val result = new Array[T](elems.length)
  var i = 0
  for (elem <- elems) {
    result(i) = elem
    i += 1
  }
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that under Java's erasure model of arrays the method above would
not work as expected -- in fact it would always return an array of
`Object`.

Third, in a Java environment there is a method `System.arraycopy`
which takes two objects as parameters together with start indices and
a length argument, and copies elements from one object to the other,
provided the objects are arrays of compatible element
types. `System.arraycopy` will not work for Scala's polymorphic
arrays because of their different representation. One should instead
use method `Array.copy` which is defined in the companion object
of class `Array`. This companion object also defines various
constructor methods for arrays, as well as 
the [extractor method](#extractor-patterns) `unapplySeq`
which enables pattern matching over arrays.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
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
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(@) The following method duplicates a given argument array and returns a pair
    consisting of the original and the duplicate:

    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    def duplicate[T](xs: Array[T]) = {
      val ys = new Array[T](xs.length)
      Array.copy(xs, 0, ys, 0, xs.length)
      (xs, ys)
    }
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


Class Node
----------

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
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
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


The `Predef` Object
-------------------

The `Predef` object defines standard functions and type aliases
for Scala programs. It is always implicitly imported, so that all its
defined members are available without qualification. Its definition
for the JVM environment conforms to the following signature:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
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
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}

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
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


### Predefined Implicit Definitions

The `Predef` object also contains a number of implicit definitions, which are available by default (because `Predef` is implicitly imported).
Implicit definitions come in two priorities. High-priority implicits are defined in the `Predef` class itself whereas low priority implicits are defined in a class inherited by `Predef`. The rules of 
static [overloading resolution](#overloading-resolution)
stipulate that, all other things being equal, implicit resolution 
prefers high-priority implicits over low-priority ones.

The available low-priority implicits include definitions falling into the following categories.

#.  For every primitive type, a wrapper that takes values of that type
    to instances of a `runtime.Rich*` class. For instance, values of type `Int`
    can be implicitly converted to instances of class `runtime.RichInt`.

#.  For every array type with elements of primitive type, a wrapper that
    takes the arrays of that type to instances of a `runtime.WrappedArray` class. For instance, values of type `Array[Float]` can be implicitly converted to instances of class `runtime.WrappedArray[Float]`.
    There are also generic array wrappers that take elements
    of type `Array[T]` for arbitrary `T` to `WrappedArray`s.

#.  An implicit conversion from `String` to `WrappedString`.


The available high-priority implicits include definitions falling into the following categories.

  * An implicit wrapper that adds `ensuring` methods 
    with the following overloaded variants to type `Any`.

    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    def ensuring(cond: Boolean): A = { assert(cond); x }
    def ensuring(cond: Boolean, msg: Any): A = { assert(cond, msg); x }
    def ensuring(cond: A => Boolean): A = { assert(cond(x)); x }
    def ensuring(cond: A => Boolean, msg: Any): A = { assert(cond(x), msg); x }
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  * An implicit wrapper that adds a `->` method with the following implementation
    to type `Any`.

    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    def -> [B](y: B): (A, B) = (x, y)
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  * For every array type with elements of primitive type, a wrapper that
    takes the arrays of that type to instances of a `runtime.ArrayOps`
    class. For instance, values of type `Array[Float]` can be implicitly
    converted to instances of class `runtime.ArrayOps[Float]`.  There are
    also generic array wrappers that take elements of type `Array[T]` for
    arbitrary `T` to `ArrayOps`s.

  * An implicit wrapper that adds `+` and `formatted` method with the following
    implementations to type `Any`.

    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    def +(other: String) = String.valueOf(self) + other
    def formatted(fmtstr: String): String = fmtstr format self
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  * Numeric primitive conversions that implement the transitive closure of the 
    following mappings:

    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    Byte  -> Short
    Short -> Int
    Char  -> Int
    Int   -> Long
    Long  -> Float
    Float -> Double
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  * Boxing and unboxing conversions between primitive types and their boxed 
    versions:

    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    Byte    <-> java.lang.Byte
    Short   <-> java.lang.Short
    Char    <-> java.lang.Character
    Int     <-> java.lang.Integer
    Long    <-> java.lang.Long
    Float   <-> java.lang.Float
    Double  <-> java.lang.Double
    Boolean <-> java.lang.Boolean
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  * An implicit definition that generates instances of type `T <:< T`, for
    any type `T`. Here, `<:<` is a class defined as follows.

    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    sealed abstract class <:<[-From, +To] extends (From => To)
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    Implicit parameters of `<:<` types are typically used to implement type constraints.

