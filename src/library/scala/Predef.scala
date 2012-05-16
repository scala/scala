/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

import scala.collection.{ mutable, immutable, generic }
import immutable.StringOps
import mutable.ArrayOps
import generic.CanBuildFrom
import annotation.{ elidable, implicitNotFound }
import annotation.elidable.ASSERTION
import language.{implicitConversions, existentials}

/** The `Predef` object provides definitions that are accessible in all Scala
 *  compilation units without explicit qualification.
 *
 *  === Commonly Used Types ===
 *  Predef provides type aliases for types which are commonly used, such as
 *  the immutable collection types [[scala.collection.immutable.Map]],
 *  [[scala.collection.immutable.Set]], and the [[scala.collection.immutable.List]]
 *  constructors ([[scala.collection.immutable.::]] and
 *  [[scala.collection.immutable.Nil]]).
 *  The types `Pair` (a [[scala.Tuple2]]) and `Triple` (a [[scala.Tuple3]]), with
 *  simple constructors, are also provided.
 *
 *  === Console I/O ===
 *  Predef provides a number of simple functions for console I/O, such as
 *  `print`, `println`, `readLine`, `readInt`, etc. These functions are all
 *  aliases of the functions provided by [[scala.Console]].
 *
 *  === Assertions ===
 *
 *  A set of `assert` functions are provided for use as a way to document
 *  and dynamically check invariants in code. `assert` statements can be elided
 *  at runtime by providing the command line argument `-Xdisable-assertions` to
 *  the `scala` command.
 *
 *  Variants of `assert` intended for use with static analysis tools are also
 *  provided: `assume`, `require` and `ensuring`. `require` and `ensuring` are
 *  intended for use as a means of design-by-contract style specification
 *  of pre- and post-conditions on functions, with the intention that these
 *  specifications could be consumed by a static analysis tool. For instance,
 *
 *  {{{
 *  def addNaturals(nats: List[Int]): Int = {
 *    require(nats forall (_ >= 0), "List contains negative numbers")
 *    nats.foldLeft(0)(_ + _)
 *  } ensuring(_ >= 0)
 *  }}}
 *
 *  The declaration of `addNaturals` states that the list of integers passed should
 *  only contain natural numbers (i.e. non-negative), and that the result returned
 *  will also be natural. `require` is distinct from `assert` in that if the
 *  condition fails, then the caller of the function is to blame rather than a
 *  logical error having been made within `addNaturals` itself. `ensures` is a
 *  form of `assert` that declares the guarantee the function is providing with
 *  regards to it's return value.
 *
 *  === Implicit Conversions ===
 *  A number of commonly applied implicit conversions are also defined here, and
 *  in the parent type [[scala.LowPriorityImplicits]]. Implicit conversions
 *  are provided for the "widening" of numeric values, for instance, converting a
 *  Short value to a Long value as required, and to add additional higher-order
 *  functions to Array values. These are described in more detail in the documentation of [[scala.Array]].
 */
object Predef extends LowPriorityImplicits {
  /**
   * Retrieve the runtime representation of a class type. `classOf[T]` is equivalent to
   * the class literal `T.class` in Java.
   *
   * @example {{{
   * val listClass = classOf[List[_]]
   * // listClass is java.lang.Class[List[_]] = class scala.collection.immutable.List
   *
   * val mapIntString = classOf[Map[Int,String]]
   * // mapIntString is java.lang.Class[Map[Int,String]] = interface scala.collection.immutable.Map
   * }}}
   */
  def classOf[T]: Class[T] = null // This is a stub method. The actual implementation is filled in by the compiler.

  type String        = java.lang.String
  type Class[T]      = java.lang.Class[T]

  // miscelleaneous -----------------------------------------------------
  scala.`package`                         // to force scala package object to be seen.
  scala.collection.immutable.List         // to force Nil, :: to be seen.

  type Function[-A, +B] = Function1[A, B]

  type Map[A, +B] = immutable.Map[A, B]
  type Set[A]     = immutable.Set[A]
  val Map         = immutable.Map
  val Set         = immutable.Set
  // @deprecated("Use scala.AnyRef instead", "2.10.0")
  // def AnyRef = scala.AnyRef

  // Manifest types, companions, and incantations for summoning
  type ClassManifest[T] = scala.reflect.ClassManifest[T]
  type OptManifest[T]   = scala.reflect.OptManifest[T]
  type Manifest[T]      = scala.reflect.Manifest[T]
  val ClassManifest     = scala.reflect.ClassManifest
  val Manifest          = scala.reflect.Manifest
  val NoManifest        = scala.reflect.NoManifest

  def manifest[T](implicit m: Manifest[T])           = m
  def classManifest[T](implicit m: ClassManifest[T]) = m
  def optManifest[T](implicit m: OptManifest[T])     = m

  // Tag types and companions, and incantations for summoning
  type ArrayTag[T]           = scala.reflect.ArrayTag[T]
  type ErasureTag[T]         = scala.reflect.ErasureTag[T]
  type ClassTag[T]           = scala.reflect.ClassTag[T]
  type TypeTag[T]            = scala.reflect.TypeTag[T]
  type ConcreteTypeTag[T]    = scala.reflect.ConcreteTypeTag[T]
  val ClassTag               = scala.reflect.ClassTag // doesn't need to be lazy, because it's not a path-dependent type
  // [Paul to Eugene] No lazy vals in Predef.  Too expensive.  Have to work harder on breaking initialization dependencies.
  lazy val TypeTag           = scala.reflect.TypeTag // needs to be lazy, because requires scala.reflect.mirror instance
  lazy val ConcreteTypeTag   = scala.reflect.ConcreteTypeTag

  // [Eugene to Martin] it's really tedious to type "implicitly[...]" all the time, so I'm reintroducing these shortcuts
  def arrayTag[T](implicit atag: ArrayTag[T])                      = atag
  def erasureTag[T](implicit etag: ErasureTag[T])                  = etag
  def classTag[T](implicit ctag: ClassTag[T])                      = ctag
  def tag[T](implicit ttag: TypeTag[T])                            = ttag
  def typeTag[T](implicit ttag: TypeTag[T])                        = ttag
  def concreteTag[T](implicit cttag: ConcreteTypeTag[T])           = cttag
  def concreteTypeTag[T](implicit cttag: ConcreteTypeTag[T])       = cttag

  // Minor variations on identity functions
  def identity[A](x: A): A         = x    // @see `conforms` for the implicit version
  @inline def implicitly[T](implicit e: T) = e    // for summoning implicit values from the nether world -- TODO: when dependent method types are on by default, give this result type `e.type`, so that inliner has better chance of knowing which method to inline in calls like `implicitly[MatchingStrategy[Option]].zero`
  @inline def locally[T](x: T): T  = x    // to communicate intent and avoid unmoored statements

  // Apparently needed for the xml library
  val $scope = scala.xml.TopScope

  // Deprecated

  @deprecated("Use sys.error(message) instead", "2.9.0")
  def error(message: String): Nothing = sys.error(message)

  @deprecated("Use sys.exit() instead", "2.9.0")
  def exit(): Nothing = sys.exit()

  @deprecated("Use sys.exit(status) instead", "2.9.0")
  def exit(status: Int): Nothing = sys.exit(status)

  @deprecated("Use formatString.format(args: _*) or arg.formatted(formatString) instead", "2.9.0")
  def format(text: String, xs: Any*) = augmentString(text).format(xs: _*)

  // errors and asserts -------------------------------------------------

  /** Tests an expression, throwing an `AssertionError` if false.
   *  Calls to this method will not be generated if `-Xelide-below`
   *  is at least `ASSERTION`.
   *
   *  @see elidable
   *  @param assertion   the expression to test
   */
  @elidable(ASSERTION)
  def assert(assertion: Boolean) {
    if (!assertion)
      throw new java.lang.AssertionError("assertion failed")
  }

  /** Tests an expression, throwing an `AssertionError` if false.
   *  Calls to this method will not be generated if `-Xelide-below`
   *  is at least `ASSERTION`.
   *
   *  @see elidable
   *  @param assertion   the expression to test
   *  @param message     a String to include in the failure message
   */
  @elidable(ASSERTION) @inline
  final def assert(assertion: Boolean, message: => Any) {
    if (!assertion)
      throw new java.lang.AssertionError("assertion failed: "+ message)
  }

  /** Tests an expression, throwing an `AssertionError` if false.
   *  This method differs from assert only in the intent expressed:
   *  assert contains a predicate which needs to be proven, while
   *  assume contains an axiom for a static checker.  Calls to this method
   *  will not be generated if `-Xelide-below` is at least `ASSERTION`.
   *
   *  @see elidable
   *  @param assumption   the expression to test
   */
  @elidable(ASSERTION)
  def assume(assumption: Boolean) {
    if (!assumption)
      throw new java.lang.AssertionError("assumption failed")
  }

  /** Tests an expression, throwing an `AssertionError` if false.
   *  This method differs from assert only in the intent expressed:
   *  assert contains a predicate which needs to be proven, while
   *  assume contains an axiom for a static checker.  Calls to this method
   *  will not be generated if `-Xelide-below` is at least `ASSERTION`.
   *
   *  @see elidable
   *  @param assumption   the expression to test
   *  @param message      a String to include in the failure message
   */
  @elidable(ASSERTION) @inline
  final def assume(assumption: Boolean, message: => Any) {
    if (!assumption)
      throw new java.lang.AssertionError("assumption failed: "+ message)
  }

  /** Tests an expression, throwing an `IllegalArgumentException` if false.
   *  This method is similar to `assert`, but blames the caller of the method
   *  for violating the condition.
   *
   *  @param requirement   the expression to test
   */
  def require(requirement: Boolean) {
    if (!requirement)
      throw new IllegalArgumentException("requirement failed")
  }

  /** Tests an expression, throwing an `IllegalArgumentException` if false.
   *  This method is similar to `assert`, but blames the caller of the method
   *  for violating the condition.
   *
   *  @param requirement   the expression to test
   *  @param message       a String to include in the failure message
   */
  @inline final def require(requirement: Boolean, message: => Any) {
    if (!requirement)
      throw new IllegalArgumentException("requirement failed: "+ message)
  }

  final class Ensuring[A](val __resultOfEnsuring: A) extends AnyVal {
    // `__resultOfEnsuring` must be a public val to allow inlining.
    // See comments in ArrowAssoc for more.
    @deprecated("Use __resultOfEnsuring instead", "2.10.0")
    def x = __resultOfEnsuring

    def ensuring(cond: Boolean): A = { assert(cond); __resultOfEnsuring }
    def ensuring(cond: Boolean, msg: => Any): A = { assert(cond, msg); __resultOfEnsuring }
    def ensuring(cond: A => Boolean): A = { assert(cond(__resultOfEnsuring)); __resultOfEnsuring }
    def ensuring(cond: A => Boolean, msg: => Any): A = { assert(cond(__resultOfEnsuring), msg); __resultOfEnsuring }
  }
  @inline implicit def any2Ensuring[A](x: A): Ensuring[A] = new Ensuring(x)

  /** `???` can be used for marking methods that remain to be implemented.
   *  @throws  A `NotImplementedError`
   */
  def ??? : Nothing = throw new NotImplementedError

  // tupling ------------------------------------------------------------

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

  final class ArrowAssoc[A](val __leftOfArrow: A) extends AnyVal {
    // `__leftOfArrow` must be a public val to allow inlining. The val
    // used to be called `x`, but now goes by `__leftOfArrow`, as that
    // reduces the chances of a user's writing `foo.__leftOfArrow` and
    // being confused why they get an ambiguous implicit conversion
    // error. (`foo.x` used to produce this error since both
    // any2Ensuring and any2ArrowAssoc pimped an `x` onto everything)
    @deprecated("Use __leftOfArrow instead", "2.10.0")
    def x = __leftOfArrow

    @inline def -> [B](y: B): Tuple2[A, B] = Tuple2(__leftOfArrow, y)
    def →[B](y: B): Tuple2[A, B] = ->(y)
  }
  @inline implicit def any2ArrowAssoc[A](x: A): ArrowAssoc[A] = new ArrowAssoc(x)

  // printing and reading -----------------------------------------------

  def print(x: Any) = Console.print(x)
  def println() = Console.println()
  def println(x: Any) = Console.println(x)
  def printf(text: String, xs: Any*) = Console.print(text.format(xs: _*))

  def readLine(): String = Console.readLine()
  def readLine(text: String, args: Any*) = Console.readLine(text, args: _*)
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

  // views --------------------------------------------------------------

  implicit def exceptionWrapper(exc: Throwable)                                 = new runtime.RichException(exc)
  implicit def tuple2ToZippedOps[T1, T2](x: (T1, T2))                           = new runtime.Tuple2Zipped.Ops(x)
  implicit def tuple3ToZippedOps[T1, T2, T3](x: (T1, T2, T3))                   = new runtime.Tuple3Zipped.Ops(x)
  implicit def seqToCharSequence(xs: collection.IndexedSeq[Char]): CharSequence = new runtime.SeqCharSequence(xs)
  implicit def arrayToCharSequence(xs: Array[Char]): CharSequence               = new runtime.ArrayCharSequence(xs, 0, xs.length)

  implicit def genericArrayOps[T](xs: Array[T]): ArrayOps[T] = (xs match {
    case x: Array[AnyRef]  => refArrayOps[AnyRef](x)
    case x: Array[Boolean] => booleanArrayOps(x)
    case x: Array[Byte]    => byteArrayOps(x)
    case x: Array[Char]    => charArrayOps(x)
    case x: Array[Double]  => doubleArrayOps(x)
    case x: Array[Float]   => floatArrayOps(x)
    case x: Array[Int]     => intArrayOps(x)
    case x: Array[Long]    => longArrayOps(x)
    case x: Array[Short]   => shortArrayOps(x)
    case x: Array[Unit]    => unitArrayOps(x)
    case null              => null
  }).asInstanceOf[ArrayOps[T]]

  implicit def booleanArrayOps(xs: Array[Boolean]): ArrayOps[Boolean] = new ArrayOps.ofBoolean(xs)
  implicit def byteArrayOps(xs: Array[Byte]): ArrayOps[Byte]          = new ArrayOps.ofByte(xs)
  implicit def charArrayOps(xs: Array[Char]): ArrayOps[Char]          = new ArrayOps.ofChar(xs)
  implicit def doubleArrayOps(xs: Array[Double]): ArrayOps[Double]    = new ArrayOps.ofDouble(xs)
  implicit def floatArrayOps(xs: Array[Float]): ArrayOps[Float]       = new ArrayOps.ofFloat(xs)
  implicit def intArrayOps(xs: Array[Int]): ArrayOps[Int]             = new ArrayOps.ofInt(xs)
  implicit def longArrayOps(xs: Array[Long]): ArrayOps[Long]          = new ArrayOps.ofLong(xs)
  implicit def refArrayOps[T <: AnyRef](xs: Array[T]): ArrayOps[T]    = new ArrayOps.ofRef[T](xs)
  implicit def shortArrayOps(xs: Array[Short]): ArrayOps[Short]       = new ArrayOps.ofShort(xs)
  implicit def unitArrayOps(xs: Array[Unit]): ArrayOps[Unit]          = new ArrayOps.ofUnit(xs)

  // Primitive Widenings --------------------------------------------------------------

  @deprecated("Use a method in an AnyVal's companion object", "2.10.0") def byte2short(x: Byte): Short = x.toShort
  @deprecated("Use a method in an AnyVal's companion object", "2.10.0") def byte2int(x: Byte): Int = x.toInt
  @deprecated("Use a method in an AnyVal's companion object", "2.10.0") def byte2long(x: Byte): Long = x.toLong
  @deprecated("Use a method in an AnyVal's companion object", "2.10.0") def byte2float(x: Byte): Float = x.toFloat
  @deprecated("Use a method in an AnyVal's companion object", "2.10.0") def byte2double(x: Byte): Double = x.toDouble

  @deprecated("Use a method in an AnyVal's companion object", "2.10.0") def short2int(x: Short): Int = x.toInt
  @deprecated("Use a method in an AnyVal's companion object", "2.10.0") def short2long(x: Short): Long = x.toLong
  @deprecated("Use a method in an AnyVal's companion object", "2.10.0") def short2float(x: Short): Float = x.toFloat
  @deprecated("Use a method in an AnyVal's companion object", "2.10.0") def short2double(x: Short): Double = x.toDouble

  @deprecated("Use a method in an AnyVal's companion object", "2.10.0") def char2int(x: Char): Int = x.toInt
  @deprecated("Use a method in an AnyVal's companion object", "2.10.0") def char2long(x: Char): Long = x.toLong
  @deprecated("Use a method in an AnyVal's companion object", "2.10.0") def char2float(x: Char): Float = x.toFloat
  @deprecated("Use a method in an AnyVal's companion object", "2.10.0") def char2double(x: Char): Double = x.toDouble

  @deprecated("Use a method in an AnyVal's companion object", "2.10.0") def int2long(x: Int): Long = x.toLong
  @deprecated("Use a method in an AnyVal's companion object", "2.10.0") def int2float(x: Int): Float = x.toFloat
  @deprecated("Use a method in an AnyVal's companion object", "2.10.0") def int2double(x: Int): Double = x.toDouble

  @deprecated("Use a method in an AnyVal's companion object", "2.10.0") def long2float(x: Long): Float = x.toFloat
  @deprecated("Use a method in an AnyVal's companion object", "2.10.0") def long2double(x: Long): Double = x.toDouble

  @deprecated("Use a method in an AnyVal's companion object", "2.10.0") def float2double(x: Float): Double = x.toDouble

  // "Autoboxing" and "Autounboxing" ---------------------------------------------------

  implicit def byte2Byte(x: Byte)           = java.lang.Byte.valueOf(x)
  implicit def short2Short(x: Short)        = java.lang.Short.valueOf(x)
  implicit def char2Character(x: Char)      = java.lang.Character.valueOf(x)
  implicit def int2Integer(x: Int)          = java.lang.Integer.valueOf(x)
  implicit def long2Long(x: Long)           = java.lang.Long.valueOf(x)
  implicit def float2Float(x: Float)        = java.lang.Float.valueOf(x)
  implicit def double2Double(x: Double)     = java.lang.Double.valueOf(x)
  implicit def boolean2Boolean(x: Boolean)  = java.lang.Boolean.valueOf(x)

  // These next eight implicits exist solely to exclude AnyRef methods from the
  // eight implicits above so that primitives are not coerced to AnyRefs.  They
  // only create such conflict for AnyRef methods, so the methods on the java.lang
  // boxed types are unambiguously reachable.
  implicit def byte2ByteConflict(x: Byte)           = new AnyRef
  implicit def short2ShortConflict(x: Short)        = new AnyRef
  implicit def char2CharacterConflict(x: Char)      = new AnyRef
  implicit def int2IntegerConflict(x: Int)          = new AnyRef
  implicit def long2LongConflict(x: Long)           = new AnyRef
  implicit def float2FloatConflict(x: Float)        = new AnyRef
  implicit def double2DoubleConflict(x: Double)     = new AnyRef
  implicit def boolean2BooleanConflict(x: Boolean)  = new AnyRef

  implicit def Byte2byte(x: java.lang.Byte): Byte             = x.byteValue
  implicit def Short2short(x: java.lang.Short): Short         = x.shortValue
  implicit def Character2char(x: java.lang.Character): Char   = x.charValue
  implicit def Integer2int(x: java.lang.Integer): Int         = x.intValue
  implicit def Long2long(x: java.lang.Long): Long             = x.longValue
  implicit def Float2float(x: java.lang.Float): Float         = x.floatValue
  implicit def Double2double(x: java.lang.Double): Double     = x.doubleValue
  implicit def Boolean2boolean(x: java.lang.Boolean): Boolean = x.booleanValue

  // Strings and CharSequences --------------------------------------------------------------

  @inline implicit def any2stringfmt(x: Any) = new runtime.StringFormat(x)
  @inline implicit def augmentString(x: String): StringOps = new StringOps(x)
  implicit def any2stringadd(x: Any) = new runtime.StringAdd(x)
  implicit def unaugmentString(x: StringOps): String = x.repr

  @deprecated("Use StringCanBuildFrom", "2.10.0")
  def stringCanBuildFrom: CanBuildFrom[String, Char, String] = StringCanBuildFrom

  implicit val StringCanBuildFrom: CanBuildFrom[String, Char, String] = new CanBuildFrom[String, Char, String] {
    def apply(from: String) = apply()
    def apply()             = mutable.StringBuilder.newBuilder
  }

  // Type Constraints --------------------------------------------------------------

  /**
   * An instance of `A <:< B` witnesses that `A` is a subtype of `B`.
   * Requiring an implicit argument of the type `A <:< B` encodes
   * the generalized constraint `A <: B`.
   *
   * @note we need a new type constructor `<:<` and evidence `conforms`,
   * as reusing `Function1` and `identity` leads to ambiguities in
   * case of type errors (`any2stringadd` is inferred)
   *
   * To constrain any abstract type T that's in scope in a method's
   * argument list (not just the method's own type parameters) simply
   * add an implicit argument of type `T <:< U`, where `U` is the required
   * upper bound; or for lower-bounds, use: `L <:< T`, where `L` is the
   * required lower bound.
   *
   * In part contributed by Jason Zaugg.
   */
  @implicitNotFound(msg = "Cannot prove that ${From} <:< ${To}.")
  sealed abstract class <:<[-From, +To] extends (From => To) with Serializable
  private[this] final val singleton_<:< = new <:<[Any,Any] { def apply(x: Any): Any = x }
  // not in the <:< companion object because it is also
  // intended to subsume identity (which is no longer implicit)
  implicit def conforms[A]: A <:< A = singleton_<:<.asInstanceOf[A <:< A]

  /** An instance of `A =:= B` witnesses that the types `A` and `B` are equal.
   *
   * @see `<:<` for expressing subtyping constraints
   */
  @implicitNotFound(msg = "Cannot prove that ${From} =:= ${To}.")
  sealed abstract class =:=[From, To] extends (From => To) with Serializable
  private[this] final val singleton_=:= = new =:=[Any,Any] { def apply(x: Any): Any = x }
  object =:= {
     implicit def tpEquals[A]: A =:= A = singleton_=:=.asInstanceOf[A =:= A]
  }

  /** A type for which there is always an implicit value.
   *  @see [[scala.Array$]], method `fallbackCanBuildFrom`
   */
  class DummyImplicit

  object DummyImplicit {

    /** An implicit value yielding a `DummyImplicit`.
     *   @see [[scala.Array$]], method `fallbackCanBuildFrom`
     */
    implicit def dummyImplicit: DummyImplicit = new DummyImplicit
  }
}
