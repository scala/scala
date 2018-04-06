/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

import scala.language.implicitConversions

import scala.collection.{ StringOps, StringView }
import scala.collection.{ mutable, immutable, ArrayOps }
import scala.collection.immutable.{ ImmutableArray, WrappedString }
import scala.annotation.{ elidable, implicitNotFound }
import scala.annotation.elidable.ASSERTION
import scala.io.StdIn

/** The `Predef` object provides definitions that are accessible in all Scala
 *  compilation units without explicit qualification.
 *
 *  === Commonly Used Types ===
 *  Predef provides type aliases for types which are commonly used, such as
 *  the immutable collection types [[scala.collection.immutable.Map]],
 *  [[scala.collection.immutable.Set]], and the [[scala.collection.immutable.List]]
 *  constructors ([[scala.collection.immutable.::]] and
 *  [[scala.collection.immutable.Nil]]).
 *
 *  === Console Output ===
 *  For basic console output, `Predef` provides convenience methods [[print(x:Any* print]] and [[println(x:Any* println]],
 *  which are aliases of the methods in the object [[scala.Console]].
 *
 *  === Assertions ===
 *  A set of `assert` functions are provided for use as a way to document
 *  and dynamically check invariants in code. Invocations of `assert` can be elided
 *  at compile time by providing the command line option `-Xdisable-assertions`,
 *  which raises `-Xelide-below` above `elidable.ASSERTION`, to the `scalac` command.
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
 *  logical error having been made within `addNaturals` itself. `ensuring` is a
 *  form of `assert` that declares the guarantee the function is providing with
 *  regards to its return value.
 *
 *  === Implicit Conversions ===
 *  A number of commonly applied implicit conversions are also defined here, and
 *  in the parent type [[scala.LowPriorityImplicits]]. Implicit conversions
 *  are provided for the "widening" of numeric values, for instance, converting a
 *  Short value to a Long value as required, and to add additional higher-order
 *  functions to Array values. These are described in more detail in the documentation of [[scala.Array]].
 *
 * @groupname utilities Utility Methods
 * @groupprio utilities 10
 *
 * @groupname assertions Assertions
 * @groupprio assertions 20
 * @groupdesc assertions These methods support program verification and runtime correctness.
 *
 * @groupname console-output Console Output
 * @groupprio console-output 30
 * @groupdesc console-output These methods provide output via the console.
 *
 * @groupname type-constraints Type Constraints
 * @groupprio type-constraints 40
 * @groupdesc type-constraints These entities allows constraints between types to be stipulated.
 *
 * @groupname aliases Aliases
 * @groupprio aliases 50
 * @groupdesc aliases These aliases bring selected immutable types into scope without any imports.
 *
 * @groupname conversions-string String Conversions
 * @groupprio conversions-string 60
 * @groupdesc conversions-string Conversions to and from String and StringOps.
 *
 * @groupname implicit-classes-any Implicit Classes
 * @groupprio implicit-classes-any 70
 * @groupdesc implicit-classes-any These implicit classes add useful extension methods to every type.
 *
 * @groupname implicit-classes-char CharSequence Conversions
 * @groupprio implicit-classes-char 80
 * @groupdesc implicit-classes-char These implicit classes add CharSequence methods to Array[Char] and IndexedSeq[Char] instances.
 *
 * @groupname conversions-java-to-anyval Java to Scala
 * @groupprio conversions-java-to-anyval 90
 * @groupdesc conversions-java-to-anyval Implicit conversion from Java primitive wrapper types to Scala equivalents.
 *
 * @groupname conversions-anyval-to-java Scala to Java
 * @groupprio conversions-anyval-to-java 100
 * @groupdesc conversions-anyval-to-java Implicit conversion from Scala AnyVals to Java primitive wrapper types equivalents.
 *
 * @groupname conversions-array-to-wrapped-array Array to WrappedArray
 * @groupprio conversions-array-to-wrapped-array 110
 * @groupdesc conversions-array-to-wrapped-array Conversions from Arrays to WrappedArrays.
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
   * @group utilities
   */
  def classOf[T]: Class[T] = null // This is a stub method. The actual implementation is filled in by the compiler.

  /**
   * Retrieve the single value of a type with a unique inhabitant.
   *
   * @example {{{
   * object Foo
   * val foo = valueOf[Foo.type]
   * // foo is Foo.type = Foo
   *
   * val bar = valueOf[23]
   * // bar is 23.type = 23
   * }}}
   * @group utilities
   */
  @inline def valueOf[T](implicit vt: ValueOf[T]): T = vt.value

  /** The `String` type in Scala has methods that come either from the underlying
   *  Java String (see the documentation corresponding to your Java version, for
   *  example [[http://docs.oracle.com/javase/8/docs/api/java/lang/String.html]]) or
   *  are added implicitly through [[scala.collection.immutable.StringOps]].
   *  @group aliases
   */
  type String        = java.lang.String
  /**  @group aliases */
  type Class[T]      = java.lang.Class[T]

  // miscellaneous -----------------------------------------------------
  scala.`package`                         // to force scala package object to be seen.
  scala.collection.immutable.List         // to force Nil, :: to be seen.

  /**  @group aliases */
  type Function[-A, +B] = Function1[A, B]

  /**  @group aliases */
  type Map[A, +B] = immutable.Map[A, B]
  /**  @group aliases */
  type Set[A]     = immutable.Set[A]
  /**  @group aliases */
  val Map         = immutable.Map
  /**  @group aliases */
  val Set         = immutable.Set

  /**
   * Allows destructuring tuples with the same syntax as constructing them.
   *
   * @example {{{
   * val tup = "foobar" -> 3
   *
   * val c = tup match {
   *   case str -> i => str.charAt(i)
   * }
   * }}}
   * @group aliases
   */
  val ->        = Tuple2

  // Manifest types, companions, and incantations for summoning
  // TODO undeprecated until Scala reflection becomes non-experimental
  // @deprecated("this notion doesn't have a corresponding concept in 2.10, because scala.reflect.runtime.universe.TypeTag can capture arbitrary types. Use type tags instead of manifests, and there will be no need in opt manifests.", "2.10.0")
  type OptManifest[T]   = scala.reflect.OptManifest[T]
  @annotation.implicitNotFound(msg = "No Manifest available for ${T}.")
  // TODO undeprecated until Scala reflection becomes non-experimental
  // @deprecated("use `scala.reflect.ClassTag` (to capture erasures) or scala.reflect.runtime.universe.TypeTag (to capture types) or both instead", "2.10.0")
  type Manifest[T]      = scala.reflect.Manifest[T]
  // TODO undeprecated until Scala reflection becomes non-experimental
  // @deprecated("use `scala.reflect.ClassTag` (to capture erasures) or scala.reflect.runtime.universe.TypeTag (to capture types) or both instead", "2.10.0")
  val Manifest          = scala.reflect.Manifest
  // TODO undeprecated until Scala reflection becomes non-experimental
  // @deprecated("this notion doesn't have a corresponding concept in 2.10, because scala.reflect.runtime.universe.TypeTag can capture arbitrary types. Use type tags instead of manifests, and there will be no need in opt manifests.", "2.10.0")
  val NoManifest        = scala.reflect.NoManifest

  // TODO undeprecated until Scala reflection becomes non-experimental
  // @deprecated("use scala.reflect.classTag[T] and scala.reflect.runtime.universe.typeTag[T] instead", "2.10.0")
  def manifest[T](implicit m: Manifest[T])           = m
  // TODO undeprecated until Scala reflection becomes non-experimental
  // @deprecated("this notion doesn't have a corresponding concept in 2.10, because scala.reflect.runtime.universe.TypeTag can capture arbitrary types. Use type tags instead of manifests, and there will be no need in opt manifests.", "2.10.0")
  def optManifest[T](implicit m: OptManifest[T])     = m

  // Minor variations on identity functions
  /** @group utilities */
  @inline def identity[A](x: A): A         = x    // @see `conforms` for the implicit version
  /** @group utilities */
  @inline def implicitly[T](implicit e: T) = e    // for summoning implicit values from the nether world -- TODO: when dependent method types are on by default, give this result type `e.type`, so that inliner has better chance of knowing which method to inline in calls like `implicitly[MatchingStrategy[Option]].zero`
  /** @group utilities */
  @inline def locally[T](x: T): T  = x    // to communicate intent and avoid unmoored statements

  // assertions ---------------------------------------------------------

  /** Tests an expression, throwing an `AssertionError` if false.
   *  Calls to this method will not be generated if `-Xelide-below`
   *  is greater than `ASSERTION`.
   *
   *  @see [[scala.annotation.elidable elidable]]
   *  @param assertion   the expression to test
   *  @group assertions
   */
  @elidable(ASSERTION)
  def assert(assertion: Boolean) {
    if (!assertion)
      throw new java.lang.AssertionError("assertion failed")
  }

  /** Tests an expression, throwing an `AssertionError` if false.
   *  Calls to this method will not be generated if `-Xelide-below`
   *  is greater than `ASSERTION`.
   *
   *  @see [[scala.annotation.elidable elidable]]
   *  @param assertion   the expression to test
   *  @param message     a String to include in the failure message
   *  @group assertions
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
   *  will not be generated if `-Xelide-below` is greater than `ASSERTION`.
   *
   *  @see [[scala.annotation.elidable elidable]]
   *  @param assumption   the expression to test
   *  @group assertions
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
   *  will not be generated if `-Xelide-below` is greater than `ASSERTION`.
   *
   *  @see [[scala.annotation.elidable elidable]]
   *  @param assumption   the expression to test
   *  @param message      a String to include in the failure message
   *  @group assertions
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
   *  @group assertions
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
   *  @group assertions
   */
  @inline final def require(requirement: Boolean, message: => Any) {
    if (!requirement)
      throw new IllegalArgumentException("requirement failed: "+ message)
  }

  /** `???` can be used for marking methods that remain to be implemented.
   *  @throws NotImplementedError
   *  @group utilities
   */
  def ??? : Nothing = throw new NotImplementedError

  // implicit classes -----------------------------------------------------

  /** @group implicit-classes-any */
  implicit final class ArrowAssoc[A](private val self: A) extends AnyVal {
    @inline def -> [B](y: B): Tuple2[A, B] = Tuple2(self, y)
    def →[B](y: B): Tuple2[A, B] = ->(y)
  }

  /** @group implicit-classes-any */
  implicit final class Ensuring[A](private val self: A) extends AnyVal {
    def ensuring(cond: Boolean): A = { assert(cond); self }
    def ensuring(cond: Boolean, msg: => Any): A = { assert(cond, msg); self }
    def ensuring(cond: A => Boolean): A = { assert(cond(self)); self }
    def ensuring(cond: A => Boolean, msg: => Any): A = { assert(cond(self), msg); self }
  }

  /** @group implicit-classes-any */
  implicit final class StringFormat[A](private val self: A) extends AnyVal {
    /** Returns string formatted according to given `format` string.
     *  Format strings are as for `String.format`
     *  (@see java.lang.String.format).
     */
    @inline def formatted(fmtstr: String): String = fmtstr format self
  }

  // scala/bug#8229 retaining the pre 2.11 name for source compatibility in shadowing this implicit
  /** @group implicit-classes-any */
  implicit final class any2stringadd[A](private val self: A) extends AnyVal {
    def +(other: String): String = String.valueOf(self) + other
  }

  implicit final class SeqCharSequence(sequenceOfChars: scala.collection.IndexedSeq[Char]) extends CharSequence {
    def length: Int                                     = sequenceOfChars.length
    def charAt(index: Int): Char                        = sequenceOfChars(index)
    def subSequence(start: Int, end: Int): CharSequence = new SeqCharSequence(sequenceOfChars.slice(start, end))
    override def toString                               = sequenceOfChars mkString ""
  }

  /** @group implicit-classes-char */
  implicit final class ArrayCharSequence(arrayOfChars: Array[Char]) extends CharSequence {
    def length: Int                                     = arrayOfChars.length
    def charAt(index: Int): Char                        = arrayOfChars(index)
    def subSequence(start: Int, end: Int): CharSequence = new runtime.ArrayCharSequence(arrayOfChars, start, end)
    override def toString                               = arrayOfChars mkString ""
  }

  /** @group conversions-string */
  @inline implicit def augmentString(x: String): StringOps = new StringOps(x)

  // printing -----------------------------------------------------------

  /** Prints an object to `out` using its `toString` method.
   *
   *  @param x the object to print; may be null.
   *  @group console-output
   */
  def print(x: Any) = Console.print(x)

  /** Prints a newline character on the default output.
   *  @group console-output
   */
  def println() = Console.println()

  /** Prints out an object to the default output, followed by a newline character.
   *
   *  @param x the object to print.
   *  @group console-output
   */
  def println(x: Any) = Console.println(x)

  /** Prints its arguments as a formatted string to the default output,
   *  based on a string pattern (in a fashion similar to printf in C).
   *
   *  The interpretation of the formatting patterns is described in
   *  [[java.util.Formatter]].
   *
   *  Consider using the [[scala.StringContext.f f interpolator]] as more type safe and idiomatic.
   *
   *  @param text the pattern for formatting the arguments.
   *  @param args the arguments used to instantiating the pattern.
   *  @throws java.lang.IllegalArgumentException if there was a problem with the format string or arguments
   *
   *  @see [[scala.StringContext.f StringContext.f]]
   *  @group console-output
   */
  def printf(text: String, xs: Any*) = Console.print(text.format(xs: _*))

  // views --------------------------------------------------------------

  implicit def tuple2ToZippedOps[T1, T2](x: (T1, T2))                           = new runtime.Tuple2Zipped.Ops(x)
  implicit def tuple3ToZippedOps[T1, T2, T3](x: (T1, T2, T3))                   = new runtime.Tuple3Zipped.Ops(x)

  // Not specialized anymore since 2.13 but we still need separate methods
  // to avoid https://github.com/scala/bug/issues/10746
  implicit def genericArrayOps[T](xs: Array[T]): ArrayOps[T]          = new ArrayOps(xs)
  implicit def booleanArrayOps(xs: Array[Boolean]): ArrayOps[Boolean] = new ArrayOps(xs)
  implicit def byteArrayOps(xs: Array[Byte]): ArrayOps[Byte]          = new ArrayOps(xs)
  implicit def charArrayOps(xs: Array[Char]): ArrayOps[Char]          = new ArrayOps(xs)
  implicit def doubleArrayOps(xs: Array[Double]): ArrayOps[Double]    = new ArrayOps(xs)
  implicit def floatArrayOps(xs: Array[Float]): ArrayOps[Float]       = new ArrayOps(xs)
  implicit def intArrayOps(xs: Array[Int]): ArrayOps[Int]             = new ArrayOps(xs)
  implicit def longArrayOps(xs: Array[Long]): ArrayOps[Long]          = new ArrayOps(xs)
  implicit def refArrayOps[T <: AnyRef](xs: Array[T]): ArrayOps[T]    = new ArrayOps(xs)
  implicit def shortArrayOps(xs: Array[Short]): ArrayOps[Short]       = new ArrayOps(xs)
  implicit def unitArrayOps(xs: Array[Unit]): ArrayOps[Unit]          = new ArrayOps(xs)

  // "Autoboxing" and "Autounboxing" ---------------------------------------------------

  /** @group conversions-anyval-to-java */
  implicit def byte2Byte(x: Byte): java.lang.Byte             = x.asInstanceOf[java.lang.Byte]
  /** @group conversions-anyval-to-java */
  implicit def short2Short(x: Short): java.lang.Short         = x.asInstanceOf[java.lang.Short]
  /** @group conversions-anyval-to-java */
  implicit def char2Character(x: Char): java.lang.Character   = x.asInstanceOf[java.lang.Character]
  /** @group conversions-anyval-to-java */
  implicit def int2Integer(x: Int): java.lang.Integer         = x.asInstanceOf[java.lang.Integer]
  /** @group conversions-anyval-to-java */
  implicit def long2Long(x: Long): java.lang.Long             = x.asInstanceOf[java.lang.Long]
  /** @group conversions-anyval-to-java */
  implicit def float2Float(x: Float): java.lang.Float         = x.asInstanceOf[java.lang.Float]
  /** @group conversions-anyval-to-java */
  implicit def double2Double(x: Double): java.lang.Double     = x.asInstanceOf[java.lang.Double]
  /** @group conversions-anyval-to-java */
  implicit def boolean2Boolean(x: Boolean): java.lang.Boolean = x.asInstanceOf[java.lang.Boolean]

  /** @group conversions-java-to-anyval */
  implicit def Byte2byte(x: java.lang.Byte): Byte             = x.asInstanceOf[Byte]
  /** @group conversions-java-to-anyval */
  implicit def Short2short(x: java.lang.Short): Short         = x.asInstanceOf[Short]
  /** @group conversions-java-to-anyval */
  implicit def Character2char(x: java.lang.Character): Char   = x.asInstanceOf[Char]
  /** @group conversions-java-to-anyval */
  implicit def Integer2int(x: java.lang.Integer): Int         = x.asInstanceOf[Int]
  /** @group conversions-java-to-anyval */
  implicit def Long2long(x: java.lang.Long): Long             = x.asInstanceOf[Long]
  /** @group conversions-java-to-anyval */
  implicit def Float2float(x: java.lang.Float): Float         = x.asInstanceOf[Float]
  /** @group conversions-java-to-anyval */
  implicit def Double2double(x: java.lang.Double): Double     = x.asInstanceOf[Double]
  /** @group conversions-java-to-anyval */
  implicit def Boolean2boolean(x: java.lang.Boolean): Boolean = x.asInstanceOf[Boolean]

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
   * @group type-constraints
   */
  @implicitNotFound(msg = "Cannot prove that ${From} <:< ${To}.")
  sealed abstract class <:<[-From, +To] extends (From => To) with Serializable {
    /** substitute the To in the type F[To] to From
     */
    def substitute[F[-_]](ft: F[To]): F[From]
    /** create a new evidence for a covariant type F[_]
     */
    def liftCo[F[+_]]: <:<[F[From], F[To]] = {
      type G[-T] = F[T] <:< F[To]
      substitute[G](implicitly[F[To] <:< F[To]])
    }
    /** create a new evidence for a contravariant type F[_]
     */
    def liftContra[F[-_]]: <:<[F[To], F[From]] = {
      type G[-T] = F[To] <:< F[T]
      substitute[G](implicitly[F[To] <:< F[To]])
    }
  }
  private[this] final val singleton_<:< = new <:<[Any,Any] {
    def apply(x: Any): Any = x
    def substitute[F[-_]](ft: F[Any]): F[Any] = ft
  }
  // The dollar prefix is to dodge accidental shadowing of this method
  // by a user-defined method of the same name (scala/bug#7788).
  // The collections rely on this method.
  /** @group type-constraints */
  implicit def $conforms[A]: A <:< A = singleton_<:<.asInstanceOf[A <:< A]

  /** An instance of `A =:= B` witnesses that the types `A` and `B` are equal.
   *
   * @see `<:<` for expressing subtyping constraints
   * @group type-constraints
   */
  @implicitNotFound(msg = "Cannot prove that ${From} =:= ${To}.")
  sealed abstract class =:=[From, To] extends (From => To) with Serializable {
    /** substitute the From in the type F[From] to To
     */
    def substitute[F[_]](ff: F[From]): F[To]
    /** create an instance for a type constructor F[_]
     */
    def liftTo[F[_]]: =:=[F[From], F[To]] = {
      type G[T] = F[From] =:= F[T]
      substitute[G](implicitly[F[From] =:= F[From]])
    }
    /** flip the order of the type paraemeters
     */
    def flip: =:=[To, From] = {
      type G[T] = T =:= From
      substitute[G](implicitly[From =:= From])
    }
  }
  private[this] final val singleton_=:= = new =:=[Any,Any] {
    def apply(x: Any): Any = x
    def substitute[F[_]](ff: F[Any]): F[Any] = ff
  }
  /** @group type-constraints */
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

/** The `LowPriorityImplicits` class provides implicit values that
*  are valid in all Scala compilation units without explicit qualification,
*  but that are partially overridden by higher-priority conversions in object
*  `Predef`.
*
*  @author  Martin Odersky
*  @since 2.8
*/
// scala/bug#7335 Parents of Predef are defined in the same compilation unit to avoid
// cyclic reference errors compiling the standard library *without* a previously
// compiled copy on the classpath.
private[scala] abstract class LowPriorityImplicits extends LowPriorityImplicits2 {
  import mutable.WrappedArray
  //import immutable.WrappedString

  /** We prefer the java.lang.* boxed types to these wrappers in
   *  any potential conflicts.  Conflicts do exist because the wrappers
   *  need to implement ScalaNumber in order to have a symmetric equals
   *  method, but that implies implementing java.lang.Number as well.
   *
   *  Note - these are inlined because they are value classes, but
   *  the call to xxxWrapper is not eliminated even though it does nothing.
   *  Even inlined, every call site does a no-op retrieval of Predef's MODULE$
   *  because maybe loading Predef has side effects!
   */
  @inline implicit def byteWrapper(x: Byte)       = new runtime.RichByte(x)
  @inline implicit def shortWrapper(x: Short)     = new runtime.RichShort(x)
  @inline implicit def intWrapper(x: Int)         = new runtime.RichInt(x)
  @inline implicit def charWrapper(c: Char)       = new runtime.RichChar(c)
  @inline implicit def longWrapper(x: Long)       = new runtime.RichLong(x)
  @inline implicit def floatWrapper(x: Float)     = new runtime.RichFloat(x)
  @inline implicit def doubleWrapper(x: Double)   = new runtime.RichDouble(x)
  @inline implicit def booleanWrapper(x: Boolean) = new runtime.RichBoolean(x)

  /** @group conversions-array-to-wrapped-array */
  implicit def genericWrapArray[T](xs: Array[T]): WrappedArray[T] =
    if (xs eq null) null
    else WrappedArray.make(xs)

  // Since the JVM thinks arrays are covariant, one 0-length Array[AnyRef]
  // is as good as another for all T <: AnyRef.  Instead of creating 100,000,000
  // unique ones by way of this implicit, let's share one.
  /** @group conversions-array-to-wrapped-array */
  implicit def wrapRefArray[T <: AnyRef](xs: Array[T]): WrappedArray.ofRef[T] = {
    if (xs eq null) null
    else if (xs.length == 0) WrappedArray.empty[AnyRef].asInstanceOf[WrappedArray.ofRef[T]]
    else new WrappedArray.ofRef[T](xs)
  }

  /** @group conversions-array-to-wrapped-array */
  implicit def wrapIntArray(xs: Array[Int]): WrappedArray.ofInt = if (xs ne null) new WrappedArray.ofInt(xs) else null
  /** @group conversions-array-to-wrapped-array */
  implicit def wrapDoubleArray(xs: Array[Double]): WrappedArray.ofDouble = if (xs ne null) new WrappedArray.ofDouble(xs) else null
  /** @group conversions-array-to-wrapped-array */
  implicit def wrapLongArray(xs: Array[Long]): WrappedArray.ofLong = if (xs ne null) new WrappedArray.ofLong(xs) else null
  /** @group conversions-array-to-wrapped-array */
  implicit def wrapFloatArray(xs: Array[Float]): WrappedArray.ofFloat = if (xs ne null) new WrappedArray.ofFloat(xs) else null
  /** @group conversions-array-to-wrapped-array */
  implicit def wrapCharArray(xs: Array[Char]): WrappedArray.ofChar = if (xs ne null) new WrappedArray.ofChar(xs) else null
  /** @group conversions-array-to-wrapped-array */
  implicit def wrapByteArray(xs: Array[Byte]): WrappedArray.ofByte = if (xs ne null) new WrappedArray.ofByte(xs) else null
  /** @group conversions-array-to-wrapped-array */
  implicit def wrapShortArray(xs: Array[Short]): WrappedArray.ofShort = if (xs ne null) new WrappedArray.ofShort(xs) else null
  /** @group conversions-array-to-wrapped-array */
  implicit def wrapBooleanArray(xs: Array[Boolean]): WrappedArray.ofBoolean = if (xs ne null) new WrappedArray.ofBoolean(xs) else null
  /** @group conversions-array-to-wrapped-array */
  implicit def wrapUnitArray(xs: Array[Unit]): WrappedArray.ofUnit = if (xs ne null) new WrappedArray.ofUnit(xs) else null

  /** @group conversions-string */
  implicit def wrapString(s: String): WrappedString = if (s ne null) new WrappedString(s) else null
}

private[scala] abstract class LowPriorityImplicits2 {
  @deprecated("Implicit conversions from Array to immutable.IndexedSeq are implemented by copying; Use the more efficient non-copying ImmutableArray.unsafeWrapArray or an explicit toIndexedSeq call", "2.13.0")
  implicit def copyArrayToImmutableIndexedSeq[T](xs: Array[T]): IndexedSeq[T] =
    if (xs eq null) null
    else new ArrayOps(xs).toIndexedSeq
}
