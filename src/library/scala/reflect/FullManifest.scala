/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Manifest.scala 18506 2009-08-18 17:24:36Z odersky $


package scala.reflect

import scala.runtime._

/** <p>
  *   A <code>Manifest[T]</code> is an opaque descriptor for type <code>T</code>.
  *   Currently, its only use is to give access to the erasure of the type as a
  *   <code>Class</code> instance.
  * </p>
  * <p>
  *   <b>BE AWARE</b>: The different type-relation operators are all forwarded
  *   to the erased type as an approximation of the final semantics where
  *   these operators should be on the unerased type.
  * </p>
  */
@serializable
trait FullManifest[T] extends Manifest[T] {
  override def typeArguments: List[FullManifest[_]] = List()
}

/** <p>
  *   This object is used by the compiler and <b>should not be used in client
  *   code</b>. The object <code>Manifest</code> defines factory methods for
  *   manifests.
  * </p>
  * <p>
  *   <b>BE AWARE</b>: The factory for refinement types is missing and
  *   will be implemented in a later version of this class.
  * </p>
  */
object FullManifest {

  val Byte = new (FullManifest[Byte] @serializable) {
    def erasure = java.lang.Byte.TYPE
    override def toString = "Byte"
    override def newArray(len: Int): BoxedArray[Byte] = new BoxedByteArray(new Array[Byte](len))
  }

  val Short = new (FullManifest[Short] @serializable) {
    def erasure = java.lang.Short.TYPE
    override def toString = "Short"
    override def newArray(len: Int): BoxedArray[Short] = new BoxedShortArray(new Array[Short](len))
  }

  val Char = new (FullManifest[Char] @serializable) {
    def erasure = java.lang.Character.TYPE
    override def toString = "Char"
    override def newArray(len: Int): BoxedArray[Char] = new BoxedCharArray(new Array[Char](len))
  }

  val Int = new (FullManifest[Int] @serializable) {
    def erasure = java.lang.Integer.TYPE
    override def toString = "Int"
    override def newArray(len: Int): BoxedArray[Int] = new BoxedIntArray(new Array[Int](len))
  }

  val Long = new (FullManifest[Long] @serializable) {
    def erasure = java.lang.Long.TYPE
    override def toString = "Long"
    override def newArray(len: Int): BoxedArray[Long] = new BoxedLongArray(new Array[Long](len))
  }

  val Float = new (FullManifest[Float] @serializable) {
    def erasure = java.lang.Float.TYPE
    override def toString = "Float"
    override def newArray(len: Int): BoxedArray[Float] = new BoxedFloatArray(new Array[Float](len))
  }

  val Double = new (FullManifest[Double] @serializable) {
    def erasure = java.lang.Double.TYPE
    override def toString = "Double"
    override def newArray(len: Int): BoxedArray[Double] = new BoxedDoubleArray(new Array[Double](len))
  }

  val Boolean = new (FullManifest[Boolean] @serializable) {
    def erasure = java.lang.Boolean.TYPE
    override def toString = "Boolean"
    override def newArray(len: Int): BoxedArray[Boolean] = new BoxedBooleanArray(new Array[Boolean](len))
  }

  val Unit = new (FullManifest[Unit] @serializable) {
    def erasure = java.lang.Void.TYPE
    override def toString = "Unit"
    override def newArray(len: Int): BoxedArray[Unit] = new BoxedUnitArray(new Array[Unit](len))
  }

  /** Manifest for the singleton type `value.type'. */
  def singleType[T](value: Any): FullManifest[T] =
    new (FullManifest[T] @serializable) {
      lazy val erasure =
        value match {
          case anyRefValue: AnyRef => anyRefValue.getClass
          case anyValue => error("There is no singleton type for AnyVal values")
        }
      override lazy val toString = value.toString + ".type"
    }

  /** Manifest for the class type `clazz', where `clazz' is
    * a top-level or static class. */
  def classType[T](clazz: Predef.Class[T], args: FullManifest[_]*): FullManifest[T] =
    classType(None, clazz, args: _*)

  /** Manifest for the class type `clazz[args]', where `clazz' is
    * a top-level or static class. */
  def classType[T](prefix: FullManifest[_], clazz: Predef.Class[_], args: FullManifest[_]*): FullManifest[T] =
    classType(Some(prefix), clazz, args: _*)

  /** Manifest for the class type `clazz[args]', where `clazz' is
    * a top-level or static class. */
  def classType[T](prefix: Option[FullManifest[_]], clazz: Predef.Class[_], args: FullManifest[_]*): FullManifest[T] =
    new (FullManifest[T] @serializable) {
      def erasure = clazz
      override val typeArguments = args.toList
      override def toString =
        (if (prefix.isEmpty) "" else prefix.get.toString+"#") +
        (if (erasure.isArray) "Array" else erasure.getName) +
        argString
    }

  /** Manifest for the abstract type `prefix # name'. `upperBound' is not
    * strictly necessary as it could be obtained by reflection. It was
    * added so that erasure can be calculated without reflection. */
  def abstractType[T](prefix: FullManifest[_], name: String, upperBound: FullManifest[_], args: FullManifest[_]*): FullManifest[T] =
    new (FullManifest[T] @serializable) {
      def erasure = upperBound.erasure
      override val typeArguments = args.toList
      override def toString = prefix.toString+"#"+name+argString
    }

  /** Manifest for the intersection type `parents_0 with ... with parents_n'. */
  def intersectionType[T](parents: FullManifest[_]*): FullManifest[T] =
    new (FullManifest[T] @serializable) {
      def erasure = parents.head.erasure
      override def toString = parents.mkString(" with ")
    }
}
