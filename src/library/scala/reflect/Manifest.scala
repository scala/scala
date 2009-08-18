/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


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
trait Manifest[T] extends OptManifest[T] {

  /** A class representing the type U to which T would be erased. Note
    * that there is no subtyping relationship between T and U. */
  def erasure: Predef.Class[U] forSome { type U }

  /** Tests whether the type represented by this manifest is a subtype of
    * the type represented by `that' manifest. BE AWARE: the current
    * implementation is an approximation, as the test is done on the
    * erasure of the type. */
  def <:<(that: Manifest[_]): Boolean = {
    def subtype(sub: Predef.Class[_], sup: Predef.Class[_]): Boolean = {
      val subSuperClass = sub.getSuperclass
      val subSuperInterfaces = sub.getInterfaces.toList
      val subSuper =
        (if (subSuperClass == null) Nil else List(subSuperClass)) ::: subSuperInterfaces
      (subSuper contains sup) || (subSuper exists (subtype(_, sup)))
    }
    this.erasure == that.erasure || subtype(this.erasure, that.erasure)
  }

  /** Tests whether the type represented by this manifest is a supertype
    * of the type represented by `that' manifest. BE AWARE: the current
    * implementation is an approximation, as the test is done on the
    * erasure of the type. */
  def >:>(that: Manifest[_]): Boolean =
    that <:< this

  /** Tests whether the type represented by this manifest is equal to the
    * type represented by `that' manifest. BE AWARE: the current
    * implementation is an approximation, as the test is done on the
    * erasure of the type. */
  override def equals(that: Any): Boolean = that match {
    case m:Manifest[_] => this.erasure == m.erasure
    case _ => false
  }

  def newArray(len: Int): BoxedArray[T] = {
    // it's safe to assume T <: AnyRef here because the method is overridden for all value type manifests
    new BoxedObjectArray(java.lang.reflect.Array.newInstance(erasure, len).asInstanceOf[Array[AnyRef]])
      .asInstanceOf[BoxedArray[T]]
  }

  private[reflect] def typeArguments: Option[List[Manifest[_]]] = None

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
object Manifest {

  val Byte = new (Manifest[Byte] @serializable) {
    def erasure = java.lang.Byte.TYPE
    override def toString = "Byte"
    override def newArray(len: Int): BoxedArray[Byte] = new BoxedByteArray(new Array[Byte](len))
  }

  val Short = new (Manifest[Short] @serializable) {
    def erasure = java.lang.Short.TYPE
    override def toString = "Short"
    override def newArray(len: Int): BoxedArray[Short] = new BoxedShortArray(new Array[Short](len))
  }

  val Char = new (Manifest[Char] @serializable) {
    def erasure = java.lang.Character.TYPE
    override def toString = "Char"
    override def newArray(len: Int): BoxedArray[Char] = new BoxedCharArray(new Array[Char](len))
  }

  val Int = new (Manifest[Int] @serializable) {
    def erasure = java.lang.Integer.TYPE
    override def toString = "Int"
    override def newArray(len: Int): BoxedArray[Int] = new BoxedIntArray(new Array[Int](len))
  }

  val Long = new (Manifest[Long] @serializable) {
    def erasure = java.lang.Long.TYPE
    override def toString = "Long"
    override def newArray(len: Int): BoxedArray[Long] = new BoxedLongArray(new Array[Long](len))
  }

  val Float = new (Manifest[Float] @serializable) {
    def erasure = java.lang.Float.TYPE
    override def toString = "Float"
    override def newArray(len: Int): BoxedArray[Float] = new BoxedFloatArray(new Array[Float](len))
  }

  val Double = new (Manifest[Double] @serializable) {
    def erasure = java.lang.Double.TYPE
    override def toString = "Double"
    override def newArray(len: Int): BoxedArray[Double] = new BoxedDoubleArray(new Array[Double](len))
  }

  val Boolean = new (Manifest[Boolean] @serializable) {
    def erasure = java.lang.Boolean.TYPE
    override def toString = "Boolean"
    override def newArray(len: Int): BoxedArray[Boolean] = new BoxedBooleanArray(new Array[Boolean](len))
  }

  val Unit = new (Manifest[Unit] @serializable) {
    def erasure = java.lang.Void.TYPE
    override def toString = "Unit"
    override def newArray(len: Int): BoxedArray[Unit] = new BoxedUnitArray(new Array[Unit](len))
  }

  /** Manifest for the singleton type `value.type'. */
  def singleType[T](value: Any): Manifest[T] =
    new (Manifest[T] @serializable) {
      lazy val erasure =
        value match {
          case anyRefValue: AnyRef => anyRefValue.getClass
          case anyValue => error("There is no singleton type for AnyVal values")
        }
      override lazy val toString = value.toString + ".type"
    }

  /** Manifest for the class type `clazz', where `clazz' is
    * a top-level or static class. */
  def classType[T](clazz: Predef.Class[T]): Manifest[T] =
    new (Manifest[T] @serializable) {
      val erasure = clazz
      // Martin why is toString a lazy val instead of a def?
      override lazy val toString = erasure.getName
    }

  /** Manifest for the class type `clazz[args]', where `clazz' is
    * a top-level or static class. */
  def classType[T](clazz: Predef.Class[_], args: Manifest[_]*): Manifest[T] =
    new (Manifest[T] @serializable) {
      val erasure = clazz
      private[reflect] override val typeArguments = Some(args.toList)
      override def <:<(that: Manifest[_]): Boolean = {
        that.typeArguments match {
          case Some(thatArgs) =>
            super.<:<(that) && ((args zip thatArgs) forall { case (x, y) => x <:< y })
          case None =>
            false
        }
      }
      override lazy val toString =
        (if (erasure.isArray) "Array" else erasure.getName) +
        args.toList.mkString("[", ", ", "]")
    }

  /** Manifest for the class type `prefix # clazz'. */
  def classType[T](prefix: Manifest[_], clazz: Predef.Class[_]): Manifest[T] =
    new (Manifest[T] @serializable) {
      val erasure = clazz
      override lazy val toString = prefix.toString + "#" + erasure.getName
    }

  /** Manifest for the class type `prefix # clazz[args]'. */
  def classType[T](prefix: Manifest[_], clazz: Predef.Class[_], args: Manifest[_]*): Manifest[T] =
    new (Manifest[T] @serializable) {
      val erasure = clazz
      private[reflect] override val typeArguments = Some(args.toList)
      override lazy val toString =
        prefix.toString + "#" + erasure.getName + typeArguments.mkString("[", ", ", "]")
    }

  /** Manifest for the abstract type `prefix # name'. `upperBound' is not
    * strictly necessary as it could be obtained by reflection. It was
    * added so that erasure can be calculated without reflection. */
  def abstractType[T](prefix: Manifest[_], name: String, upperBound: Manifest[_]): Manifest[T] =
    new (Manifest[T] @serializable) {
      lazy val erasure = upperBound.erasure
      override lazy val toString = prefix.toString + "#" + name
    }

  /** Manifest for the abstract type `prefix # name[args]'. */
  def abstractType[T](prefix: Manifest[_], name: String, upperBound: Manifest[_], args: Manifest[_]*): Manifest[T] =
    new (Manifest[T] @serializable) {
      lazy val erasure = upperBound.erasure
      private[reflect] override val typeArguments = Some(args.toList)
      override lazy val toString =
        prefix.toString + "#" + name + typeArguments.mkString("[", ", ", "]")
    }

  /** Manifest for the intersection type `parents_0 with ... with parents_n'. */
  def intersectionType[T](parents: Manifest[_]*): Manifest[T] =
    new (Manifest[T] @serializable) {
      lazy val erasure = parents.head.erasure
      override lazy val toString = parents.mkString(" with ")
    }

}
