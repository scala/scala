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
  def erasure: Predef.Class[_]

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
    def subargs(args1: List[OptManifest[_]], args2: List[OptManifest[_]]): Boolean = {
      (args1 zip args2) forall {
        case (x: Manifest[_], y: Manifest[_]) => x <:< y // !!! [Martin] this is wrong, need to take variance into account
        case (NoManifest, NoManifest) => true
        case _ => false
      }
    }
    (this.erasure == that.erasure || subtype(this.erasure, that.erasure)) &&
    subargs(this.typeArguments, that.typeArguments)
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
    case m: Manifest[_] => this.erasure == m.erasure
    case _ => false
  }

  def newArray(len: Int): BoxedArray[T] = {
    // it's safe to assume T <: AnyRef here because the method is overridden for all value type manifests
    new BoxedObjectArray(java.lang.reflect.Array.newInstance(erasure, len).asInstanceOf[Array[AnyRef]])
      .asInstanceOf[BoxedArray[T]]
  }

  def typeArguments: List[OptManifest[_]] = List()

  protected def argString = if (typeArguments.isEmpty) "" else typeArguments.mkString("[", ", ", "]")

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

  val Byte = FullManifest.Byte
  val Short = FullManifest.Short
  val Char = FullManifest.Char
  val Int = FullManifest.Int
  val Long = FullManifest.Long
  val Float = FullManifest.Float
  val Double = FullManifest.Double
  val Boolean = FullManifest.Boolean
  val Unit = FullManifest.Unit

  def singleType[T](value: Any): FullManifest[T] = FullManifest.singleType(value)

  /** Manifest for the class type `clazz[args]', where `clazz' is
    * a top-level or static class. */
  def classType[T](clazz: Predef.Class[_], args: OptManifest[_]*): Manifest[T] =
    classType(None, clazz, args: _*)

  /** Manifest for the class type `clazz[args]', where `clazz' is
    * a top-level or static class. */
  def classType[T](prefix: Manifest[_], clazz: Predef.Class[_], args: OptManifest[_]*): Manifest[T] =
    classType(Some(prefix), clazz, args: _*)

  /** Manifest for the class type `clazz[args]', where `clazz' is
    * a top-level or static class. */
  def classType[T](prefix: Option[Manifest[_]], clazz: Predef.Class[_], args: OptManifest[_]*): Manifest[T] =
    new (Manifest[T] @serializable) {
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
  def abstractType[T](prefix: Manifest[_], name: String, upperBound: Manifest[_], args: OptManifest[_]*): Manifest[T] =
    new (Manifest[T] @serializable) {
      def erasure = upperBound.erasure
      override val typeArguments = args.toList
      override def toString = prefix.toString+"#"+name+argString
    }

  /** Manifest for the intersection type `parents_0 with ... with parents_n'. */
  def intersectionType[T](parents: Manifest[_]*): Manifest[T] =
    new (Manifest[T] @serializable) {
      def erasure = parents.head.erasure
      override def toString = parents.mkString(" with ")
    }
}
