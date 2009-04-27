/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.reflect

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
      override lazy val toString = erasure.getName
    }

  /** Manifest for the class type `clazz[args]', where `clazz' is
    * a top-level or static class. */
  def classType[T](clazz: Predef.Class[_], args: Manifest[_]*): Manifest[T] =
    new (Manifest[T] @serializable) {
      val erasure = clazz
      val typeArguments: Seq[Manifest[_]] = args
      override def <:<(that: Manifest[_]): Boolean = {
        try {
          val meth = that.getClass().getMethod("typeArguments", null)
          val args1 = meth.invoke(that, null).asInstanceOf[Array[Manifest[_]]]
          super.<:<(that) && args.equalsWith(args1)((x, y) => x <:< y)
        } catch {
          case _ => super.<:<(that)
        }
      }
      override lazy val toString =
        (if (erasure.isArray) "Array" else erasure.getName) +
        typeArguments.mkString("[", ", ", "]")
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
      val typeArguments: Seq[Manifest[_]] = args
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
      val typeArguments: Seq[Manifest[_]] = args
      override lazy val toString =
        prefix.toString + "#" + name + typeArguments.mkString("[", ", ", "]")
    }

  /** Manifest for the intersection type `parents_0 with ... with parents_n'. */
  def intersectionType[T](parents: Manifest[_]*): Manifest[T] =
    new (Manifest[T] @serializable) {
      lazy val erasure = parents.first.erasure
      override lazy val toString = parents.mkString(" with ")
    }

}
