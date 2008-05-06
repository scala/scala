/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
package scala.reflect

/** A Manifest[T] is an opaque descriptor for type T. Currently, its only
  * use is to give access to the erasure of the type as a Class instance.
  * BE AWARE: The different type-relation operators are all forwarded to
  * the erased type as an approximation of the final semantics where
  * these operators should be on the unerased type. */
trait Manifest[T] {

  /** A class representing the type U to which T would be erased. Note
    * that there is no subtyping relationship between T and U. */
  def erasure: Predef.Class[U] forSome { type U }

  /** Tests whether the type represented by this manifest is a subtype of
    * the type represented by `that' manifest. BE AWARE: the current
    * implementation is an approximation, as the test is done on the
    * erasure of the type. */
  def <:<(that: Manifest[_]): Boolean = {
    def subtype(sub: Predef.Class[_], sup: Predef.Class[_]): Boolean = {
      val subSuperClass = sub.BaseType
      val subSuperInterfaces = sub.GetInterfaces.toList
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

/** This object is used by the compiler and <b>should not be used in
  * client code</b>. The object `Manifest' defines factory methods for
  * manifests. BE AWARE: The factory for refinement types is missing and
  * will be implemented in a later version of this class. */
object Manifest {

  /** Manifest for the singleton type `value.type'. */
  def singleType[T](value: Any): Manifest[T] =
    new Manifest[T] {
      lazy val erasure =
        value match {
          case anyRefValue: AnyRef => anyRefValue.GetType
          case anyValue => error("There is no singleton type for AnyVal values")
        }
      override lazy val toString = value.toString + ".type"
    }

  /** Manifest for the class type `clazz', where `clazz' is
    * a top-level or static class. */
  def classType[T](clazz: Predef.Class[T]): Manifest[T] =
    new Manifest[T] {
      val erasure = clazz
      override lazy val toString = erasure.getName
    }

  /** Manifest for the class type `clazz[args]', where `clazz' is
    * a top-level or static class. */
  def classType[T](clazz: Predef.Class[_], args: Manifest[_]*): Manifest[T] =
    new Manifest[T] {
      val erasure = clazz
      val typeArguments: Seq[Manifest[_]] = args
      override lazy val toString = erasure.getName + typeArguments.mkString("[", ", ", "]")
    }

  /** Manifest for the class type `prefix # clazz'. */
  def classType[T](prefix: Manifest[_], clazz: Predef.Class[_]): Manifest[T] =
    new Manifest[T] {
      val erasure = clazz
      override lazy val toString = prefix.toString + "#" + clazz.getName
    }

  /** Manifest for the class type `prefix # clazz[args]'. */
  def classType[T](prefix: Manifest[_], clazz: Predef.Class[_], args: Manifest[_]*): Manifest[T] =
    new Manifest[T] {
      val erasure = clazz
      val typeArguments: Seq[Manifest[_]] = args
      override lazy val toString = prefix.toString + "#" + clazz.getName + typeArguments.mkString("[", ", ", "]")
    }

  /** Manifest for the abstract type `prefix # name'. `upperBound' is not
    * strictly necessary as it could be obtained by reflection. It was
    * added so that erasure can be calculated without reflection. */
  def abstractType[T](prefix: Manifest[_], name: String, upperBound: Manifest[_]): Manifest[T] =
    new Manifest[T] {
      lazy val erasure = upperBound.erasure
      override lazy val  toString = prefix.toString + "#" + name
    }

  /** Manifest for the abstract type `prefix # name[args]'. */
  def abstractType[T](prefix: Manifest[_], name: String, upperBound: Manifest[_], args: Manifest[_]*): Manifest[T] =
    new Manifest[T] {
      lazy val erasure = upperBound.erasure
      val typeArguments: Seq[Manifest[_]] = args
      override lazy val  toString = prefix.toString + "#" + name + typeArguments.mkString("[", ", ", "]")
    }

  /** Manifest for the intersection type `parents_0 with ... with parents_n'. */
  def intersectionType[T](parents: Manifest[_]*): Manifest[T] =
    new Manifest[T] {
      lazy val erasure = parents.first.erasure
      override lazy val toString = parents.mkString(" with ")
    }

}
