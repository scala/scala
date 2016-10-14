/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package reflect

import scala.collection.mutable.{ WrappedArray, ArrayBuilder }
import java.lang.{ Class => jClass }

@deprecated("use scala.reflect.ClassTag instead", "2.10.0")
trait ClassManifestDeprecatedApis[T] extends OptManifest[T] {
  self: ClassManifest[T] =>

  // Still in use in target test.junit.comp.
  @deprecated("use runtimeClass instead", "2.10.0")
  def erasure: jClass[_] = runtimeClass

  private def subtype(sub: jClass[_], sup: jClass[_]): Boolean = {
    def loop(left: Set[jClass[_]], seen: Set[jClass[_]]): Boolean = {
      left.nonEmpty && {
        val next = left.head
        val supers = next.getInterfaces.toSet ++ Option(next.getSuperclass)
        supers(sup) || {
          val xs = left ++ supers filterNot seen
          loop(xs - next, seen + next)
        }
      }
    }
    loop(Set(sub), Set())
  }

  private def subargs(args1: List[OptManifest[_]], args2: List[OptManifest[_]]) = (args1 corresponds args2) {
    // !!! [Martin] this is wrong, need to take variance into account
    case (x: ClassManifest[_], y: ClassManifest[_]) => x <:< y
    case (x, y)                                     => (x eq NoManifest) && (y eq NoManifest)
  }

  /** Tests whether the type represented by this manifest is a subtype
    * of the type represented by `that` manifest, subject to the limitations
    * described in the header.
    */
  @deprecated("use scala.reflect.runtime.universe.TypeTag for subtype checking instead", "2.10.0")
  def <:<(that: ClassManifest[_]): Boolean = {
    // All types which could conform to these types will override <:<.
    def cannotMatch = {
      import Manifest._
      that.isInstanceOf[AnyValManifest[_]] || (that eq AnyVal) || (that eq Nothing) || (that eq Null)
    }

    // This is wrong, and I don't know how it can be made right
    // without more development of Manifests, due to arity-defying
    // relationships like:
    //
    //   List[String] <: AnyRef
    //   Map[Int, Int] <: Iterable[(Int, Int)]
    //
    // Given the manifest for Map[A, B] how do I determine that a
    // supertype has single type argument (A, B) ? I don't see how we
    // can say whether X <:< Y when type arguments are involved except
    // when the erasure is the same, even before considering variance.
    !cannotMatch && {
      // this part is wrong for not considering variance
      if (this.runtimeClass == that.runtimeClass)
        subargs(this.typeArguments, that.typeArguments)
      // this part is wrong for punting unless the rhs has no type
      // arguments, but it's better than a blindfolded pinata swing.
      else
        that.typeArguments.isEmpty && subtype(this.runtimeClass, that.runtimeClass)
    }
  }

  /** Tests whether the type represented by this manifest is a supertype
    * of the type represented by `that` manifest, subject to the limitations
    * described in the header.
    */
  @deprecated("use scala.reflect.runtime.universe.TypeTag for subtype checking instead", "2.10.0")
  def >:>(that: ClassManifest[_]): Boolean =
    that <:< this

  override def canEqual(other: Any) = other match {
    case _: ClassManifest[_] => true
    case _                   => false
  }

  protected def arrayClass[T](tp: jClass[_]): jClass[Array[T]] =
    java.lang.reflect.Array.newInstance(tp, 0).getClass.asInstanceOf[jClass[Array[T]]]

  @deprecated("use wrap instead", "2.10.0")
  def arrayManifest: ClassManifest[Array[T]] =
    ClassManifest.classType[Array[T]](arrayClass[T](runtimeClass), this)

  override def newArray(len: Int): Array[T] =
    java.lang.reflect.Array.newInstance(runtimeClass, len).asInstanceOf[Array[T]]

  @deprecated("use wrap.newArray instead", "2.10.0")
  def newArray2(len: Int): Array[Array[T]] =
    java.lang.reflect.Array.newInstance(arrayClass[T](runtimeClass), len)
      .asInstanceOf[Array[Array[T]]]

  @deprecated("use wrap.wrap.newArray instead", "2.10.0")
  def newArray3(len: Int): Array[Array[Array[T]]] =
    java.lang.reflect.Array.newInstance(arrayClass[Array[T]](arrayClass[T](runtimeClass)), len)
      .asInstanceOf[Array[Array[Array[T]]]]

  @deprecated("use wrap.wrap.wrap.newArray instead", "2.10.0")
  def newArray4(len: Int): Array[Array[Array[Array[T]]]] =
    java.lang.reflect.Array.newInstance(arrayClass[Array[Array[T]]](arrayClass[Array[T]](arrayClass[T](runtimeClass))), len)
      .asInstanceOf[Array[Array[Array[Array[T]]]]]

  @deprecated("use wrap.wrap.wrap.wrap.newArray instead", "2.10.0")
  def newArray5(len: Int): Array[Array[Array[Array[Array[T]]]]] =
    java.lang.reflect.Array.newInstance(arrayClass[Array[Array[Array[T]]]](arrayClass[Array[Array[T]]](arrayClass[Array[T]](arrayClass[T](runtimeClass)))), len)
      .asInstanceOf[Array[Array[Array[Array[Array[T]]]]]]

  @deprecated("create WrappedArray directly instead", "2.10.0")
  def newWrappedArray(len: Int): WrappedArray[T] =
    // it's safe to assume T <: AnyRef here because the method is overridden for all value type manifests
    new WrappedArray.ofRef[T with AnyRef](newArray(len).asInstanceOf[Array[T with AnyRef]]).asInstanceOf[WrappedArray[T]]

  @deprecated("use ArrayBuilder.make(this) instead", "2.10.0")
  def newArrayBuilder(): ArrayBuilder[T] =
    // it's safe to assume T <: AnyRef here because the method is overridden for all value type manifests
    new ArrayBuilder.ofRef[T with AnyRef]()(this.asInstanceOf[ClassManifest[T with AnyRef]]).asInstanceOf[ArrayBuilder[T]]

  @deprecated("use scala.reflect.runtime.universe.TypeTag to capture type structure instead", "2.10.0")
  def typeArguments: List[OptManifest[_]] = List()

  protected def argString =
    if (typeArguments.nonEmpty) typeArguments.mkString("[", ", ", "]")
    else if (runtimeClass.isArray) "["+ClassManifest.fromClass(runtimeClass.getComponentType)+"]"
    else ""
}

/** `ClassManifestFactory` defines factory methods for manifests.
 *  It is intended for use by the compiler and should not be used in client code.
 *
 *  Unlike `ClassManifest`, this factory isn't annotated with a deprecation warning.
 *  This is done to prevent avalanches of deprecation warnings in the code that calls methods with manifests.
 *
 *  In a perfect world, we would just remove the @deprecated annotation from `ClassManifest` the object
 *  and then delete it in 2.11. After all, that object is explicitly marked as internal, so noone should use it.
 *  However a lot of existing libraries disregarded the scaladoc that comes with `ClassManifest`,
 *  so we need to somehow nudge them into migrating prior to removing stuff out of the blue.
 *  Hence we've introduced this design decision as the lesser of two evils.
 */
object ClassManifestFactory {
  val Byte    = ManifestFactory.Byte
  val Short   = ManifestFactory.Short
  val Char    = ManifestFactory.Char
  val Int     = ManifestFactory.Int
  val Long    = ManifestFactory.Long
  val Float   = ManifestFactory.Float
  val Double  = ManifestFactory.Double
  val Boolean = ManifestFactory.Boolean
  val Unit    = ManifestFactory.Unit
  val Any     = ManifestFactory.Any
  val Object  = ManifestFactory.Object
  val AnyVal  = ManifestFactory.AnyVal
  val Nothing = ManifestFactory.Nothing
  val Null    = ManifestFactory.Null

  def fromClass[T](clazz: jClass[T]): ClassManifest[T] = clazz match {
    case java.lang.Byte.TYPE      => Byte.asInstanceOf[ClassManifest[T]]
    case java.lang.Short.TYPE     => Short.asInstanceOf[ClassManifest[T]]
    case java.lang.Character.TYPE => Char.asInstanceOf[ClassManifest[T]]
    case java.lang.Integer.TYPE   => Int.asInstanceOf[ClassManifest[T]]
    case java.lang.Long.TYPE      => Long.asInstanceOf[ClassManifest[T]]
    case java.lang.Float.TYPE     => Float.asInstanceOf[ClassManifest[T]]
    case java.lang.Double.TYPE    => Double.asInstanceOf[ClassManifest[T]]
    case java.lang.Boolean.TYPE   => Boolean.asInstanceOf[ClassManifest[T]]
    case java.lang.Void.TYPE      => Unit.asInstanceOf[ClassManifest[T]]
    case _                        => classType[T with AnyRef](clazz).asInstanceOf[ClassManifest[T]]
  }

  def singleType[T <: AnyRef](value: AnyRef): Manifest[T] = Manifest.singleType(value)

  /** ClassManifest for the class type `clazz`, where `clazz` is
    * a top-level or static class.
    * @note This no-prefix, no-arguments case is separate because we
    *       it's called from ScalaRunTime.boxArray itself. If we
    *       pass varargs as arrays into this, we get an infinitely recursive call
    *       to boxArray. (Besides, having a separate case is more efficient)
    */
  def classType[T](clazz: jClass[_]): ClassManifest[T] =
    new ClassTypeManifest[T](None, clazz, Nil)

  /** ClassManifest for the class type `clazz[args]`, where `clazz` is
    * a top-level or static class and `args` are its type arguments */
  def classType[T](clazz: jClass[_], arg1: OptManifest[_], args: OptManifest[_]*): ClassManifest[T] =
    new ClassTypeManifest[T](None, clazz, arg1 :: args.toList)

  /** ClassManifest for the class type `clazz[args]`, where `clazz` is
    * a class with non-package prefix type `prefix` and type arguments `args`.
    */
  def classType[T](prefix: OptManifest[_], clazz: jClass[_], args: OptManifest[_]*): ClassManifest[T] =
    new ClassTypeManifest[T](Some(prefix), clazz, args.toList)

  def arrayType[T](arg: OptManifest[_]): ClassManifest[Array[T]] = arg match {
    case NoManifest          => Object.asInstanceOf[ClassManifest[Array[T]]]
    case m: ClassManifest[_] => m.asInstanceOf[ClassManifest[T]].arrayManifest
  }

  @SerialVersionUID(1L)
  private class AbstractTypeClassManifest[T](prefix: OptManifest[_], name: String, clazz: jClass[_], args: OptManifest[_]*) extends ClassManifest[T] {
    override def runtimeClass = clazz
    override val typeArguments = args.toList
    override def toString = prefix.toString+"#"+name+argString
  }

  /** ClassManifest for the abstract type `prefix # name`. `upperBound` is not
    * strictly necessary as it could be obtained by reflection. It was
    * added so that erasure can be calculated without reflection. */
  def abstractType[T](prefix: OptManifest[_], name: String, clazz: jClass[_], args: OptManifest[_]*): ClassManifest[T] =
    new AbstractTypeClassManifest(prefix, name, clazz)

  /** ClassManifest for the abstract type `prefix # name`. `upperBound` is not
    * strictly necessary as it could be obtained by reflection. It was
    * added so that erasure can be calculated without reflection.
    * todo: remove after next bootstrap
    */
  def abstractType[T](prefix: OptManifest[_], name: String, upperbound: ClassManifest[_], args: OptManifest[_]*): ClassManifest[T] =
    new AbstractTypeClassManifest(prefix, name, upperbound.runtimeClass)
}

/** Manifest for the class type `clazz[args]`, where `clazz` is
  * a top-level or static class */
@SerialVersionUID(1L)
private class ClassTypeManifest[T](
  prefix: Option[OptManifest[_]],
  val runtimeClass: jClass[_],
  override val typeArguments: List[OptManifest[_]]) extends ClassManifest[T]
{
  override def toString =
    (if (prefix.isEmpty) "" else prefix.get.toString+"#") +
    (if (runtimeClass.isArray) "Array" else runtimeClass.getName) +
    argString
}
