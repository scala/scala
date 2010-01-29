/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.reflect

import scala.runtime._
import scala.collection.immutable.{List, Nil}
import scala.collection.mutable.{WrappedArray, ArrayBuilder}

/** <p>
  *   A <code>ClassManifest[T]</code> is an opaque descriptor for type <code>T</code>.
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
trait ClassManifest[T] extends OptManifest[T] {

  /** A class representing the type U to which T would be erased. Note
    * that there is no subtyping relationship between T and U. */
  def erasure: Predef.Class[_]

  /** Tests whether the type represented by this manifest is a subtype of
    * the type represented by `that' manifest. BE AWARE: the current
    * implementation is an approximation, as the test is done on the
    * erasure of the type. */
  def <:<(that: ClassManifest[_]): Boolean = {
    def subtype(sub: Predef.Class[_], sup: Predef.Class[_]): Boolean = {
      val subSuperClass = sub.getSuperclass
      val subSuperInterfaces = sub.getInterfaces.toList
      val subSuper =
        (if (subSuperClass == null) Nil else List(subSuperClass)) ::: subSuperInterfaces
      (subSuper contains sup) || (subSuper exists (subtype(_, sup)))
    }
    def subargs(args1: List[OptManifest[_]], args2: List[OptManifest[_]]): Boolean = {
      (args1 zip args2) forall {
        case (x: ClassManifest[_], y: ClassManifest[_]) => x <:< y // !!! [Martin] this is wrong, need to take variance into account
        case (NoManifest, NoManifest) => true
        case _ => false
      }
    }

    import Manifest.{ AnyVal, Nothing, Null }

    that match {
      // All types which conform to AnyVal will override <:<.
      case _: AnyValManifest[_]     => false
      // Anything which conforms to a bottom type will override <:<.
      case AnyVal | Nothing | Null  => false
      case _  =>
        (this.erasure == that.erasure || subtype(this.erasure, that.erasure)) &&
        subargs(this.typeArguments, that.typeArguments)
    }
  }

  /** Tests whether the type represented by this manifest is a supertype
    * of the type represented by `that' manifest. BE AWARE: the current
    * implementation is an approximation, as the test is done on the
    * erasure of the type. */
  def >:>(that: ClassManifest[_]): Boolean =
    that <:< this

  /** Tests whether the type represented by this manifest is equal to the
    * type represented by `that' manifest. BE AWARE: the current
    * implementation is an approximation, as the test is done on the
    * erasure of the type. */
  override def equals(that: Any): Boolean = that match {
    case _: AnyValManifest[_] => false
    case m: ClassManifest[_]  => this.erasure == m.erasure
    case _ => false
  }
  override def hashCode = this.erasure.hashCode

  protected def arrayClass[T](tp: Predef.Class[_]): Predef.Class[Array[T]] =
    java.lang.reflect.Array.newInstance(tp, 0).getClass.asInstanceOf[Predef.Class[Array[T]]]

  def arrayManifest: ClassManifest[Array[T]] =
    ClassManifest.classType[Array[T]](arrayClass[T](erasure))

  def newArray(len: Int): Array[T] =
    java.lang.reflect.Array.newInstance(erasure, len).asInstanceOf[Array[T]]

  def newArray2(len: Int): Array[Array[T]] =
    java.lang.reflect.Array.newInstance(arrayClass[T](erasure), len)
      .asInstanceOf[Array[Array[T]]]

  def newArray3(len: Int): Array[Array[Array[T]]] =
    java.lang.reflect.Array.newInstance(arrayClass[Array[T]](arrayClass[T](erasure)), len)
      .asInstanceOf[Array[Array[Array[T]]]]

  def newArray4(len: Int): Array[Array[Array[Array[T]]]] =
    java.lang.reflect.Array.newInstance(arrayClass[Array[Array[T]]](arrayClass[Array[T]](arrayClass[T](erasure))), len)
      .asInstanceOf[Array[Array[Array[Array[T]]]]]

  def newArray5(len: Int): Array[Array[Array[Array[Array[T]]]]] =
    java.lang.reflect.Array.newInstance(arrayClass[Array[Array[Array[T]]]](arrayClass[Array[Array[T]]](arrayClass[Array[T]](arrayClass[T](erasure)))), len)
      .asInstanceOf[Array[Array[Array[Array[Array[T]]]]]]

  def newWrappedArray(len: Int): WrappedArray[T] =
    // it's safe to assume T <: AnyRef here because the method is overridden for all value type manifests
    new WrappedArray.ofRef[T with AnyRef](newArray(len).asInstanceOf[Array[T with AnyRef]]).asInstanceOf[WrappedArray[T]]

  def newArrayBuilder(): ArrayBuilder[T] =
    // it's safe to assume T <: AnyRef here because the method is overridden for all value type manifests
    new ArrayBuilder.ofRef[T with AnyRef]()(this.asInstanceOf[ClassManifest[T with AnyRef]]).asInstanceOf[ArrayBuilder[T]]

  def typeArguments: List[OptManifest[_]] = List()

  protected def argString =
    if (typeArguments.nonEmpty) typeArguments.mkString("[", ", ", "]")
    else if (erasure.isArray) "["+ClassManifest.fromClass(erasure.getComponentType)+"]"
    else ""
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
object ClassManifest {

  val Byte = Manifest.Byte
  val Short = Manifest.Short
  val Char = Manifest.Char
  val Int = Manifest.Int
  val Long = Manifest.Long
  val Float = Manifest.Float
  val Double = Manifest.Double
  val Boolean = Manifest.Boolean
  val Unit = Manifest.Unit
  val Any = Manifest.Any
  val Object = Manifest.Object
  val AnyVal = Manifest.AnyVal
  val Nothing = Manifest.Nothing
  val Null = Manifest.Null

  def fromClass[T](clazz: Predef.Class[T]): ClassManifest[T] = clazz match {
    case java.lang.Byte.TYPE => Byte.asInstanceOf[ClassManifest[T]]
    case java.lang.Short.TYPE => Short.asInstanceOf[ClassManifest[T]]
    case java.lang.Character.TYPE => Char.asInstanceOf[ClassManifest[T]]
    case java.lang.Integer.TYPE => Int.asInstanceOf[ClassManifest[T]]
    case java.lang.Long.TYPE => Long.asInstanceOf[ClassManifest[T]]
    case java.lang.Float.TYPE => Float.asInstanceOf[ClassManifest[T]]
    case java.lang.Double.TYPE => Double.asInstanceOf[ClassManifest[T]]
    case java.lang.Boolean.TYPE => Boolean.asInstanceOf[ClassManifest[T]]
    case java.lang.Void.TYPE => Unit.asInstanceOf[ClassManifest[T]]
    case _ => classType[T with AnyRef](clazz).asInstanceOf[ClassManifest[T]]
  }

  def singleType[T <: AnyRef](value: AnyRef): Manifest[T] = Manifest.singleType(value)

  /** ClassManifest for the class type `clazz', where `clazz' is
    * a top-level or static class.
    * @note This no-prefix, no-arguments case is separate because we
    *       it's called from ScalaRunTime.boxArray itself. If we
    *       pass varargs as arrays into this, we get an infinitely recursive call
    *       to boxArray. (Besides, having a separate case is more efficient)
    */
  def classType[T <: AnyRef](clazz: Predef.Class[_]): ClassManifest[T] =
    new ClassTypeManifest[T](None, clazz, Nil)

  /** ClassManifest for the class type `clazz[args]', where `clazz' is
    * a top-level or static class and `args` are its type arguments */
  def classType[T <: AnyRef](clazz: Predef.Class[_], arg1: OptManifest[_], args: OptManifest[_]*): ClassManifest[T] =
    new ClassTypeManifest[T](None, clazz, arg1 :: args.toList)

  /** ClassManifest for the class type `clazz[args]', where `clazz' is
    * a class with non-package prefix type `prefix` and type arguments `args`.
    */
  def classType[T <: AnyRef](prefix: OptManifest[_], clazz: Predef.Class[_], args: OptManifest[_]*): ClassManifest[T] =
    new ClassTypeManifest[T](Some(prefix), clazz, args.toList)

  /** Manifest for the class type `clazz[args]', where `clazz' is
    * a top-level or static class. */
  @serializable
  private class ClassTypeManifest[T <: AnyRef](prefix: Option[OptManifest[_]],
                                               val erasure: Predef.Class[_],
                                               override val typeArguments: List[OptManifest[_]]) extends ClassManifest[T] {
    override def toString =
      (if (prefix.isEmpty) "" else prefix.get.toString+"#") +
      (if (erasure.isArray) "Array" else erasure.getName) +
      argString
   }

  def arrayType[T](arg: OptManifest[_]): ClassManifest[Array[T]] = arg match {
    case NoManifest => Object.asInstanceOf[ClassManifest[Array[T]]]
    case m: ClassManifest[_] => m.asInstanceOf[ClassManifest[T]].arrayManifest
  }

  /** ClassManifest for the abstract type `prefix # name'. `upperBound' is not
    * strictly necessary as it could be obtained by reflection. It was
    * added so that erasure can be calculated without reflection. */
  def abstractType[T](prefix: OptManifest[_], name: String, clazz: Predef.Class[_], args: OptManifest[_]*): ClassManifest[T] =
    new (ClassManifest[T] @serializable) {
      def erasure = clazz
      override val typeArguments = args.toList
      override def toString = prefix.toString+"#"+name+argString
    }

  /** ClassManifest for the abstract type `prefix # name'. `upperBound' is not
    * strictly necessary as it could be obtained by reflection. It was
    * added so that erasure can be calculated without reflection.
    * todo: remove after next boostrap
    */
  def abstractType[T](prefix: OptManifest[_], name: String, upperbound: ClassManifest[_], args: OptManifest[_]*): ClassManifest[T] =
    new (ClassManifest[T] @serializable) {
      def erasure = upperbound.erasure
      override val typeArguments = args.toList
      override def toString = prefix.toString+"#"+name+argString
    }

  /** ClassManifest for the intersection type `parents_0 with ... with parents_n'. */
  def intersectionType[T](parents: ClassManifest[_]*): ClassManifest[T] =
    new (ClassManifest[T] @serializable) {
      def erasure = parents.head.erasure
      override def toString = parents.mkString(" with ")
    }
}
