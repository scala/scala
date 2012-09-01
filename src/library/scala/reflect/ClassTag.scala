package scala.reflect

import java.lang.{ Class => jClass }
import language.{implicitConversions, existentials}
import scala.runtime.ScalaRunTime.arrayClass

/** A `ClassTag[T]` wraps a runtime class, which can be accessed via the `runtimeClass` method.
 *
 *  This is useful in itself, but also enables very important use case.
 *  Having this knowledge ClassTag can instantiate `Arrays`
 *  in those cases where the element type is unknown at compile time.
 *
 *  If an implicit value of type u.ClassTag[T] is required, the compiler will make one up on demand.
 *  The implicitly created value contains in its `runtimeClass` field the runtime class that is the result of erasing type T.
 *  In that value, any occurrences of type parameters or abstract types U which come themselves with a ClassTag
 *  are represented by the type referenced by that tag.
 *  If the type T contains unresolved references to type parameters or abstract types, a static error results.
 *
 * @see [[scala.reflect.base.TypeTags]]
 */
@annotation.implicitNotFound(msg = "No ClassTag available for ${T}")
trait ClassTag[T] extends ClassManifestDeprecatedApis[T] with Equals with Serializable {
  // please, don't add any APIs here, like it was with `newWrappedArray` and `newArrayBuilder`
  // class tags, and all tags in general, should be as minimalistic as possible

  /** A class representing the type `U` to which `T` would be erased.
   *  Note that there is no subtyping relationship between `T` and `U`.
   */
  def runtimeClass: jClass[_]

  /** Produces a `ClassTag` that knows how to build `Array[Array[T]]` */
  def wrap: ClassTag[Array[T]] = ClassTag[Array[T]](arrayClass(runtimeClass))

  /** Produces a new array with element type `T` and length `len` */
  override def newArray(len: Int): Array[T] =
    runtimeClass match {
      case java.lang.Byte.TYPE      => new Array[Byte](len).asInstanceOf[Array[T]]
      case java.lang.Short.TYPE     => new Array[Short](len).asInstanceOf[Array[T]]
      case java.lang.Character.TYPE => new Array[Char](len).asInstanceOf[Array[T]]
      case java.lang.Integer.TYPE   => new Array[Int](len).asInstanceOf[Array[T]]
      case java.lang.Long.TYPE      => new Array[Long](len).asInstanceOf[Array[T]]
      case java.lang.Float.TYPE     => new Array[Float](len).asInstanceOf[Array[T]]
      case java.lang.Double.TYPE    => new Array[Double](len).asInstanceOf[Array[T]]
      case java.lang.Boolean.TYPE   => new Array[Boolean](len).asInstanceOf[Array[T]]
      case java.lang.Void.TYPE      => new Array[Unit](len).asInstanceOf[Array[T]]
      case _                        => java.lang.reflect.Array.newInstance(runtimeClass, len).asInstanceOf[Array[T]]
    }

  /** A ClassTag[T] can serve as an extractor that matches only objects of type T.
   *
   * The compiler tries to turn unchecked type tests in pattern matches into checked ones
   * by wrapping a `(_: T)` type pattern as `ct(_: T)`, where `ct` is the `ClassTag[T]` instance.
   * Type tests necessary before calling other extractors are treated similarly.
   * `SomeExtractor(...)` is turned into `ct(SomeExtractor(...))` if `T` in `SomeExtractor.unapply(x: T)`
   * is uncheckable, but we have an instance of `ClassTag[T]`.
   */
  def unapply(x: Any): Option[T] = if (x != null && runtimeClass.isAssignableFrom(x.getClass)) Some(x.asInstanceOf[T]) else None

  /** case class accessories */
  override def canEqual(x: Any) = x.isInstanceOf[ClassTag[_]]
  override def equals(x: Any) = x.isInstanceOf[ClassTag[_]] && this.runtimeClass == x.asInstanceOf[ClassTag[_]].runtimeClass
  override def hashCode = scala.runtime.ScalaRunTime.hash(runtimeClass)
  override def toString = "ClassTag[" + runtimeClass + "]"
}

object ClassTag {
  private val NothingTYPE = classOf[scala.runtime.Nothing$]
  private val NullTYPE = classOf[scala.runtime.Null$]
  private val ObjectTYPE = classOf[java.lang.Object]

  val Byte    : ClassTag[scala.Byte]       = new ClassTag[scala.Byte]{ def runtimeClass = java.lang.Byte.TYPE; private def readResolve() = ClassTag.Byte }
  val Short   : ClassTag[scala.Short]      = new ClassTag[scala.Short]{ def runtimeClass = java.lang.Short.TYPE; private def readResolve() = ClassTag.Short }
  val Char    : ClassTag[scala.Char]       = new ClassTag[scala.Char]{ def runtimeClass = java.lang.Character.TYPE; private def readResolve() = ClassTag.Char }
  val Int     : ClassTag[scala.Int]        = new ClassTag[scala.Int]{ def runtimeClass = java.lang.Integer.TYPE; private def readResolve() = ClassTag.Int }
  val Long    : ClassTag[scala.Long]       = new ClassTag[scala.Long]{ def runtimeClass = java.lang.Long.TYPE; private def readResolve() = ClassTag.Long }
  val Float   : ClassTag[scala.Float]      = new ClassTag[scala.Float]{ def runtimeClass = java.lang.Float.TYPE; private def readResolve() = ClassTag.Float }
  val Double  : ClassTag[scala.Double]     = new ClassTag[scala.Double]{ def runtimeClass = java.lang.Double.TYPE; private def readResolve() = ClassTag.Double }
  val Boolean : ClassTag[scala.Boolean]    = new ClassTag[scala.Boolean]{ def runtimeClass = java.lang.Boolean.TYPE; private def readResolve() = ClassTag.Boolean }
  val Unit    : ClassTag[scala.Unit]       = new ClassTag[scala.Unit]{ def runtimeClass = java.lang.Void.TYPE; private def readResolve() = ClassTag.Unit }
  val Any     : ClassTag[scala.Any]        = new ClassTag[scala.Any]{ def runtimeClass = ObjectTYPE; private def readResolve() = ClassTag.Any }
  val Object  : ClassTag[java.lang.Object] = new ClassTag[java.lang.Object]{ def runtimeClass = ObjectTYPE; private def readResolve() = ClassTag.Object }
  val AnyVal  : ClassTag[scala.AnyVal]     = ClassTag.Object.asInstanceOf[ClassTag[scala.AnyVal]]
  val AnyRef  : ClassTag[scala.AnyRef]     = ClassTag.Object.asInstanceOf[ClassTag[scala.AnyRef]]
  val Nothing : ClassTag[scala.Nothing]    = new ClassTag[scala.Nothing]{ def runtimeClass = NothingTYPE; private def readResolve() = ClassTag.Nothing }
  val Null    : ClassTag[scala.Null]       = new ClassTag[scala.Null]{ def runtimeClass = NullTYPE; private def readResolve() = ClassTag.Null }

  def apply[T](runtimeClass1: jClass[_]): ClassTag[T] =
    runtimeClass1 match {
      case java.lang.Byte.TYPE      => ClassTag.Byte.asInstanceOf[ClassTag[T]]
      case java.lang.Short.TYPE     => ClassTag.Short.asInstanceOf[ClassTag[T]]
      case java.lang.Character.TYPE => ClassTag.Char.asInstanceOf[ClassTag[T]]
      case java.lang.Integer.TYPE   => ClassTag.Int.asInstanceOf[ClassTag[T]]
      case java.lang.Long.TYPE      => ClassTag.Long.asInstanceOf[ClassTag[T]]
      case java.lang.Float.TYPE     => ClassTag.Float.asInstanceOf[ClassTag[T]]
      case java.lang.Double.TYPE    => ClassTag.Double.asInstanceOf[ClassTag[T]]
      case java.lang.Boolean.TYPE   => ClassTag.Boolean.asInstanceOf[ClassTag[T]]
      case java.lang.Void.TYPE      => ClassTag.Unit.asInstanceOf[ClassTag[T]]
      case ObjectTYPE               => ClassTag.Object.asInstanceOf[ClassTag[T]]
      case _                        => new ClassTag[T]{ def runtimeClass = runtimeClass1 }
    }

  def unapply[T](ctag: ClassTag[T]): Option[Class[_]] = Some(ctag.runtimeClass)
}