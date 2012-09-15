package scala.reflect

import java.lang.{ Class => jClass }
import scala.language.{implicitConversions, existentials}
import scala.runtime.ScalaRunTime.{ arrayClass, arrayElementClass }

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
  def unapply(x: Any): Option[T] = unapply_impl(x)
  def unapply(x: Byte): Option[T] = unapply_impl(x)
  def unapply(x: Short): Option[T] = unapply_impl(x)
  def unapply(x: Char): Option[T] = unapply_impl(x)
  def unapply(x: Int): Option[T] = unapply_impl(x)
  def unapply(x: Long): Option[T] = unapply_impl(x)
  def unapply(x: Float): Option[T] = unapply_impl(x)
  def unapply(x: Double): Option[T] = unapply_impl(x)
  def unapply(x: Boolean): Option[T] = unapply_impl(x)
  def unapply(x: Unit): Option[T] = unapply_impl(x)

  private def unapply_impl[U: ClassTag](x: U): Option[T] =
    if (x == null) None
    else {
      val staticClass = classTag[U].runtimeClass
      val dynamicClass = x.getClass
      val effectiveClass = if (staticClass.isPrimitive) staticClass else dynamicClass
      val conforms = runtimeClass.isAssignableFrom(effectiveClass)
      if (conforms) Some(x.asInstanceOf[T]) else None
    }

  /** case class accessories */
  override def canEqual(x: Any) = x.isInstanceOf[ClassTag[_]]
  override def equals(x: Any) = x.isInstanceOf[ClassTag[_]] && this.runtimeClass == x.asInstanceOf[ClassTag[_]].runtimeClass
  override def hashCode = scala.runtime.ScalaRunTime.hash(runtimeClass)
  override def toString = {
    def prettyprint(clazz: jClass[_]): String =
      if (clazz.isArray) s"Array[${prettyprint(arrayElementClass(clazz))}]" else
      clazz.getName
    prettyprint(runtimeClass)
  }
}

object ClassTag {
  private val ObjectTYPE = classOf[java.lang.Object]
  private val NothingTYPE = classOf[scala.runtime.Nothing$]
  private val NullTYPE = classOf[scala.runtime.Null$]

  val Byte    : ClassTag[scala.Byte]       = Manifest.Byte
  val Short   : ClassTag[scala.Short]      = Manifest.Short
  val Char    : ClassTag[scala.Char]       = Manifest.Char
  val Int     : ClassTag[scala.Int]        = Manifest.Int
  val Long    : ClassTag[scala.Long]       = Manifest.Long
  val Float   : ClassTag[scala.Float]      = Manifest.Float
  val Double  : ClassTag[scala.Double]     = Manifest.Double
  val Boolean : ClassTag[scala.Boolean]    = Manifest.Boolean
  val Unit    : ClassTag[scala.Unit]       = Manifest.Unit
  val Any     : ClassTag[scala.Any]        = Manifest.Any
  val Object  : ClassTag[java.lang.Object] = Manifest.Object
  val AnyVal  : ClassTag[scala.AnyVal]     = Manifest.AnyVal
  val AnyRef  : ClassTag[scala.AnyRef]     = Manifest.AnyRef
  val Nothing : ClassTag[scala.Nothing]    = Manifest.Nothing
  val Null    : ClassTag[scala.Null]       = Manifest.Null

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
      case NothingTYPE              => ClassTag.Nothing.asInstanceOf[ClassTag[T]]
      case NullTYPE                 => ClassTag.Null.asInstanceOf[ClassTag[T]]
      case _                        => new ClassTag[T]{ def runtimeClass = runtimeClass1 }
    }

  def unapply[T](ctag: ClassTag[T]): Option[Class[_]] = Some(ctag.runtimeClass)
}
