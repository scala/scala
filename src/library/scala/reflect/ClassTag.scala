package scala.reflect

import java.lang.{ Class => jClass }
import scala.reflect.{ mirror => rm }
import language.{implicitConversions, existentials}
import scala.runtime.ScalaRunTime.arrayClass

/** A `ClassTag[T]` wraps a Java class, which can be accessed via the `erasure` method.
 *
 *  This is useful in itself, but also enables very important use case.
 *  Having this knowledge ClassTag can instantiate `Arrays`
 *  in those cases where the element type is unknown at compile time.
 *  Hence, ClassTag[T] conforms to the ArrayTag[T] trait.
 *
 *  If an implicit value of type u.ClassTag[T] is required, the compiler will make one up on demand.
 *  The implicitly created value contains in its erasure field the Java class that is the result of erasing type T.
 *  In that value, any occurrences of type parameters or abstract types U which come themselves with a ClassTag
 *  or a reflect.mirror.ConcreteTypeTag are represented by the type referenced by that tag.
 *  If the type T contains unresolved references to type parameters or abstract types, a static error results.
 *
 *  A ConcreteTypeTag member of the reflect.mirror object is convertible to a ClassTag via an implicit conversion
 *  (this is not possible to do in all reflection universes because an operation that converts a type to a Java class might not be available).
 *
 * @see [[scala.reflect.api.TypeTags]]
 */
@annotation.implicitNotFound(msg = "No ClassTag available for ${T}")
trait ClassTag[T] extends ArrayTag[T] with ErasureTag[T] with Equals with Serializable {
  // please, don't add any APIs here, like it was with `newWrappedArray` and `newArrayBuilder`
  // class tags, and all tags in general, should be as minimalistic as possible

  /** Produces a `ClassTag` that knows how to build `Array[Array[T]]` */
  def wrap: ClassTag[Array[T]] = ClassTag[Array[T]](arrayClass(erasure))

  /** Produces a new array with element type `T` and length `len` */
  def newArray(len: Int): Array[T] =
    erasure match {
      case java.lang.Byte.TYPE      => new Array[Byte](len).asInstanceOf[Array[T]]
      case java.lang.Short.TYPE     => new Array[Short](len).asInstanceOf[Array[T]]
      case java.lang.Character.TYPE => new Array[Char](len).asInstanceOf[Array[T]]
      case java.lang.Integer.TYPE   => new Array[Int](len).asInstanceOf[Array[T]]
      case java.lang.Long.TYPE      => new Array[Long](len).asInstanceOf[Array[T]]
      case java.lang.Float.TYPE     => new Array[Float](len).asInstanceOf[Array[T]]
      case java.lang.Double.TYPE    => new Array[Double](len).asInstanceOf[Array[T]]
      case java.lang.Boolean.TYPE   => new Array[Boolean](len).asInstanceOf[Array[T]]
      case java.lang.Void.TYPE      => new Array[Unit](len).asInstanceOf[Array[T]]
      case _                        => java.lang.reflect.Array.newInstance(erasure, len).asInstanceOf[Array[T]]
    }

  /** case class accessories */
  override def canEqual(x: Any) = x.isInstanceOf[ClassTag[_]]
  override def equals(x: Any) = x.isInstanceOf[ClassTag[_]] && this.erasure == x.asInstanceOf[ClassTag[_]].erasure
  override def hashCode = scala.runtime.ScalaRunTime.hash(erasure)
  override def toString = "ClassTag[" + erasure + "]"
}

object ClassTag {
  private val NothingTYPE = classOf[scala.runtime.Nothing$]
  private val NullTYPE = classOf[scala.runtime.Null$]
  private val ObjectTYPE = classOf[java.lang.Object]
  private val StringTYPE = classOf[java.lang.String]

  val Byte    : ClassTag[scala.Byte]       = new ClassTag[scala.Byte]{ def erasure = java.lang.Byte.TYPE; private def readResolve() = ClassTag.Byte }
  val Short   : ClassTag[scala.Short]      = new ClassTag[scala.Short]{ def erasure = java.lang.Short.TYPE; private def readResolve() = ClassTag.Short }
  val Char    : ClassTag[scala.Char]       = new ClassTag[scala.Char]{ def erasure = java.lang.Character.TYPE; private def readResolve() = ClassTag.Char }
  val Int     : ClassTag[scala.Int]        = new ClassTag[scala.Int]{ def erasure = java.lang.Integer.TYPE; private def readResolve() = ClassTag.Int }
  val Long    : ClassTag[scala.Long]       = new ClassTag[scala.Long]{ def erasure = java.lang.Long.TYPE; private def readResolve() = ClassTag.Long }
  val Float   : ClassTag[scala.Float]      = new ClassTag[scala.Float]{ def erasure = java.lang.Float.TYPE; private def readResolve() = ClassTag.Float }
  val Double  : ClassTag[scala.Double]     = new ClassTag[scala.Double]{ def erasure = java.lang.Double.TYPE; private def readResolve() = ClassTag.Double }
  val Boolean : ClassTag[scala.Boolean]    = new ClassTag[scala.Boolean]{ def erasure = java.lang.Boolean.TYPE; private def readResolve() = ClassTag.Boolean }
  val Unit    : ClassTag[scala.Unit]       = new ClassTag[scala.Unit]{ def erasure = java.lang.Void.TYPE; private def readResolve() = ClassTag.Unit }
  val Any     : ClassTag[scala.Any]        = new ClassTag[scala.Any]{ def erasure = ObjectTYPE; private def readResolve() = ClassTag.Any }
  val Object  : ClassTag[java.lang.Object] = new ClassTag[java.lang.Object]{ def erasure = ObjectTYPE; private def readResolve() = ClassTag.Object }
  val AnyVal  : ClassTag[scala.AnyVal]     = new ClassTag[scala.AnyVal]{ def erasure = ObjectTYPE; private def readResolve() = ClassTag.AnyVal }
  val AnyRef  : ClassTag[scala.AnyRef]     = new ClassTag[scala.AnyRef]{ def erasure = ObjectTYPE; private def readResolve() = ClassTag.AnyRef }
  val Nothing : ClassTag[scala.Nothing]    = new ClassTag[scala.Nothing]{ def erasure = NothingTYPE; private def readResolve() = ClassTag.Nothing }
  val Null    : ClassTag[scala.Null]       = new ClassTag[scala.Null]{ def erasure = NullTYPE; private def readResolve() = ClassTag.Null }
  val String  : ClassTag[java.lang.String] = new ClassTag[java.lang.String]{ def erasure = StringTYPE; private def readResolve() = ClassTag.String }

  def apply[T](erasure1: jClass[_]): ClassTag[T] =
    erasure1 match {
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
      case StringTYPE               => ClassTag.String.asInstanceOf[ClassTag[T]]
      case _                        => new ClassTag[T]{ def erasure = erasure1 }
    }

  def unapply[T](ctag: ClassTag[T]): Option[Class[_]] = Some(ctag.erasure)
}