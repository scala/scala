package scala
package reflect

import java.lang.{ Class => jClass }

/**
 *
 * A `ClassTag[T]` stores the erased class of a given type `T`, accessible via the `runtimeClass`
 * field. This is particularly useful for instantiating `Array`s whose element types are unknown
 * at compile time.
 *
 * `ClassTag`s are a weaker special case of [[scala.reflect.api.TypeTags#TypeTag]]s, in that they
 * wrap only the runtime class of a given type, whereas a `TypeTag` contains all static type
 * information. That is, `ClassTag`s are constructed from knowing only the top-level class of a
 * type, without necessarily knowing all of its argument types. This runtime information is enough
 * for runtime `Array` creation.
 *
 * For example:
 * {{{
 *   scala> def mkArray[T : ClassTag](elems: T*) = Array[T](elems: _*)
 *   mkArray: [T](elems: T*)(implicit evidence$1: scala.reflect.ClassTag[T])Array[T]
 *
 *   scala> mkArray(42, 13)
 *   res0: Array[Int] = Array(42, 13)
 *
 *   scala> mkArray("Japan","Brazil","Germany")
 *   res1: Array[String] = Array(Japan, Brazil, Germany)
 * }}}
 *
 * See [[scala.reflect.api.TypeTags]] for more examples, or the
 * [[http://docs.scala-lang.org/overviews/reflection/typetags-manifests.html Reflection Guide: TypeTags]]
 * for more details.
 *
 */
@scala.annotation.implicitNotFound(msg = "No ClassTag available for ${T}")
trait ClassTag[T] extends Equals with Serializable {
  // please, don't add any APIs here, like it was with `newWrappedArray` and `newArrayBuilder`
  // class tags, and all tags in general, should be as minimalistic as possible

  /** A class representing the type `U` to which `T` would be erased.
   *  Note that there is no subtyping relationship between `T` and `U`.
   */
  def runtimeClass: jClass[_]

  protected def arrayClass[T](tp: jClass[_]): jClass[Array[T]] =
    java.lang.reflect.Array.newInstance(tp, 0).getClass.asInstanceOf[jClass[Array[T]]]

  /** Produces a `ClassTag` that knows how to instantiate an `Array[Array[T]]` */
  def wrap: ClassTag[Array[T]] = ClassTag[Array[T]](arrayClass(runtimeClass))

  /** Produces a new array with element type `T` and length `len` */
  def newArray(len: Int): Array[T]

  /** A ClassTag[T] can serve as an extractor that matches only objects of type T.
   *
   * The compiler tries to turn unchecked type tests in pattern matches into checked ones
   * by wrapping a `(_: T)` type pattern as `ct(_: T)`, where `ct` is the `ClassTag[T]` instance.
   * Type tests necessary before calling other extractors are treated similarly.
   * `SomeExtractor(...)` is turned into `ct(SomeExtractor(...))` if `T` in `SomeExtractor.unapply(x: T)`
   * is uncheckable, but we have an instance of `ClassTag[T]`.
   */
  def unapply(x: Any): Option[T] =
    if (runtimeClass.isInstance(x)) Some(x.asInstanceOf[T])
    else None

  // case class accessories
  override def canEqual(x: Any) = x.isInstanceOf[ClassTag[_]]
  override def equals(x: Any) = x.isInstanceOf[ClassTag[_]] && this.runtimeClass == x.asInstanceOf[ClassTag[_]].runtimeClass
  override def hashCode = runtimeClass.##
  override def toString = {
    def prettyprint(clazz: jClass[_]): String =
      if (clazz.isArray) s"Array[${prettyprint(clazz.getComponentType)}]" else
      clazz.getName
    prettyprint(runtimeClass)
  }
}

@SerialVersionUID(1L)
abstract class SingletonClassTag[T](override val toString: String,
  val runtimeClass: jClass[_]) extends ClassTag[T] {
  override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]
  @transient
  override val hashCode = System.identityHashCode(this)
}

/**
 * Class tags corresponding to primitive types and constructor/extractor for ClassTags.
 */
object ClassTag {
  @SerialVersionUID(1L)
  private class ByteClassTag extends SingletonClassTag[scala.Byte]("Byte", java.lang.Byte.TYPE) {
    override def newArray(len: Int): Array[Byte] = new Array[Byte](len)
    override def unapply(x: Any): Option[Byte] = {
      x match {
        case d: Byte => Some(d)
        case _ => None
      }
    }
    private def readResolve(): Any = ClassTag.Byte
  }
  val Byte: ClassTag[Byte] = new ByteClassTag

  @SerialVersionUID(1L)
  private class ShortClassTag extends SingletonClassTag[scala.Short]("Short", java.lang.Short.TYPE) {
    override def newArray(len: Int): Array[Short] = new Array[Short](len)
    override def unapply(x: Any): Option[Short] = {
      x match {
        case d: Short => Some(d)
        case _ => None
      }
    }
    private def readResolve(): Any = ClassTag.Short
  }
  val Short: ClassTag[Short] = new ShortClassTag

  @SerialVersionUID(1L)
  private class CharClassTag extends SingletonClassTag[scala.Char]("Char", java.lang.Character.TYPE) {
    override def newArray(len: Int): Array[Char] = new Array[Char](len)
    override def unapply(x: Any): Option[Char] = {
      x match {
        case d: Char => Some(d)
        case _ => None
      }
    }
    private def readResolve(): Any = ClassTag.Char
  }
  val Char: ClassTag[Char] = new CharClassTag

  @SerialVersionUID(1L)
  private class IntClassTag extends SingletonClassTag[scala.Int]("Int", java.lang.Integer.TYPE) {
    override def newArray(len: Int): Array[Int] = new Array[Int](len)
    override def unapply(x: Any): Option[Int] = {
      x match {
        case d: Int => Some(d)
        case _ => None
      }
    }
    private def readResolve(): Any = ClassTag.Int
  }
  val Int: ClassTag[Int] = new IntClassTag

  @SerialVersionUID(1L)
  private class LongClassTag extends SingletonClassTag[scala.Long]("Long", java.lang.Long.TYPE) {
    override def newArray(len: Int): Array[Long] = new Array[Long](len)
    override def unapply(x: Any): Option[Long] = {
      x match {
        case d: Long => Some(d)
        case _ => None
      }
    }
    private def readResolve(): Any = ClassTag.Long
  }
  val Long: ClassTag[Long] = new LongClassTag

  @SerialVersionUID(1L)
  private class FloatClassTag extends SingletonClassTag[scala.Float]("Float", java.lang.Float.TYPE) {
    override def newArray(len: Int): Array[Float] = new Array[Float](len)
    override def unapply(x: Any): Option[Float] = {
      x match {
        case d: Float => Some(d)
        case _ => None
      }
    }
    private def readResolve(): Any = ClassTag.Float
  }
  val Float: ClassTag[Float] = new FloatClassTag

  @SerialVersionUID(1L)
  private class DoubleClassTag extends SingletonClassTag[scala.Double]("Double", java.lang.Double.TYPE) {
    override def newArray(len: Int): Array[Double] = new Array[Double](len)
    override def unapply(x: Any): Option[Double] = {
      x match {
        case d: Double => Some(d)
        case _ => None
      }
    }
    private def readResolve(): Any = ClassTag.Double
  }
  val Double: ClassTag[Double] = new DoubleClassTag

  @SerialVersionUID(1L)
  private class BooleanClassTag extends SingletonClassTag[scala.Boolean]("Boolean", java.lang.Boolean.TYPE) {
    override def newArray(len: Int): Array[Boolean] = new Array[Boolean](len)
    override def unapply(x: Any): Option[Boolean] = {
      x match {
        case d: Boolean => Some(d)
        case _ => None
      }
    }
    private def readResolve(): Any = ClassTag.Boolean
  }
  val Boolean: ClassTag[Boolean] = new BooleanClassTag

  @SerialVersionUID(1L)
  private class UnitClassTag extends SingletonClassTag[scala.Unit]("Unit", java.lang.Void.TYPE) {
    override def newArray(len: Int): Array[Unit] = new Array[Unit](len)
    override protected def arrayClass[T](tp: Class[_]): Class[Array[T]] =
      if (tp eq runtimeClass) classOf[Array[scala.runtime.BoxedUnit]].asInstanceOf[Class[Array[T]]]
      else super.arrayClass(tp)
    override def unapply(x: Any): Option[Unit] = {
      x match {
        case d: Unit => Some(d)
        case _ => None
      }
    }
    private def readResolve(): Any = ClassTag.Unit
  }
  val Unit: ClassTag[Unit] = new UnitClassTag

  private[this] val ObjectTYPE = classOf[java.lang.Object]
  private[this] val NothingTYPE = classOf[scala.runtime.Nothing$]
  private[this] val NullTYPE = classOf[scala.runtime.Null$]

  @SerialVersionUID(1L)
  private class AnyClassTag extends SingletonClassTag[scala.Any]("Any", ObjectTYPE) {
    override def newArray(len: Int) = new Array[scala.Any](len)
    private def readResolve(): Any = ClassTag.Any
  }
  val Any: ClassTag[scala.Any] = new AnyClassTag

  @SerialVersionUID(1L)
  private class ObjectClassTag extends SingletonClassTag[java.lang.Object]("Object", ObjectTYPE) {
    override def newArray(len: Int) = new Array[java.lang.Object](len)
    private def readResolve(): Any = ClassTag.Object
  }
  val Object: ClassTag[java.lang.Object] = new ObjectClassTag

  val AnyRef: ClassTag[scala.AnyRef] = Object.asInstanceOf[ClassTag[scala.AnyRef]]

  @SerialVersionUID(1L)
  private class AnyValClassTag extends SingletonClassTag[scala.AnyVal]("AnyVal", ObjectTYPE) {
    override def newArray(len: Int) = new Array[scala.AnyVal](len)
    private def readResolve(): Any = ClassTag.AnyVal
  }
  val AnyVal: ClassTag[scala.AnyVal] = new AnyValClassTag

  @SerialVersionUID(1L)
  private class NullClassTag extends SingletonClassTag[scala.Null]("Null", NullTYPE) {
    override def newArray(len: Int) = new Array[scala.Null](len)
    private def readResolve(): Any = ClassTag.Null
  }
  val Null: ClassTag[scala.Null] = new NullClassTag

  @SerialVersionUID(1L)
  private class NothingClassTag extends SingletonClassTag[scala.Nothing]("Nothing", NothingTYPE) {
    override def newArray(len: Int) = new Array[scala.Nothing](len)
    private def readResolve(): Any = ClassTag.Nothing
  }
  val Nothing: ClassTag[scala.Nothing] = new NothingClassTag

  @SerialVersionUID(1L)
  private class GenericClassTag[T](val runtimeClass: jClass[_]) extends ClassTag[T] {
    override def newArray(len: Int): Array[T] = {
      java.lang.reflect.Array.newInstance(runtimeClass, len).asInstanceOf[Array[T]]
    }
  }

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
      case _                        => new GenericClassTag[T](runtimeClass1)
    }

  def unapply[T](ctag: ClassTag[T]): Option[Class[_]] = Some(ctag.runtimeClass)
}
