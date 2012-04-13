package scala.reflect

import java.lang.{ Class => jClass }
import scala.reflect.{ mirror => rm }

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
 *  (this is not possible to do in all reflection universes because an operation that converts a type to a Java class might not be available). */
// please, don't add any APIs here, like it was with `newWrappedArray` and `newArrayBuilder`
// class tags, and all tags in general, should be as minimalistic as possible
@annotation.implicitNotFound(msg = "No ClassTag available for ${T}")
abstract case class ClassTag[T](erasure: jClass[_]) extends ArrayTag[T] {
  // quick and dirty fix to a deadlock in Predef:
  // http://groups.google.com/group/scala-internals/browse_thread/thread/977de028a4e75d6f
  // todo. fix that in a sane way
  // assert(erasure != null)

  /** A Scala reflection type representing T.
   *  For ClassTags this representation is lossy (in their case tpe is retrospectively constructed from erasure).
   *  For TypeTags and ConcreteTypeTags the representation is almost precise, because they use reification
   *  (information is lost only when T refers to non-locatable symbols, which are then reified as free variables). */
  def tpe: rm.Type = rm.classToType(erasure)

  /** A Scala reflection symbol representing T. */
  def symbol: rm.Symbol = rm.classToSymbol(erasure)

  /** Produces a `ClassTag` that knows how to build `Array[Array[T]]` */
  def wrap: ClassTag[Array[T]] = {
    val arrayClazz = java.lang.reflect.Array.newInstance(erasure, 0).getClass.asInstanceOf[jClass[Array[T]]]
    ClassTag[Array[T]](arrayClazz)
  }

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
}

object ClassTag {
  private val ObjectTYPE = classOf[java.lang.Object]
  private val StringTYPE = classOf[java.lang.String]

  val Byte    : ClassTag[scala.Byte]       = new ClassTag[scala.Byte](java.lang.Byte.TYPE) { private def readResolve() = ClassTag.Byte }
  val Short   : ClassTag[scala.Short]      = new ClassTag[scala.Short](java.lang.Short.TYPE) { private def readResolve() = ClassTag.Short }
  val Char    : ClassTag[scala.Char]       = new ClassTag[scala.Char](java.lang.Character.TYPE) { private def readResolve() = ClassTag.Char }
  val Int     : ClassTag[scala.Int]        = new ClassTag[scala.Int](java.lang.Integer.TYPE) { private def readResolve() = ClassTag.Int }
  val Long    : ClassTag[scala.Long]       = new ClassTag[scala.Long](java.lang.Long.TYPE) { private def readResolve() = ClassTag.Long }
  val Float   : ClassTag[scala.Float]      = new ClassTag[scala.Float](java.lang.Float.TYPE) { private def readResolve() = ClassTag.Float }
  val Double  : ClassTag[scala.Double]     = new ClassTag[scala.Double](java.lang.Double.TYPE) { private def readResolve() = ClassTag.Double }
  val Boolean : ClassTag[scala.Boolean]    = new ClassTag[scala.Boolean](java.lang.Boolean.TYPE) { private def readResolve() = ClassTag.Boolean }
  val Unit    : ClassTag[scala.Unit]       = new ClassTag[scala.Unit](java.lang.Void.TYPE) { private def readResolve() = ClassTag.Unit }
  val Any     : ClassTag[scala.Any]        = new ClassTag[scala.Any](ObjectTYPE) { private def readResolve() = ClassTag.Any }
  val Object  : ClassTag[java.lang.Object] = new ClassTag[java.lang.Object](ObjectTYPE) { private def readResolve() = ClassTag.Object }
  val AnyVal  : ClassTag[scala.AnyVal]     = new ClassTag[scala.AnyVal](ObjectTYPE) { private def readResolve() = ClassTag.AnyVal }
  val AnyRef  : ClassTag[scala.AnyRef]     = new ClassTag[scala.AnyRef](ObjectTYPE) { private def readResolve() = ClassTag.AnyRef }
  val Nothing : ClassTag[scala.Nothing]    = new ClassTag[scala.Nothing](ObjectTYPE) { private def readResolve() = ClassTag.Nothing }
  val Null    : ClassTag[scala.Null]       = new ClassTag[scala.Null](ObjectTYPE) { private def readResolve() = ClassTag.Null }
  val String  : ClassTag[java.lang.String] = new ClassTag[java.lang.String](StringTYPE) { private def readResolve() = ClassTag.String }

  def apply[T](clazz: jClass[_]): ClassTag[T] =
    clazz match {
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
      case _                        => new ClassTag[T](clazz) {}
    }

  def apply[T](tpe: rm.Type): ClassTag[T] =
    tpe match {
      case rm.ByteTpe    => ClassTag.Byte.asInstanceOf[ClassTag[T]]
      case rm.ShortTpe   => ClassTag.Short.asInstanceOf[ClassTag[T]]
      case rm.CharTpe    => ClassTag.Char.asInstanceOf[ClassTag[T]]
      case rm.IntTpe     => ClassTag.Int.asInstanceOf[ClassTag[T]]
      case rm.LongTpe    => ClassTag.Long.asInstanceOf[ClassTag[T]]
      case rm.FloatTpe   => ClassTag.Float.asInstanceOf[ClassTag[T]]
      case rm.DoubleTpe  => ClassTag.Double.asInstanceOf[ClassTag[T]]
      case rm.BooleanTpe => ClassTag.Boolean.asInstanceOf[ClassTag[T]]
      case rm.UnitTpe    => ClassTag.Unit.asInstanceOf[ClassTag[T]]
      case rm.AnyTpe     => ClassTag.Any.asInstanceOf[ClassTag[T]]
      case rm.ObjectTpe  => ClassTag.Object.asInstanceOf[ClassTag[T]]
      case rm.AnyValTpe  => ClassTag.AnyVal.asInstanceOf[ClassTag[T]]
      case rm.AnyRefTpe  => ClassTag.AnyRef.asInstanceOf[ClassTag[T]]
      case rm.NothingTpe => ClassTag.Nothing.asInstanceOf[ClassTag[T]]
      case rm.NullTpe    => ClassTag.Null.asInstanceOf[ClassTag[T]]
      case rm.StringTpe  => ClassTag.String.asInstanceOf[ClassTag[T]]
      case _             => apply[T](rm.typeToClass(tpe.erasure))
    }

  implicit def toDeprecatedClassManifestApis[T](ctag: ClassTag[T]): DeprecatedClassManifestApis[T] = new DeprecatedClassManifestApis[T](ctag)
}

// this class should not be used directly in client code
class DeprecatedClassManifestApis[T](ctag: ClassTag[T]) {
  import scala.collection.mutable.{ WrappedArray, ArrayBuilder }

  @deprecated("Use `tpe` to analyze the underlying type", "2.10.0")
  def <:<(that: ClassManifest[_]): Boolean = ctag.tpe <:< that.tpe

  @deprecated("Use `tpe` to analyze the underlying type", "2.10.0")
  def >:>(that: ClassManifest[_]): Boolean = that <:< ctag

  @deprecated("Use `wrap` instead", "2.10.0")
  def arrayManifest: ClassManifest[Array[T]] = ctag.wrap

  @deprecated("Use a combination of `wrap` and `newArray` instead", "2.10.0")
  def newArray2(len: Int): Array[Array[T]] = ctag.wrap.newArray(len)

  @deprecated("Use a combination of `wrap` and `newArray` instead", "2.10.0")
  def newArray3(len: Int): Array[Array[Array[T]]] = ctag.wrap.wrap.newArray(len)

  @deprecated("Use a combination of `wrap` and `newArray` instead", "2.10.0")
  def newArray4(len: Int): Array[Array[Array[Array[T]]]] = ctag.wrap.wrap.wrap.newArray(len)

  @deprecated("Use a combination of `wrap` and `newArray` instead", "2.10.0")
  def newArray5(len: Int): Array[Array[Array[Array[Array[T]]]]] = ctag.wrap.wrap.wrap.wrap.newArray(len)

  @deprecated("Use `@scala.collection.mutable.WrappedArray` object instead", "2.10.0")
  def newWrappedArray(len: Int): WrappedArray[T] =
    ctag.erasure match {
      case java.lang.Byte.TYPE      => new WrappedArray.ofByte(new Array[Byte](len)).asInstanceOf[WrappedArray[T]]
      case java.lang.Short.TYPE     => new WrappedArray.ofShort(new Array[Short](len)).asInstanceOf[WrappedArray[T]]
      case java.lang.Character.TYPE => new WrappedArray.ofChar(new Array[Char](len)).asInstanceOf[WrappedArray[T]]
      case java.lang.Integer.TYPE   => new WrappedArray.ofInt(new Array[Int](len)).asInstanceOf[WrappedArray[T]]
      case java.lang.Long.TYPE      => new WrappedArray.ofLong(new Array[Long](len)).asInstanceOf[WrappedArray[T]]
      case java.lang.Float.TYPE     => new WrappedArray.ofFloat(new Array[Float](len)).asInstanceOf[WrappedArray[T]]
      case java.lang.Double.TYPE    => new WrappedArray.ofDouble(new Array[Double](len)).asInstanceOf[WrappedArray[T]]
      case java.lang.Boolean.TYPE   => new WrappedArray.ofBoolean(new Array[Boolean](len)).asInstanceOf[WrappedArray[T]]
      case java.lang.Void.TYPE      => new WrappedArray.ofUnit(new Array[Unit](len)).asInstanceOf[WrappedArray[T]]
      case _                        => new WrappedArray.ofRef[T with AnyRef](ctag.newArray(len).asInstanceOf[Array[T with AnyRef]]).asInstanceOf[WrappedArray[T]]
    }

  @deprecated("Use `@scala.collection.mutable.ArrayBuilder` object instead", "2.10.0")
  def newArrayBuilder(): ArrayBuilder[T] = ArrayBuilder.make[T]()(ctag)

  @deprecated("`typeArguments` is no longer supported, and will always return an empty list. Use `@scala.reflect.TypeTag` or `@scala.reflect.ConcreteTypeTag` to capture and analyze type arguments", "2.10.0")
  def typeArguments: List[OptManifest[_]] = List()
}