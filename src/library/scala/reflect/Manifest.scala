/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package reflect

import scala.annotation.{implicitNotFound, nowarn}
import scala.collection.mutable.{ArrayBuilder, ArraySeq}

/** A `Manifest[T]` is an opaque descriptor for type T.  Its supported use
 *  is to give access to the erasure of the type as a `Class` instance, as
 *  is necessary for the creation of native `Arrays` if the class is not
 *  known at compile time.
 *
 *  The type-relation operators `<:<` and `=:=` should be considered
 *  approximations only, as there are numerous aspects of type conformance
 *  which are not yet adequately represented in manifests.
 *
 *  Example usages:
 *  {{{
 *    def arr[T] = new Array[T](0)                          // does not compile
 *    def arr[T](implicit m: Manifest[T]) = new Array[T](0) // compiles
 *    def arr[T: Manifest] = new Array[T](0)                // shorthand for the preceding
 *
 *    // Methods manifest and optManifest are in [[scala.Predef]].
 *    def isApproxSubType[T: Manifest, U: Manifest] = manifest[T] <:< manifest[U]
 *    isApproxSubType[List[String], List[AnyRef]] // true
 *    isApproxSubType[List[String], List[Int]]    // false
 *
 *    def methods[T: Manifest] = manifest[T].runtimeClass.getMethods
 *    def retType[T: Manifest](name: String) =
 *      methods[T] find (_.getName == name) map (_.getGenericReturnType)
 *
 *    retType[Map[_, _]]("values")  // Some(scala.collection.Iterable<B>)
 *  }}}
 */
@nowarn("""cat=deprecation&origin=scala\.reflect\.ClassManifest(DeprecatedApis.*)?""")
@implicitNotFound(msg = "No Manifest available for ${T}.")
// TODO undeprecated until Scala reflection becomes non-experimental
// @deprecated("use scala.reflect.ClassTag (to capture erasures) or scala.reflect.runtime.universe.TypeTag (to capture types) or both instead", "2.10.0")
trait Manifest[T] extends ClassManifest[T] with Equals {
  override def typeArguments: List[Manifest[_]] = Nil

  override def arrayManifest: Manifest[Array[T]] =
    Manifest.classType[Array[T]](arrayClass[T](runtimeClass), this)

  override def canEqual(that: Any): Boolean = that match {
    case _: Manifest[_]   => true
    case _                => false
  }
  /** Note: testing for erasure here is important, as it is many times
   *  faster than <:< and rules out most comparisons.
   */
  override def equals(that: Any): Boolean = that match {
    case m: Manifest[_] => (m canEqual this) && (this.runtimeClass == m.runtimeClass) && (this <:< m) && (m <:< this)
    case _              => false
  }
  override def hashCode = this.runtimeClass.##
}

/** The object `Manifest` defines factory methods for manifests.
 *  It is intended for use by the compiler and should not be used in client code.
 */
// TODO undeprecated until Scala reflection becomes non-experimental
// @deprecated("use scala.reflect.ClassTag (to capture erasures), scala.reflect.runtime.universe.TypeTag (to capture types) or both instead", "2.10.0")
object Manifest {
  /* Forward all the public members of ManifestFactory, since this object used
   * to be a `private val Manifest = ManifestFactory` in the package object. It
   * was moved here because it needs to be in the same file as `trait Manifest`
   * defined above.
   */

  def valueManifests: List[AnyValManifest[_]] =
    ManifestFactory.valueManifests

  val Byte: ManifestFactory.ByteManifest = ManifestFactory.Byte
  val Short: ManifestFactory.ShortManifest = ManifestFactory.Short
  val Char: ManifestFactory.CharManifest = ManifestFactory.Char
  val Int: ManifestFactory.IntManifest = ManifestFactory.Int
  val Long: ManifestFactory.LongManifest = ManifestFactory.Long
  val Float: ManifestFactory.FloatManifest = ManifestFactory.Float
  val Double: ManifestFactory.DoubleManifest = ManifestFactory.Double
  val Boolean: ManifestFactory.BooleanManifest = ManifestFactory.Boolean
  val Unit: ManifestFactory.UnitManifest = ManifestFactory.Unit

  val Any: Manifest[scala.Any] = ManifestFactory.Any
  val Object: Manifest[java.lang.Object] = ManifestFactory.Object
  val AnyRef: Manifest[scala.AnyRef] = ManifestFactory.AnyRef
  val AnyVal: Manifest[scala.AnyVal] = ManifestFactory.AnyVal
  val Null: Manifest[scala.Null] = ManifestFactory.Null
  val Nothing: Manifest[scala.Nothing] = ManifestFactory.Nothing

  /** Manifest for the singleton type `value.type`. */
  def singleType[T <: AnyRef](value: AnyRef): Manifest[T] =
    ManifestFactory.singleType[T](value)

  /** Manifest for the class type `clazz[args]`, where `clazz` is
    * a top-level or static class.
    * @note This no-prefix, no-arguments case is separate because we
    *       it's called from ScalaRunTime.boxArray itself. If we
    *       pass varargs as arrays into this, we get an infinitely recursive call
    *       to boxArray. (Besides, having a separate case is more efficient)
    */
  def classType[T](clazz: Predef.Class[_]): Manifest[T] =
    ManifestFactory.classType[T](clazz)

  /** Manifest for the class type `clazz`, where `clazz` is
    * a top-level or static class and args are its type arguments. */
  def classType[T](clazz: Predef.Class[T], arg1: Manifest[_], args: Manifest[_]*): Manifest[T] =
    ManifestFactory.classType[T](clazz, arg1, args: _*)

  /** Manifest for the class type `clazz[args]`, where `clazz` is
    * a class with non-package prefix type `prefix` and type arguments `args`.
    */
  def classType[T](prefix: Manifest[_], clazz: Predef.Class[_], args: Manifest[_]*): Manifest[T] =
    ManifestFactory.classType[T](prefix, clazz, args: _*)

  def arrayType[T](arg: Manifest[_]): Manifest[Array[T]] =
    ManifestFactory.arrayType[T](arg)

  /** Manifest for the abstract type `prefix # name`. `upperBound` is not
    * strictly necessary as it could be obtained by reflection. It was
    * added so that erasure can be calculated without reflection. */
  def abstractType[T](prefix: Manifest[_], name: String, upperBound: Predef.Class[_], args: Manifest[_]*): Manifest[T] =
    ManifestFactory.abstractType[T](prefix, name, upperBound, args: _*)

  /** Manifest for the unknown type `_ >: L <: U` in an existential. */
  def wildcardType[T](lowerBound: Manifest[_], upperBound: Manifest[_]): Manifest[T] =
    ManifestFactory.wildcardType[T](lowerBound, upperBound)

  /** Manifest for the intersection type `parents_0 with ... with parents_n`. */
  def intersectionType[T](parents: Manifest[_]*): Manifest[T] =
    ManifestFactory.intersectionType[T](parents: _*)

}

// TODO undeprecated until Scala reflection becomes non-experimental
// @deprecated("use type tags and manually check the corresponding class or type instead", "2.10.0")
@nowarn("""cat=deprecation&origin=scala\.reflect\.ClassManifest(DeprecatedApis.*)?""")
@SerialVersionUID(1L)
abstract class AnyValManifest[T <: AnyVal](override val toString: String) extends Manifest[T] with Equals {
  override def <:<(that: ClassManifest[_]): Boolean =
    (that eq this) || (that eq Manifest.Any) || (that eq Manifest.AnyVal)
  override def canEqual(other: Any) = other match {
    case _: AnyValManifest[_] => true
    case _                    => false
  }
  override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]
  @transient
  override val hashCode = System.identityHashCode(this)
}

/** `ManifestFactory` defines factory methods for manifests.
 *  It is intended for use by the compiler and should not be used in client code.
 *
 *  Unlike `Manifest`, this factory isn't annotated with a deprecation warning.
 *  This is done to prevent avalanches of deprecation warnings in the code that calls methods with manifests.
 *  Why so complicated? Read up the comments for `ClassManifestFactory`.
 */
@nowarn("""cat=deprecation&origin=scala\.reflect\.ClassManifest(DeprecatedApis.*)?""")
object ManifestFactory {
  def valueManifests: List[AnyValManifest[_]] =
    List(Byte, Short, Char, Int, Long, Float, Double, Boolean, Unit)

  @SerialVersionUID(1L)
  final private[reflect] class ByteManifest extends AnyValManifest[scala.Byte]("Byte") {
    def runtimeClass: Class[java.lang.Byte] = java.lang.Byte.TYPE
    @inline override def newArray(len: Int): Array[Byte] = new Array[Byte](len)
    override def newWrappedArray(len: Int): ArraySeq[Byte] = new ArraySeq.ofByte(new Array[Byte](len))
    override def newArrayBuilder(): ArrayBuilder[Byte] = new ArrayBuilder.ofByte()
    override def unapply(x: Any): Option[Byte] = {
      x match {
        case d: Byte => Some(d)
        case _ => None
      }
    }
    private def readResolve(): Any = Manifest.Byte
  }
  val Byte: ByteManifest = new ByteManifest

  @SerialVersionUID(1L)
  final private[reflect] class ShortManifest extends AnyValManifest[scala.Short]("Short") {
    def runtimeClass: Class[java.lang.Short] = java.lang.Short.TYPE
    @inline override def newArray(len: Int): Array[Short] = new Array[Short](len)
    override def newWrappedArray(len: Int): ArraySeq[Short] = new ArraySeq.ofShort(new Array[Short](len))
    override def newArrayBuilder(): ArrayBuilder[Short] = new ArrayBuilder.ofShort()
    override def unapply(x: Any): Option[Short] = {
      x match {
        case d: Short => Some(d)
        case _ => None
      }
    }
    private def readResolve(): Any = Manifest.Short
  }
  val Short: ShortManifest = new ShortManifest

  @SerialVersionUID(1L)
  final private[reflect] class CharManifest extends AnyValManifest[scala.Char]("Char") {
    def runtimeClass: Class[java.lang.Character] = java.lang.Character.TYPE
    @inline override def newArray(len: Int): Array[Char] = new Array[Char](len)
    override def newWrappedArray(len: Int): ArraySeq[Char] = new ArraySeq.ofChar(new Array[Char](len))
    override def newArrayBuilder(): ArrayBuilder[Char] = new ArrayBuilder.ofChar()
    override def unapply(x: Any): Option[Char] = {
      x match {
        case d: Char => Some(d)
        case _ => None
      }
    }
    private def readResolve(): Any = Manifest.Char
  }
  val Char: CharManifest = new CharManifest

  @SerialVersionUID(1L)
  final private[reflect] class IntManifest extends AnyValManifest[scala.Int]("Int") {
    def runtimeClass: Class[java.lang.Integer] = java.lang.Integer.TYPE
    @inline override def newArray(len: Int): Array[Int] = new Array[Int](len)
    override def newWrappedArray(len: Int): ArraySeq[Int] = new ArraySeq.ofInt(new Array[Int](len))
    override def newArrayBuilder(): ArrayBuilder[Int] = new ArrayBuilder.ofInt()
    override def unapply(x: Any): Option[Int] = {
      x match {
        case d: Int => Some(d)
        case _ => None
      }
    }
    private def readResolve(): Any = Manifest.Int
  }
  val Int: IntManifest = new IntManifest

  @SerialVersionUID(1L)
  final private[reflect] class LongManifest extends AnyValManifest[scala.Long]("Long") {
    def runtimeClass: Class[java.lang.Long] = java.lang.Long.TYPE
    @inline override def newArray(len: Int): Array[Long] = new Array[Long](len)
    override def newWrappedArray(len: Int): ArraySeq[Long] = new ArraySeq.ofLong(new Array[Long](len))
    override def newArrayBuilder(): ArrayBuilder[Long] = new ArrayBuilder.ofLong()
    override def unapply(x: Any): Option[Long] = {
      x match {
        case d: Long => Some(d)
        case _ => None
      }
    }
    private def readResolve(): Any = Manifest.Long
  }
  val Long: LongManifest = new LongManifest

  @SerialVersionUID(1L)
  final private[reflect] class FloatManifest extends AnyValManifest[scala.Float]("Float") {
    def runtimeClass: Class[java.lang.Float] = java.lang.Float.TYPE
    @inline override def newArray(len: Int): Array[Float] = new Array[Float](len)
    override def newWrappedArray(len: Int): ArraySeq[Float] = new ArraySeq.ofFloat(new Array[Float](len))
    override def newArrayBuilder(): ArrayBuilder[Float] = new ArrayBuilder.ofFloat()
    override def unapply(x: Any): Option[Float] = {
      x match {
        case d: Float => Some(d)
        case _ => None
      }
    }
    private def readResolve(): Any = Manifest.Float
  }
  val Float: FloatManifest = new FloatManifest

  @SerialVersionUID(1L)
  final private[reflect] class DoubleManifest extends AnyValManifest[scala.Double]("Double") {
    def runtimeClass: Class[java.lang.Double] = java.lang.Double.TYPE
    @inline override def newArray(len: Int): Array[Double] = new Array[Double](len)
    override def newWrappedArray(len: Int): ArraySeq[Double] = new ArraySeq.ofDouble(new Array[Double](len))
    override def newArrayBuilder(): ArrayBuilder[Double] = new ArrayBuilder.ofDouble()

    override def unapply(x: Any): Option[Double] = {
      x match {
        case d: Double => Some(d)
        case _ => None
      }
    }
    private def readResolve(): Any = Manifest.Double
  }
  val Double: DoubleManifest = new DoubleManifest

  @SerialVersionUID(1L)
  final private[reflect] class BooleanManifest extends AnyValManifest[scala.Boolean]("Boolean") {
    def runtimeClass: Class[java.lang.Boolean] = java.lang.Boolean.TYPE
    @inline override def newArray(len: Int): Array[Boolean] = new Array[Boolean](len)
    override def newWrappedArray(len: Int): ArraySeq[Boolean] = new ArraySeq.ofBoolean(new Array[Boolean](len))
    override def newArrayBuilder(): ArrayBuilder[Boolean] = new ArrayBuilder.ofBoolean()
    override def unapply(x: Any): Option[Boolean] = {
      x match {
        case d: Boolean => Some(d)
        case _ => None
      }
    }
    private def readResolve(): Any = Manifest.Boolean
  }
  val Boolean: BooleanManifest = new BooleanManifest

  @SerialVersionUID(1L)
  final private[reflect] class UnitManifest extends AnyValManifest[scala.Unit]("Unit") {
    def runtimeClass: Class[java.lang.Void] = java.lang.Void.TYPE
    @inline override def newArray(len: Int): Array[Unit] = new Array[Unit](len)
    override def newWrappedArray(len: Int): ArraySeq[Unit] = new ArraySeq.ofUnit(new Array[Unit](len))
    override def newArrayBuilder(): ArrayBuilder[Unit] = new ArrayBuilder.ofUnit()
    override protected def arrayClass[T](tp: Class[_]): Class[Array[T]] =
      if (tp eq runtimeClass) classOf[Array[scala.runtime.BoxedUnit]].asInstanceOf[Class[Array[T]]]
      else super.arrayClass(tp)
    override def unapply(x: Any): Option[Unit] = {
      x match {
        case d: Unit => Some(d)
        case _ => None
      }
    }
    private def readResolve(): Any = Manifest.Unit
  }
  val Unit: UnitManifest = new UnitManifest

  private[this] val ObjectTYPE = classOf[java.lang.Object]
  private[this] val NothingTYPE = classOf[scala.runtime.Nothing$]
  private[this] val NullTYPE = classOf[scala.runtime.Null$]

  @SerialVersionUID(1L)
  final private class AnyManifest extends PhantomManifest[scala.Any](ObjectTYPE, "Any") {
    override def newArray(len: Int) = new Array[scala.Any](len)
    override def <:<(that: ClassManifest[_]): Boolean = (that eq this)
    private def readResolve(): Any = Manifest.Any
  }
  val Any: Manifest[scala.Any] = new AnyManifest

  @SerialVersionUID(1L)
  final private class ObjectManifest extends PhantomManifest[java.lang.Object](ObjectTYPE, "Object") {
    override def newArray(len: Int) = new Array[java.lang.Object](len)
    override def <:<(that: ClassManifest[_]): Boolean = (that eq this) || (that eq Any)
    private def readResolve(): Any = Manifest.Object
  }
  val Object: Manifest[java.lang.Object] = new ObjectManifest

  val AnyRef: Manifest[scala.AnyRef] = Object.asInstanceOf[Manifest[scala.AnyRef]]

  @SerialVersionUID(1L)
  final private class AnyValPhantomManifest extends PhantomManifest[scala.AnyVal](ObjectTYPE, "AnyVal") {
    override def newArray(len: Int) = new Array[scala.AnyVal](len)
    override def <:<(that: ClassManifest[_]): Boolean = (that eq this) || (that eq Any)
    private def readResolve(): Any = Manifest.AnyVal
  }
  val AnyVal: Manifest[scala.AnyVal] = new AnyValPhantomManifest

  @SerialVersionUID(1L)
  final private class NullManifest extends PhantomManifest[scala.Null](NullTYPE, "Null") {
    override def newArray(len: Int) = new Array[scala.Null](len)
    override def <:<(that: ClassManifest[_]): Boolean =
      (that ne null) && (that ne Nothing) && !(that <:< AnyVal)
    private def readResolve(): Any = Manifest.Null
  }
  val Null: Manifest[scala.Null] = new NullManifest

  @SerialVersionUID(1L)
  final private class NothingManifest extends PhantomManifest[scala.Nothing](NothingTYPE, "Nothing") {
    override def newArray(len: Int) = new Array[scala.Nothing](len)
    override def <:<(that: ClassManifest[_]): Boolean = (that ne null)
    private def readResolve(): Any = Manifest.Nothing
  }
  val Nothing: Manifest[scala.Nothing] = new NothingManifest

  @SerialVersionUID(1L)
  final private class SingletonTypeManifest[T <: AnyRef](value: AnyRef) extends Manifest[T] {
    lazy val runtimeClass: Class[_ <: AnyRef] = value.getClass
    override lazy val toString = value.toString + ".type"
  }

  /** Manifest for the singleton type `value.type`. */
  def singleType[T <: AnyRef](value: AnyRef): Manifest[T] =
    new SingletonTypeManifest[T](value)

  /** Manifest for the class type `clazz[args]`, where `clazz` is
    * a top-level or static class.
    * @note This no-prefix, no-arguments case is separate because we
    *       it's called from ScalaRunTime.boxArray itself. If we
    *       pass varargs as arrays into this, we get an infinitely recursive call
    *       to boxArray. (Besides, having a separate case is more efficient)
    */
  def classType[T](clazz: Predef.Class[_]): Manifest[T] =
    new ClassTypeManifest[T](None, clazz, Nil)

  /** Manifest for the class type `clazz`, where `clazz` is
    * a top-level or static class and args are its type arguments. */
  def classType[T](clazz: Predef.Class[T], arg1: Manifest[_], args: Manifest[_]*): Manifest[T] =
    new ClassTypeManifest[T](None, clazz, arg1 :: args.toList)

  /** Manifest for the class type `clazz[args]`, where `clazz` is
    * a class with non-package prefix type `prefix` and type arguments `args`.
    */
  def classType[T](prefix: Manifest[_], clazz: Predef.Class[_], args: Manifest[_]*): Manifest[T] =
    new ClassTypeManifest[T](Some(prefix), clazz, args.toList)

  @SerialVersionUID(1L)
  private abstract class PhantomManifest[T](_runtimeClass: Predef.Class[_],
                                            override val toString: String) extends ClassTypeManifest[T](None, _runtimeClass, Nil) {
    override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]
    @transient
    override val hashCode = System.identityHashCode(this)
  }

  /** Manifest for the class type `clazz[args]`, where `clazz` is
    * a top-level or static class. */
  @SerialVersionUID(1L)
  private class ClassTypeManifest[T](prefix: Option[Manifest[_]],
                                     val runtimeClass: Predef.Class[_],
                                     override val typeArguments: List[Manifest[_]]) extends Manifest[T] {
    override def toString =
      (if (prefix.isEmpty) "" else prefix.get.toString+"#") +
      (if (runtimeClass.isArray) "Array" else runtimeClass.getName) +
      argString
   }

  def arrayType[T](arg: Manifest[_]): Manifest[Array[T]] =
    arg.asInstanceOf[Manifest[T]].arrayManifest

  @SerialVersionUID(1L)
  private class AbstractTypeManifest[T](prefix: Manifest[_], name: String, upperBound: Predef.Class[_], args: scala.collection.Seq[Manifest[_]]) extends Manifest[T] {
    def runtimeClass = upperBound
    override val typeArguments = args.toList
    override def toString = prefix.toString+"#"+name+argString
  }

  /** Manifest for the abstract type `prefix # name`. `upperBound` is not
    * strictly necessary as it could be obtained by reflection. It was
    * added so that erasure can be calculated without reflection. */
  def abstractType[T](prefix: Manifest[_], name: String, upperBound: Predef.Class[_], args: Manifest[_]*): Manifest[T] =
    new AbstractTypeManifest[T](prefix, name, upperBound, args)

  @SerialVersionUID(1L)
  private class WildcardManifest[T](lowerBound: Manifest[_], upperBound: Manifest[_]) extends Manifest[T] {
    def runtimeClass = upperBound.runtimeClass
    override def toString =
      "_" +
        (if (lowerBound eq Nothing) "" else " >: "+lowerBound) +
        (if (upperBound eq Nothing) "" else " <: "+upperBound)
  }

  /** Manifest for the unknown type `_ >: L <: U` in an existential.
    */
  def wildcardType[T](lowerBound: Manifest[_], upperBound: Manifest[_]): Manifest[T] =
    new WildcardManifest[T](lowerBound, upperBound)

  @SerialVersionUID(1L)
  private class IntersectionTypeManifest[T](parents: Array[Manifest[_]]) extends Manifest[T] {
    // We use an `Array` instead of a `Seq` for `parents` to avoid cyclic dependencies during deserialization
    // which can cause serialization proxies to leak and cause a ClassCastException.
    def runtimeClass = parents(0).runtimeClass
    override def toString = parents.mkString(" with ")
  }

  /** Manifest for the intersection type `parents_0 with ... with parents_n`. */
  def intersectionType[T](parents: Manifest[_]*): Manifest[T] =
    new IntersectionTypeManifest[T](parents.toArray)
}
