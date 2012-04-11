/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect
package api

import scala.reflect.{ mirror => rm }

/**
 * Type tags encapsulate a representation of type T.
 * They are supposed to replace the pre-2.10 concept of a [[scala.reflect.Manifest]].
 * TypeTags are much better integrated with reflection than manifests are, and are consequently much simpler.
 *
 * Type tags are organized in a hierarchy of two classes:
 * [[scala.reflect.ClassTag]], [[scala.reflect.api.Universe#TypeTag]] and [[scala.reflect.api.Universe#GroundTypeTag]].
 * A [[scala.reflect.ClassTag]] value wraps a Java class, which can be accessed via the erasure method.
 * A [[scala.reflect.api.Universe#TypeTag]] value wraps a full Scala type in its tpe field.
 * A [[scala.reflect.api.Universe#GroundTypeTag]] value is a type tag that is guaranteed not to contain any references to type parameters or abstract types.
 *
 * TypeTags correspond loosely to Manifests. More precisely:
 * The previous notion of a [[scala.reflect.ClassManifest]] corresponds to a scala.reflect.ClassTag,
 * The previous notion of a [[scala.reflect.Manifest]] corresponds to scala.reflect.mirror.GroundTypeTag,
 * Whereas scala.reflect.mirror.TypeTag is approximated by the previous notion of [[scala.reflect.OptManifest]].
 *
 * Implicit in the contract for all Tag classes is that the reified type tpe represents the type parameter T.
 * Tags are typically created by the compiler, which makes sure that this contract is kept.
 *
 * An example that illustrates the TypeTag embedding, consider the following function:
 *
 *   import reflect.mirror._
 *     def f[T: TypeTag, U] = {
 *       type L = T => U
 *       implicitly[TypeTag[L]]
 *     }
 *
 * Then a call of f[String, Int] will yield a result of the form
 *
 *   TypeTag(<[ String => U ]>).
 *
 * Note that T has been replaced by String, because it comes with a TypeTag in f, whereas U was left as a type parameter.
 */
trait TypeTags { self: Universe =>

  /**
   * If an implicit value of type u.TypeTag[T] is required, the compiler will make one up on demand.
   * The implicitly created value contains in its tpe field a value of type u.Type that is a reflective representation of T.
   * In that value, any occurrences of type parameters or abstract types U
   * which come themselves with a TypeTag are represented by the type referenced by that TypeTag.
   *
   * @see [[scala.reflect.api.TypeTags]]
   */
  @annotation.implicitNotFound(msg = "No TypeTag available for ${T}")
  abstract case class TypeTag[T](tpe: Type) {
    // it's unsafe to use assert here, because we might run into deadlocks with Predef
    // also see comments in ClassTags.scala
    // assert(tpe != null)

    def sym = tpe.typeSymbol

    def isGround = !isNotGround
    def isNotGround = tpe exists (_.typeSymbol.isAbstractType)

    def toGround: GroundTypeTag[T] = {
      assert(isGround, tpe)
      GroundTypeTag[T](tpe)
    }

    override def toString = {
      var prefix = if (isGround) "GroundTypeTag" else "TypeTag"
      if (prefix != this.productPrefix) prefix = "*" + prefix
      prefix + "[" + tpe + "]"
    }
  }

  object TypeTag {
    val Byte    : TypeTag[scala.Byte]       = GroundTypeTag.Byte
    val Short   : TypeTag[scala.Short]      = GroundTypeTag.Short
    val Char    : TypeTag[scala.Char]       = GroundTypeTag.Char
    val Int     : TypeTag[scala.Int]        = GroundTypeTag.Int
    val Long    : TypeTag[scala.Long]       = GroundTypeTag.Long
    val Float   : TypeTag[scala.Float]      = GroundTypeTag.Float
    val Double  : TypeTag[scala.Double]     = GroundTypeTag.Double
    val Boolean : TypeTag[scala.Boolean]    = GroundTypeTag.Boolean
    val Unit    : TypeTag[scala.Unit]       = GroundTypeTag.Unit
    val Any     : TypeTag[scala.Any]        = GroundTypeTag.Any
    val Object  : TypeTag[java.lang.Object] = GroundTypeTag.Object
    val AnyVal  : TypeTag[scala.AnyVal]     = GroundTypeTag.AnyVal
    val AnyRef  : TypeTag[scala.AnyRef]     = GroundTypeTag.AnyRef
    val Nothing : TypeTag[scala.Nothing]    = GroundTypeTag.Nothing
    val Null    : TypeTag[scala.Null]       = GroundTypeTag.Null
    val String  : TypeTag[java.lang.String] = GroundTypeTag.String

    def apply[T](tpe: Type): TypeTag[T] =
      tpe match {
        case ByteTpe    => TypeTag.Byte.asInstanceOf[TypeTag[T]]
        case ShortTpe   => TypeTag.Short.asInstanceOf[TypeTag[T]]
        case CharTpe    => TypeTag.Char.asInstanceOf[TypeTag[T]]
        case IntTpe     => TypeTag.Int.asInstanceOf[TypeTag[T]]
        case LongTpe    => TypeTag.Long.asInstanceOf[TypeTag[T]]
        case FloatTpe   => TypeTag.Float.asInstanceOf[TypeTag[T]]
        case DoubleTpe  => TypeTag.Double.asInstanceOf[TypeTag[T]]
        case BooleanTpe => TypeTag.Boolean.asInstanceOf[TypeTag[T]]
        case UnitTpe    => TypeTag.Unit.asInstanceOf[TypeTag[T]]
        case AnyTpe     => TypeTag.Any.asInstanceOf[TypeTag[T]]
        case ObjectTpe  => TypeTag.Object.asInstanceOf[TypeTag[T]]
        case AnyValTpe  => TypeTag.AnyVal.asInstanceOf[TypeTag[T]]
        case AnyRefTpe  => TypeTag.AnyRef.asInstanceOf[TypeTag[T]]
        case NothingTpe => TypeTag.Nothing.asInstanceOf[TypeTag[T]]
        case NullTpe    => TypeTag.Null.asInstanceOf[TypeTag[T]]
        case StringTpe  => TypeTag.String.asInstanceOf[TypeTag[T]]
        case _          => new TypeTag[T](tpe) {}
      }
  }

  /**
   * If an implicit value of type u.GroundTypeTag[T] is required, the compiler will make one up on demand following the same procedure as for TypeTags.
   * However, if the resulting type still contains references to type parameters or abstract types, a static error results.
   *
   * @see [[scala.reflect.api.TypeTags]]
   */
  @annotation.implicitNotFound(msg = "No GroundTypeTag available for ${T}")
  class GroundTypeTag[T](tpe: Type) extends TypeTag[T](tpe) {
    assert(isGround, tpe)
    override def productPrefix = "GroundTypeTag"
  }

  object GroundTypeTag {
    val Byte    : GroundTypeTag[scala.Byte]       = new GroundTypeTag[scala.Byte](ByteTpe) { private def readResolve() = GroundTypeTag.Byte }
    val Short   : GroundTypeTag[scala.Short]      = new GroundTypeTag[scala.Short](ShortTpe) { private def readResolve() = GroundTypeTag.Short }
    val Char    : GroundTypeTag[scala.Char]       = new GroundTypeTag[scala.Char](CharTpe) { private def readResolve() = GroundTypeTag.Char }
    val Int     : GroundTypeTag[scala.Int]        = new GroundTypeTag[scala.Int](IntTpe) { private def readResolve() = GroundTypeTag.Int }
    val Long    : GroundTypeTag[scala.Long]       = new GroundTypeTag[scala.Long](LongTpe) { private def readResolve() = GroundTypeTag.Long }
    val Float   : GroundTypeTag[scala.Float]      = new GroundTypeTag[scala.Float](FloatTpe) { private def readResolve() = GroundTypeTag.Float }
    val Double  : GroundTypeTag[scala.Double]     = new GroundTypeTag[scala.Double](DoubleTpe) { private def readResolve() = GroundTypeTag.Double }
    val Boolean : GroundTypeTag[scala.Boolean]    = new GroundTypeTag[scala.Boolean](BooleanTpe) { private def readResolve() = GroundTypeTag.Boolean }
    val Unit    : GroundTypeTag[scala.Unit]       = new GroundTypeTag[scala.Unit](UnitTpe) { private def readResolve() = GroundTypeTag.Unit }
    val Any     : GroundTypeTag[scala.Any]        = new GroundTypeTag[scala.Any](AnyTpe) { private def readResolve() = GroundTypeTag.Any }
    val Object  : GroundTypeTag[java.lang.Object] = new GroundTypeTag[java.lang.Object](ObjectTpe) { private def readResolve() = GroundTypeTag.Object }
    val AnyVal  : GroundTypeTag[scala.AnyVal]     = new GroundTypeTag[scala.AnyVal](AnyValTpe) { private def readResolve() = GroundTypeTag.AnyVal }
    val AnyRef  : GroundTypeTag[scala.AnyRef]     = new GroundTypeTag[scala.AnyRef](AnyRefTpe) { private def readResolve() = GroundTypeTag.AnyRef }
    val Nothing : GroundTypeTag[scala.Nothing]    = new GroundTypeTag[scala.Nothing](NothingTpe) { private def readResolve() = GroundTypeTag.Nothing }
    val Null    : GroundTypeTag[scala.Null]       = new GroundTypeTag[scala.Null](NullTpe) { private def readResolve() = GroundTypeTag.Null }
    val String  : GroundTypeTag[java.lang.String] = new GroundTypeTag[java.lang.String](StringTpe) { private def readResolve() = GroundTypeTag.String }

    def apply[T](tpe: Type): GroundTypeTag[T] =
      tpe match {
        case ByteTpe    => GroundTypeTag.Byte.asInstanceOf[GroundTypeTag[T]]
        case ShortTpe   => GroundTypeTag.Short.asInstanceOf[GroundTypeTag[T]]
        case CharTpe    => GroundTypeTag.Char.asInstanceOf[GroundTypeTag[T]]
        case IntTpe     => GroundTypeTag.Int.asInstanceOf[GroundTypeTag[T]]
        case LongTpe    => GroundTypeTag.Long.asInstanceOf[GroundTypeTag[T]]
        case FloatTpe   => GroundTypeTag.Float.asInstanceOf[GroundTypeTag[T]]
        case DoubleTpe  => GroundTypeTag.Double.asInstanceOf[GroundTypeTag[T]]
        case BooleanTpe => GroundTypeTag.Boolean.asInstanceOf[GroundTypeTag[T]]
        case UnitTpe    => GroundTypeTag.Unit.asInstanceOf[GroundTypeTag[T]]
        case AnyTpe     => GroundTypeTag.Any.asInstanceOf[GroundTypeTag[T]]
        case ObjectTpe  => GroundTypeTag.Object.asInstanceOf[GroundTypeTag[T]]
        case AnyValTpe  => GroundTypeTag.AnyVal.asInstanceOf[GroundTypeTag[T]]
        case AnyRefTpe  => GroundTypeTag.AnyRef.asInstanceOf[GroundTypeTag[T]]
        case NothingTpe => GroundTypeTag.Nothing.asInstanceOf[GroundTypeTag[T]]
        case NullTpe    => GroundTypeTag.Null.asInstanceOf[GroundTypeTag[T]]
        case StringTpe  => GroundTypeTag.String.asInstanceOf[GroundTypeTag[T]]
        case _          => new GroundTypeTag[T](tpe) {}
      }

    def unapply[T](ttag: TypeTag[T]): Option[Type] = if (ttag.isGround) Some(ttag.tpe) else None

    implicit def toClassTag[T](ttag: rm.GroundTypeTag[T]): ClassTag[T] = ClassTag[T](rm.typeToClass(ttag.tpe.erasure))

    implicit def toDeprecatedManifestApis[T](ttag: rm.GroundTypeTag[T]): DeprecatedManifestApis[T] = new DeprecatedManifestApis[T](ttag)

    // this class should not be used directly in client code
    class DeprecatedManifestApis[T](ttag: rm.GroundTypeTag[T]) extends DeprecatedClassManifestApis[T](toClassTag(ttag)) {
      @deprecated("Use `tpe` to analyze the underlying type", "2.10.0")
      def <:<(that: Manifest[_]): Boolean = ttag.tpe <:< that.tpe

      @deprecated("Use `tpe` to analyze the underlying type", "2.10.0")
      def >:>(that: Manifest[_]): Boolean = that <:< ttag

      @deprecated("Use `tpe` to analyze the type arguments", "2.10.0")
      override def typeArguments: List[Manifest[_]] = ttag.tpe.typeArguments map (targ => rm.GroundTypeTag(targ))
    }
  }

  // incantations for summoning
  // moved to Context, since rm.tags have their own incantations in Predef, and these guys are only useful in macros
//  def tag[T](implicit ttag: TypeTag[T]) = ttag
//  def typeTag[T](implicit ttag: TypeTag[T]) = ttag
//  def groundTag[T](implicit gttag: GroundTypeTag[T]) = gttag
//  def groundTypeTag[T](implicit gttag: GroundTypeTag[T]) = gttag
}