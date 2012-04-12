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
 * [[scala.reflect.api.Universe#TypeTag]] and [[scala.reflect.api.Universe#ConcreteTypeTag]].
 * A [[scala.reflect.api.Universe#TypeTag]] value wraps a full Scala type in its tpe field.
 * A [[scala.reflect.api.Universe#ConcreteTypeTag]] value is a type tag that is guaranteed not to contain any references to type parameters or abstract types.
 *
 * It is also possible to capture Java classes by using a different kind of tag.
 * A [[scala.reflect.ClassTag]] value wraps a Java class, which can be accessed via the erasure method.
 *
 * TypeTags correspond loosely to Manifests. More precisely:
 * The previous notion of a [[scala.reflect.ClassManifest]] corresponds to a scala.reflect.ClassTag,
 * The previous notion of a [[scala.reflect.Manifest]] corresponds to scala.reflect.mirror.ConcreteTypeTag,
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

    def isConcrete = !isNotConcrete
    def isNotConcrete = tpe exists (_.typeSymbol.isAbstractType)
    def toConcrete: ConcreteTypeTag[T] = ConcreteTypeTag[T](tpe)

    override def toString = {
      var prefix = if (isConcrete) "ConcreteTypeTag" else "TypeTag"
      if (prefix != this.productPrefix) prefix = "*" + prefix
      prefix + "[" + tpe + "]"
    }
  }

  object TypeTag {
    val Byte    : TypeTag[scala.Byte]       = ConcreteTypeTag.Byte
    val Short   : TypeTag[scala.Short]      = ConcreteTypeTag.Short
    val Char    : TypeTag[scala.Char]       = ConcreteTypeTag.Char
    val Int     : TypeTag[scala.Int]        = ConcreteTypeTag.Int
    val Long    : TypeTag[scala.Long]       = ConcreteTypeTag.Long
    val Float   : TypeTag[scala.Float]      = ConcreteTypeTag.Float
    val Double  : TypeTag[scala.Double]     = ConcreteTypeTag.Double
    val Boolean : TypeTag[scala.Boolean]    = ConcreteTypeTag.Boolean
    val Unit    : TypeTag[scala.Unit]       = ConcreteTypeTag.Unit
    val Any     : TypeTag[scala.Any]        = ConcreteTypeTag.Any
    val Object  : TypeTag[java.lang.Object] = ConcreteTypeTag.Object
    val AnyVal  : TypeTag[scala.AnyVal]     = ConcreteTypeTag.AnyVal
    val AnyRef  : TypeTag[scala.AnyRef]     = ConcreteTypeTag.AnyRef
    val Nothing : TypeTag[scala.Nothing]    = ConcreteTypeTag.Nothing
    val Null    : TypeTag[scala.Null]       = ConcreteTypeTag.Null
    val String  : TypeTag[java.lang.String] = ConcreteTypeTag.String

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
   * If an implicit value of type u.ConcreteTypeTag[T] is required, the compiler will make one up on demand following the same procedure as for TypeTags.
   * However, if the resulting type still contains references to type parameters or abstract types, a static error results.
   *
   * @see [[scala.reflect.api.TypeTags]]
   */
  @annotation.implicitNotFound(msg = "No ConcreteTypeTag available for ${T}")
  class ConcreteTypeTag[T](tpe: Type) extends TypeTag[T](tpe) {
    // it's unsafe to use assert here, because we might run into deadlocks with Predef
    // also see comments in ClassTags.scala
    //assert(isConcrete, tpe)
    if (isNotConcrete) throw new Error("%s (%s) is not concrete and cannot be used to construct a concrete type tag".format(tpe, tpe.kind))
    override def productPrefix = "ConcreteTypeTag"
  }

  object ConcreteTypeTag {
    val Byte    : ConcreteTypeTag[scala.Byte]       = new ConcreteTypeTag[scala.Byte](ByteTpe) { private def readResolve() = ConcreteTypeTag.Byte }
    val Short   : ConcreteTypeTag[scala.Short]      = new ConcreteTypeTag[scala.Short](ShortTpe) { private def readResolve() = ConcreteTypeTag.Short }
    val Char    : ConcreteTypeTag[scala.Char]       = new ConcreteTypeTag[scala.Char](CharTpe) { private def readResolve() = ConcreteTypeTag.Char }
    val Int     : ConcreteTypeTag[scala.Int]        = new ConcreteTypeTag[scala.Int](IntTpe) { private def readResolve() = ConcreteTypeTag.Int }
    val Long    : ConcreteTypeTag[scala.Long]       = new ConcreteTypeTag[scala.Long](LongTpe) { private def readResolve() = ConcreteTypeTag.Long }
    val Float   : ConcreteTypeTag[scala.Float]      = new ConcreteTypeTag[scala.Float](FloatTpe) { private def readResolve() = ConcreteTypeTag.Float }
    val Double  : ConcreteTypeTag[scala.Double]     = new ConcreteTypeTag[scala.Double](DoubleTpe) { private def readResolve() = ConcreteTypeTag.Double }
    val Boolean : ConcreteTypeTag[scala.Boolean]    = new ConcreteTypeTag[scala.Boolean](BooleanTpe) { private def readResolve() = ConcreteTypeTag.Boolean }
    val Unit    : ConcreteTypeTag[scala.Unit]       = new ConcreteTypeTag[scala.Unit](UnitTpe) { private def readResolve() = ConcreteTypeTag.Unit }
    val Any     : ConcreteTypeTag[scala.Any]        = new ConcreteTypeTag[scala.Any](AnyTpe) { private def readResolve() = ConcreteTypeTag.Any }
    val Object  : ConcreteTypeTag[java.lang.Object] = new ConcreteTypeTag[java.lang.Object](ObjectTpe) { private def readResolve() = ConcreteTypeTag.Object }
    val AnyVal  : ConcreteTypeTag[scala.AnyVal]     = new ConcreteTypeTag[scala.AnyVal](AnyValTpe) { private def readResolve() = ConcreteTypeTag.AnyVal }
    val AnyRef  : ConcreteTypeTag[scala.AnyRef]     = new ConcreteTypeTag[scala.AnyRef](AnyRefTpe) { private def readResolve() = ConcreteTypeTag.AnyRef }
    val Nothing : ConcreteTypeTag[scala.Nothing]    = new ConcreteTypeTag[scala.Nothing](NothingTpe) { private def readResolve() = ConcreteTypeTag.Nothing }
    val Null    : ConcreteTypeTag[scala.Null]       = new ConcreteTypeTag[scala.Null](NullTpe) { private def readResolve() = ConcreteTypeTag.Null }
    val String  : ConcreteTypeTag[java.lang.String] = new ConcreteTypeTag[java.lang.String](StringTpe) { private def readResolve() = ConcreteTypeTag.String }

    def apply[T](tpe: Type): ConcreteTypeTag[T] =
      tpe match {
        case ByteTpe    => ConcreteTypeTag.Byte.asInstanceOf[ConcreteTypeTag[T]]
        case ShortTpe   => ConcreteTypeTag.Short.asInstanceOf[ConcreteTypeTag[T]]
        case CharTpe    => ConcreteTypeTag.Char.asInstanceOf[ConcreteTypeTag[T]]
        case IntTpe     => ConcreteTypeTag.Int.asInstanceOf[ConcreteTypeTag[T]]
        case LongTpe    => ConcreteTypeTag.Long.asInstanceOf[ConcreteTypeTag[T]]
        case FloatTpe   => ConcreteTypeTag.Float.asInstanceOf[ConcreteTypeTag[T]]
        case DoubleTpe  => ConcreteTypeTag.Double.asInstanceOf[ConcreteTypeTag[T]]
        case BooleanTpe => ConcreteTypeTag.Boolean.asInstanceOf[ConcreteTypeTag[T]]
        case UnitTpe    => ConcreteTypeTag.Unit.asInstanceOf[ConcreteTypeTag[T]]
        case AnyTpe     => ConcreteTypeTag.Any.asInstanceOf[ConcreteTypeTag[T]]
        case ObjectTpe  => ConcreteTypeTag.Object.asInstanceOf[ConcreteTypeTag[T]]
        case AnyValTpe  => ConcreteTypeTag.AnyVal.asInstanceOf[ConcreteTypeTag[T]]
        case AnyRefTpe  => ConcreteTypeTag.AnyRef.asInstanceOf[ConcreteTypeTag[T]]
        case NothingTpe => ConcreteTypeTag.Nothing.asInstanceOf[ConcreteTypeTag[T]]
        case NullTpe    => ConcreteTypeTag.Null.asInstanceOf[ConcreteTypeTag[T]]
        case StringTpe  => ConcreteTypeTag.String.asInstanceOf[ConcreteTypeTag[T]]
        case _          => new ConcreteTypeTag[T](tpe) {}
      }

    def unapply[T](ttag: TypeTag[T]): Option[Type] = if (ttag.isConcrete) Some(ttag.tpe) else None

    implicit def toClassTag[T](ttag: rm.ConcreteTypeTag[T]): ClassTag[T] = ClassTag[T](rm.typeToClass(ttag.tpe.erasure))

    implicit def toDeprecatedManifestApis[T](ttag: rm.ConcreteTypeTag[T]): DeprecatedManifestApis[T] = new DeprecatedManifestApis[T](ttag)

    // this class should not be used directly in client code
    class DeprecatedManifestApis[T](ttag: rm.ConcreteTypeTag[T]) extends DeprecatedClassManifestApis[T](toClassTag(ttag)) {
      @deprecated("Use `tpe` to analyze the underlying type", "2.10.0")
      def <:<(that: Manifest[_]): Boolean = ttag.tpe <:< that.tpe

      @deprecated("Use `tpe` to analyze the underlying type", "2.10.0")
      def >:>(that: Manifest[_]): Boolean = that <:< ttag

      @deprecated("Use `tpe` to analyze the type arguments", "2.10.0")
      override def typeArguments: List[Manifest[_]] = ttag.tpe.typeArguments map (targ => rm.ConcreteTypeTag(targ))
    }
  }

  // incantations for summoning
  // moved to Context, since rm.tags have their own incantations in Predef, and these guys are only useful in macros
//  def tag[T](implicit ttag: TypeTag[T]) = ttag
//  def typeTag[T](implicit ttag: TypeTag[T]) = ttag
//  def concreteTag[T](implicit gttag: ConcreteTypeTag[T]) = cttag
//  def concreteTypeTag[T](implicit gttag: ConcreteTypeTag[T]) = cttag
}