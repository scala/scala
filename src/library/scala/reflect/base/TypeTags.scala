/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect
package base

import java.io.{ObjectInputStream, ObjectOutputStream}
import java.lang.{ Class => jClass }
import language.implicitConversions

/**
 * Type tags encapsulate a representation of type T.
 * They are supposed to replace the pre-2.10 concept of a [[scala.reflect.Manifest]].
 * TypeTags are much better integrated with reflection than manifests are, and are consequently much simpler.
 *
 * === Overview ===
 *
 * Type tags are organized in a hierarchy of three classes:
 * [[scala.reflect.ClassTag]], [[scala.reflect.base.Universe#TypeTag]] and [[scala.reflect.base.Universe#AbsTypeTag]].
 *
 * A [[scala.reflect.ClassTag]] carries a runtime class that corresponds to the source type T.
 * As of such, it possesses the knowledge about how to build single- and multi-dimensional arrays of elements of that type.
 * It guarantees that the source type T did not to contain any references to type parameters or abstract types.
 * [[scala.reflect.ClassTag]] corresponds to a previous notion of [[scala.reflect.ClassManifest]].
 *
 * A [[scala.reflect.base.Universe#AbsTypeTag]] value wraps a full Scala type in its tpe field.
 * A [[scala.reflect.base.Universe#TypeTag]] value is an [[scala.reflect.base.Universe#AbsTypeTag]]
 * that is guaranteed not to contain any references to type parameters or abstract types.
 *
 * [Eugene++] also mention sensitivity to prefixes, i.e. that rb.TypeTag is different from ru.TypeTag
 * [Eugene++] migratability between mirrors and universes is also worth mentioning
 *
 * === Splicing ===
 *
 * Tags can be spliced, i.e. if compiler generates a tag for a type that contains references to tagged
 * type parameters or abstract type members, it will retrieve the corresponding tag and embed it into the result.
 * An example that illustrates the TypeTag embedding, consider the following function:
 *
 *   import reflect.mirror._
 *     def f[T: TypeTag, U] = {
 *       type L = T => U
 *       implicitly[AbsTypeTag[L]]
 *     }
 *
 * Then a call of f[String, Int] will yield a result of the form
 *
 *   AbsTypeTag(<[ String => U ]>).
 *
 * Note that T has been replaced by String, because it comes with a TypeTag in f, whereas U was left as a type parameter.
 *
 * === AbsTypeTag vs TypeTag ===
 *
 * Be careful with AbsTypeTag, because it will reify types even if these types are abstract.
 * This makes it easy to forget to tag one of the methods in the call chain and discover it much later in the runtime
 * by getting cryptic errors far away from their source. For example, consider the following snippet:
 *
 *   def bind[T: AbsTypeTag](name: String, value: T): IR.Result = bind((name, value))
 *   def bind(p: NamedParam): IR.Result                         = bind(p.name, p.tpe, p.value)
 *   object NamedParam {
 *     implicit def namedValue[T: AbsTypeTag](name: String, x: T): NamedParam = apply(name, x)
 *     def apply[T: AbsTypeTag](name: String, x: T): NamedParam = new Typed[T](name, x)
 *   }
 *
 * This fragment of Scala REPL implementation defines a `bind` function that carries a named value along with its type
 * into the heart of the REPL. Using a [[scala.reflect.base.Universe#AbsTypeTag]] here is reasonable, because it is desirable
 * to work with all types, even if they are type parameters or abstract type members.
 *
 * However if any of the three `AbsTypeTag` context bounds is omitted, the resulting code will be incorrect,
 * because the missing `AbsTypeTag` will be transparently generated by the compiler, carrying meaningless information.
 * Most likely, this problem will manifest itself elsewhere, making debugging complicated.
 * If `AbsTypeTag` context bounds were replaced with `TypeTag`, then such errors would be reported statically.
 * But in that case we wouldn't be able to use `bind` in arbitrary contexts.
 *
 * === Backward compatibility ===
 *
 * Type tags correspond loosely to manifests.
 *
 * More precisely:
 * The previous notion of a [[scala.reflect.ClassManifest]] corresponds to a scala.reflect.ClassTag,
 * The previous notion of a [[scala.reflect.Manifest]] corresponds to scala.reflect.runtime.universe.TypeTag,
 *
 * In Scala 2.10, manifests are deprecated, so it's adviseable to migrate them to tags,
 * because manifests might be removed in the next major release.
 *
 * In most cases it will be enough to replace ClassManifests with ClassTags and Manifests with TypeTags,
 * however there are a few caveats:
 *
 * 1) The notion of OptManifest is no longer supported. Tags can reify arbitrary types, so they are always available.
 *    // [Eugene++] it might be useful, though, to guard against abstractness of the incoming type.
 *
 * 2) There's no equivalent for AnyValManifest. Consider comparing your tag with one of the base tags
 *    (defined in the corresponding companion objects) to find out whether it represents a primitive value class.
 *    You can also use `<tag>.tpe.typeSymbol.isPrimitiveValueClass` for that purpose (requires scala-reflect.jar).
 *
 * 3) There's no replacement for factory methods defined in `ClassManifest` and `Manifest` companion objects.
 *    Consider assembling corresponding types using reflection API provided by Java (for classes) and Scala (for types).
 *
 * 4) Certain manifest functions (such as `<:<`, `>:>` and `typeArguments`) weren't included in the tag API.
 *    Consider using reflection API provided by Java (for classes) and Scala (for types) instead.
 */
// [Eugene++] implement serialization for typetags
trait TypeTags { self: Universe =>

  /**
   * If an implicit value of type u.AbsTypeTag[T] is required, the compiler will make one up on demand.
   * The implicitly created value contains in its tpe field a value of type u.Type that is a reflective representation of T.
   * In that value, any occurrences of type parameters or abstract types U
   * which come themselves with a TypeTag are represented by the type referenced by that TypeTag.
   *
   * @see [[scala.reflect.base.TypeTags]]
   */
  @annotation.implicitNotFound(msg = "No AbsTypeTag available for ${T}")
  trait AbsTypeTag[T] extends Equals with Serializable {
    val mirror: Mirror
    def in[U <: Universe with Singleton](otherMirror: MirrorOf[U]): U # AbsTypeTag[T]
    def tpe: Type

    /** case class accessories */
    override def canEqual(x: Any) = x.isInstanceOf[AbsTypeTag[_]]
    override def equals(x: Any) = x.isInstanceOf[AbsTypeTag[_]] && this.mirror == x.asInstanceOf[AbsTypeTag[_]].mirror && this.tpe == x.asInstanceOf[AbsTypeTag[_]].tpe
    override def hashCode = mirror.hashCode * 31 + tpe.hashCode
    override def toString = "AbsTypeTag[" + tpe + "]"
  }

  object AbsTypeTag {
    val Byte    : AbsTypeTag[scala.Byte]       = TypeTag.Byte
    val Short   : AbsTypeTag[scala.Short]      = TypeTag.Short
    val Char    : AbsTypeTag[scala.Char]       = TypeTag.Char
    val Int     : AbsTypeTag[scala.Int]        = TypeTag.Int
    val Long    : AbsTypeTag[scala.Long]       = TypeTag.Long
    val Float   : AbsTypeTag[scala.Float]      = TypeTag.Float
    val Double  : AbsTypeTag[scala.Double]     = TypeTag.Double
    val Boolean : AbsTypeTag[scala.Boolean]    = TypeTag.Boolean
    val Unit    : AbsTypeTag[scala.Unit]       = TypeTag.Unit
    val Any     : AbsTypeTag[scala.Any]        = TypeTag.Any
    val AnyVal  : AbsTypeTag[scala.AnyVal]     = TypeTag.AnyVal
    val AnyRef  : AbsTypeTag[scala.AnyRef]     = TypeTag.AnyRef
    val Object  : AbsTypeTag[java.lang.Object] = TypeTag.Object
    val Nothing : AbsTypeTag[scala.Nothing]    = TypeTag.Nothing
    val Null    : AbsTypeTag[scala.Null]       = TypeTag.Null

    def apply[T](mirror1: MirrorOf[self.type], tpec1: TypeCreator): AbsTypeTag[T] =
      tpec1(mirror1) match {
        case ByteTpe    => AbsTypeTag.Byte.asInstanceOf[AbsTypeTag[T]]
        case ShortTpe   => AbsTypeTag.Short.asInstanceOf[AbsTypeTag[T]]
        case CharTpe    => AbsTypeTag.Char.asInstanceOf[AbsTypeTag[T]]
        case IntTpe     => AbsTypeTag.Int.asInstanceOf[AbsTypeTag[T]]
        case LongTpe    => AbsTypeTag.Long.asInstanceOf[AbsTypeTag[T]]
        case FloatTpe   => AbsTypeTag.Float.asInstanceOf[AbsTypeTag[T]]
        case DoubleTpe  => AbsTypeTag.Double.asInstanceOf[AbsTypeTag[T]]
        case BooleanTpe => AbsTypeTag.Boolean.asInstanceOf[AbsTypeTag[T]]
        case UnitTpe    => AbsTypeTag.Unit.asInstanceOf[AbsTypeTag[T]]
        case AnyTpe     => AbsTypeTag.Any.asInstanceOf[AbsTypeTag[T]]
        case AnyValTpe  => AbsTypeTag.AnyVal.asInstanceOf[AbsTypeTag[T]]
        case AnyRefTpe  => AbsTypeTag.AnyRef.asInstanceOf[AbsTypeTag[T]]
        case ObjectTpe  => AbsTypeTag.Object.asInstanceOf[AbsTypeTag[T]]
        case NothingTpe => AbsTypeTag.Nothing.asInstanceOf[AbsTypeTag[T]]
        case NullTpe    => AbsTypeTag.Null.asInstanceOf[AbsTypeTag[T]]
        case _          => new AbsTypeTagImpl[T](mirror1.asInstanceOf[Mirror], tpec1)
      }

    def unapply[T](ttag: AbsTypeTag[T]): Option[Type] = Some(ttag.tpe)
  }

  private class AbsTypeTagImpl[T](val mirror: Mirror, val tpec: TypeCreator) extends AbsTypeTag[T] {
    lazy val tpe: Type = tpec[self.type](mirror)
    def in[U <: Universe with Singleton](otherMirror: MirrorOf[U]): U # AbsTypeTag[T] = {
      val otherMirror1 = otherMirror.asInstanceOf[MirrorOf[otherMirror.universe.type]]
      otherMirror.universe.AbsTypeTag[T](otherMirror1, tpec)
    }
  }

  /**
   * If an implicit value of type u.TypeTag[T] is required, the compiler will make one up on demand following the same procedure as for TypeTags.
   * However, if the resulting type still contains references to type parameters or abstract types, a static error results.
   *
   * @see [[scala.reflect.base.TypeTags]]
   */
  @annotation.implicitNotFound(msg = "No TypeTag available for ${T}")
  trait TypeTag[T] extends AbsTypeTag[T] with Equals with Serializable {
    override def in[U <: Universe with Singleton](otherMirror: MirrorOf[U]): U # TypeTag[T]

    /** case class accessories */
    override def canEqual(x: Any) = x.isInstanceOf[TypeTag[_]]
    override def equals(x: Any) = x.isInstanceOf[TypeTag[_]] && this.mirror == x.asInstanceOf[TypeTag[_]].mirror && this.tpe == x.asInstanceOf[TypeTag[_]].tpe
    override def hashCode = mirror.hashCode * 31 + tpe.hashCode
    override def toString = "TypeTag[" + tpe + "]"
  }

  object TypeTag {
    val Byte:    TypeTag[scala.Byte]       = new PredefTypeTag[scala.Byte]       (ByteTpe,    _.TypeTag.Byte)
    val Short:   TypeTag[scala.Short]      = new PredefTypeTag[scala.Short]      (ShortTpe,   _.TypeTag.Short)
    val Char:    TypeTag[scala.Char]       = new PredefTypeTag[scala.Char]       (CharTpe,    _.TypeTag.Char)
    val Int:     TypeTag[scala.Int]        = new PredefTypeTag[scala.Int]        (IntTpe,     _.TypeTag.Int)
    val Long:    TypeTag[scala.Long]       = new PredefTypeTag[scala.Long]       (LongTpe,    _.TypeTag.Long)
    val Float:   TypeTag[scala.Float]      = new PredefTypeTag[scala.Float]      (FloatTpe,   _.TypeTag.Float)
    val Double:  TypeTag[scala.Double]     = new PredefTypeTag[scala.Double]     (DoubleTpe,  _.TypeTag.Double)
    val Boolean: TypeTag[scala.Boolean]    = new PredefTypeTag[scala.Boolean]    (BooleanTpe, _.TypeTag.Boolean)
    val Unit:    TypeTag[scala.Unit]       = new PredefTypeTag[scala.Unit]       (UnitTpe,    _.TypeTag.Unit)
    val Any:     TypeTag[scala.Any]        = new PredefTypeTag[scala.Any]        (AnyTpe,     _.TypeTag.Any)
    val AnyVal:  TypeTag[scala.AnyVal]     = new PredefTypeTag[scala.AnyVal]     (AnyValTpe,  _.TypeTag.AnyVal)
    val AnyRef:  TypeTag[scala.AnyRef]     = new PredefTypeTag[scala.AnyRef]     (AnyRefTpe,  _.TypeTag.AnyRef)
    val Object:  TypeTag[java.lang.Object] = new PredefTypeTag[java.lang.Object] (ObjectTpe,  _.TypeTag.Object)
    val Nothing: TypeTag[scala.Nothing]    = new PredefTypeTag[scala.Nothing]    (NothingTpe, _.TypeTag.Nothing)
    val Null:    TypeTag[scala.Null]       = new PredefTypeTag[scala.Null]       (NullTpe,    _.TypeTag.Null)

    def apply[T](mirror1: MirrorOf[self.type], tpec1: TypeCreator): TypeTag[T] =
      tpec1(mirror1) match {
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
        case AnyValTpe  => TypeTag.AnyVal.asInstanceOf[TypeTag[T]]
        case AnyRefTpe  => TypeTag.AnyRef.asInstanceOf[TypeTag[T]]
        case ObjectTpe  => TypeTag.Object.asInstanceOf[TypeTag[T]]
        case NothingTpe => TypeTag.Nothing.asInstanceOf[TypeTag[T]]
        case NullTpe    => TypeTag.Null.asInstanceOf[TypeTag[T]]
        case _          => new TypeTagImpl[T](mirror1.asInstanceOf[Mirror], tpec1)
      }

    def unapply[T](ttag: TypeTag[T]): Option[Type] = Some(ttag.tpe)
  }

  private class TypeTagImpl[T](var mirror: Mirror, var tpec: TypeCreator) extends AbsTypeTagImpl[T](mirror, tpec) with TypeTag[T] {
    override def in[U <: Universe with Singleton](otherMirror: MirrorOf[U]): U # TypeTag[T] = {
      val otherMirror1 = otherMirror.asInstanceOf[MirrorOf[otherMirror.universe.type]]
      otherMirror.universe.TypeTag[T](otherMirror1, tpec)
    }

    private def writeObject(out: ObjectOutputStream) {
      out.writeObject(tpec)
    }

    private def readObject(in: ObjectInputStream) {
      mirror = scala.reflect.basis
      tpec = in.readObject()
    }
  }

  private class PredefTypeTag[T](_tpe: Type, copyIn: Universe => Universe # TypeTag[T]) extends TypeTagImpl[T](rootMirror, null) {
    override lazy val tpe: Type = _tpe
    override def in[U <: Universe with Singleton](otherMirror: MirrorOf[U]): U # TypeTag[T] =
      copyIn(otherMirror.universe).asInstanceOf[U # TypeTag[T]]
    private def readResolve() = copyIn(self)
  }

  // incantations
  def typeTag[T](implicit ttag: TypeTag[T]) = ttag

  // big thanks to Viktor Klang for this brilliant idea!
  def typeOf[T](implicit ttag: TypeTag[T]): Type = ttag.tpe
}
