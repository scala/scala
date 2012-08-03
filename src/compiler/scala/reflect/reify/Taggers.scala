package scala.reflect.reify

import scala.reflect.macros.{ReificationError, UnexpectedReificationError}
import scala.reflect.macros.runtime.Context

abstract class Taggers {
  val c: Context

  import c.universe._
  import definitions._
  import treeBuild._

  val coreTags = Map(
    ByteTpe -> nme.Byte,
    ShortTpe -> nme.Short,
    CharTpe -> nme.Char,
    IntTpe -> nme.Int,
    LongTpe -> nme.Long,
    FloatTpe -> nme.Float,
    DoubleTpe -> nme.Double,
    BooleanTpe -> nme.Boolean,
    UnitTpe -> nme.Unit,
    AnyTpe -> nme.Any,
    AnyValTpe -> nme.AnyVal,
    AnyRefTpe -> nme.AnyRef,
    ObjectTpe -> nme.Object,
    NothingTpe -> nme.Nothing,
    NullTpe -> nme.Null)

  def materializeClassTag(prefix: Tree, tpe: Type): Tree = {
    val tagModule = ClassTagModule
    materializeTag(prefix, tpe, tagModule, {
      val erasure = c.reifyRuntimeClass(tpe, concrete = true)
      val factory = TypeApply(Select(Ident(tagModule), nme.apply), List(TypeTree(tpe)))
      Apply(factory, List(erasure))
    })
  }

  def materializeTypeTag(universe: Tree, mirror: Tree, tpe: Type, concrete: Boolean): Tree = {
    val tagType = if (concrete) TypeTagClass else AbsTypeTagClass
    // what we need here is to compose a type BaseUniverse # TypeTag[$tpe]
    // to look for an implicit that conforms to this type
    // that's why neither appliedType(tagType, List(tpe)) aka TypeRef(TypeTagsClass.thisType, tagType, List(tpe))
    // nor TypeRef(BaseUniverseClass.thisType, tagType, List(tpe)) won't fit here
    // scala> :type -v def foo: scala.reflect.base.Universe#TypeTag[Int] = ???
    // NullaryMethodType(TypeRef(pre = TypeRef(TypeSymbol(Universe)), TypeSymbol(TypeTag), args = List($tpe))))
    val unaffiliatedTagTpe = TypeRef(BaseUniverseClass.typeConstructor, tagType, List(tpe))
    val unaffiliatedTag = c.inferImplicitValue(unaffiliatedTagTpe, silent = true, withMacrosDisabled = true)
    unaffiliatedTag match {
      case success if !success.isEmpty =>
        Apply(Select(success, nme.in), List(mirror orElse mkDefaultMirrorRef(c.universe)(universe, c.callsiteTyper)))
      case _ =>
        val tagModule = if (concrete) TypeTagModule else AbsTypeTagModule
        materializeTag(universe, tpe, tagModule, c.reifyType(universe, mirror, tpe, concrete = concrete))
    }
  }

  private def materializeTag(prefix: Tree, tpe: Type, tagModule: Symbol, materializer: => Tree): Tree = {
    val result =
      tpe match {
        case coreTpe if coreTags contains coreTpe =>
          val ref = if (tagModule.owner.isPackageClass) Ident(tagModule) else Select(prefix, tagModule.name)
          Select(ref, coreTags(coreTpe))
        case _ =>
          translatingReificationErrors(materializer)
      }
    try c.typeCheck(result)
    catch { case terr @ c.TypeError(pos, msg) => failTag(result, terr) }
  }

  def materializeExpr(universe: Tree, mirror: Tree, expr: Tree): Tree = {
    val result = translatingReificationErrors(c.reifyTree(universe, mirror, expr))
    try c.typeCheck(result)
    catch { case terr @ c.TypeError(pos, msg) => failExpr(result, terr) }
  }

  private def translatingReificationErrors(materializer: => Tree): Tree = {
    try materializer
    catch {
      case ReificationError(pos, msg) =>
        c.error(pos.asInstanceOf[c.Position], msg) // this cast is a very small price for the sanity of exception handling
        EmptyTree
      case UnexpectedReificationError(pos, err, cause) if cause != null =>
        throw cause
    }
  }

  private def failTag(result: Tree, reason: Any): Nothing = {
    val Apply(TypeApply(fun, List(tpeTree)), _) = c.macroApplication
    val tpe = tpeTree.tpe
    val PolyType(_, MethodType(_, tagTpe)) = fun.tpe
    val tagModule = tagTpe.typeSymbol.companionSymbol
    if (c.compilerSettings.contains("-Xlog-implicits"))
      c.echo(c.enclosingPosition, s"cannot materialize ${tagModule.name}[$tpe] as $result because:\n$reason")
    c.abort(c.enclosingPosition, "No %s available for %s".format(tagModule.name, tpe))
  }

  private def failExpr(result: Tree, reason: Any): Nothing = {
    val Apply(_, expr :: Nil) = c.macroApplication
    c.abort(c.enclosingPosition, s"Cannot materialize $expr as $result because:\n$reason")
  }
}
