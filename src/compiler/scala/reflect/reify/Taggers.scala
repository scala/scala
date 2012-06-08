package scala.reflect.reify

import scala.reflect.makro.{ReificationError, UnexpectedReificationError}
import scala.reflect.makro.runtime.Context

abstract class Taggers {
  val c: Context

  import c.universe._
  import definitions._
  import treeBuild._

  val coreTags = Map(
    ByteClass.asType -> nme.Byte,
    ShortClass.asType -> nme.Short,
    CharClass.asType -> nme.Char,
    IntClass.asType -> nme.Int,
    LongClass.asType -> nme.Long,
    FloatClass.asType -> nme.Float,
    DoubleClass.asType -> nme.Double,
    BooleanClass.asType -> nme.Boolean,
    UnitClass.asType -> nme.Unit,
    AnyClass.asType -> nme.Any,
    ObjectClass.asType -> nme.Object,
    NothingClass.asType -> nme.Nothing,
    NullClass.asType -> nme.Null,
    StringClass.asType -> nme.String)

  def materializeClassTag(prefix: Tree, tpe: Type): Tree = {
    val tagModule = ClassTagModule
    materializeTag(prefix, tpe, tagModule, {
      val erasure = c.reifyRuntimeClass(tpe, concrete = true)
      val factory = TypeApply(Select(Ident(tagModule), nme.apply), List(TypeTree(tpe)))
      Apply(factory, List(erasure))
    })
  }

  def materializeTypeTag(universe: Tree, mirror: Tree, tpe: Type, concrete: Boolean): Tree = {
    if (universe.symbol == MacroContextUniverse && mirror == EmptyTree) {
      import scala.reflect.makro.runtime.ContextReifiers
      import language.implicitConversions
      implicit def context2contextreifiers(c0: Context) : ContextReifiers { val c: c0.type } = new { val c: c0.type = c0 } with ContextReifiers
      val Select(prefix, _) = universe
      c.materializeTypeTagForMacroContext(prefix, tpe, concrete)
    } else {
      val tagType = if (concrete) TypeTagClass else AbsTypeTagClass
      val unaffiliatedTagTpe = TypeRef(BaseUniverseClass.asTypeConstructor, tagType, List(tpe))
      val unaffiliatedTag = c.inferImplicitValue(unaffiliatedTagTpe, silent = true, withMacrosDisabled = true)
      unaffiliatedTag match {
        case success if !success.isEmpty =>
          Apply(Select(success, nme.in), List(mirror orElse mkDefaultMirrorRef(c.universe)(universe, c.callsiteTyper)))
        case _ =>
          val tagModule = if (concrete) TypeTagModule else AbsTypeTagModule
          materializeTag(universe, tpe, tagModule, c.reifyType(universe, mirror, tpe, concrete = concrete))
      }
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
