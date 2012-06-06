package scala.reflect.reify

import scala.reflect.makro.{ReificationError, UnexpectedReificationError}
import scala.reflect.makro.runtime.Context

abstract class Taggers {
  val c: Context

  import c.mirror._
  import definitions._

  val coreTags = Map(
    ByteClass.asType -> newTermName("Byte"),
    ShortClass.asType -> newTermName("Short"),
    CharClass.asType -> newTermName("Char"),
    IntClass.asType -> newTermName("Int"),
    LongClass.asType -> newTermName("Long"),
    FloatClass.asType -> newTermName("Float"),
    DoubleClass.asType -> newTermName("Double"),
    BooleanClass.asType -> newTermName("Boolean"),
    UnitClass.asType -> newTermName("Unit"),
    AnyClass.asType -> newTermName("Any"),
    ObjectClass.asType -> newTermName("Object"),
    AnyValClass.asType -> newTermName("AnyVal"),
    AnyRefClass.asType -> newTermName("AnyRef"),
    NothingClass.asType -> newTermName("Nothing"),
    NullClass.asType -> newTermName("Null"),
    StringClass.asType -> newTermName("String"))

  // todo. the following two methods won't be necessary once we implement implicit macro generators for tags

  def materializeArrayTag(prefix: Tree, tpe: Type): Tree =
    materializeClassTag(prefix, tpe)

  def materializeErasureTag(prefix: Tree, tpe: Type, concrete: Boolean): Tree =
    if (concrete) materializeClassTag(prefix, tpe) else materializeTypeTag(prefix, tpe, concrete = false)

  def materializeClassTag(prefix: Tree, tpe: Type): Tree =
    materializeTag(prefix, tpe, ClassTagModule, {
      val erasure = c.reifyErasure(tpe, concrete = true)
      val factory = TypeApply(Select(Ident(ClassTagModule), "apply"), List(TypeTree(tpe)))
      Apply(factory, List(erasure))
    })

  def materializeTypeTag(prefix: Tree, tpe: Type, concrete: Boolean): Tree = {
    val tagModule = if (concrete) ConcreteTypeTagModule else TypeTagModule
    materializeTag(prefix, tpe, tagModule, c.reifyType(prefix, tpe, dontSpliceAtTopLevel = true, concrete = concrete))
  }

  private def materializeTag(prefix: Tree, tpe: Type, tagModule: Symbol, materializer: => Tree): Tree = {
    val result =
      tpe match {
        case coreTpe if coreTags contains coreTpe =>
          val ref = if (tagModule.owner.isPackageClass) Ident(tagModule) else Select(prefix, tagModule.name)
          Select(ref, coreTags(coreTpe))
        case _ =>
          val manifestInScope = nonSyntheticManifestInScope(tpe)
          if (manifestInScope.isEmpty) translatingReificationErrors(materializer)
          else gen.mkMethodCall(staticModule("scala.reflect.package"), newTermName("manifestToConcreteTypeTag"), List(tpe), List(manifestInScope))
      }
    try c.typeCheck(result)
    catch { case terr @ c.TypeError(pos, msg) => failTag(result, terr) }
  }

  private def nonSyntheticManifestInScope(tpe: Type) = {
    val ManifestClass = staticClass("scala.reflect.Manifest")
    val ManifestModule = staticModule("scala.reflect.Manifest")
    val manifest = c.inferImplicitValue(appliedType(ManifestClass.asTypeConstructor, List(tpe)))
    val notOk = manifest.isEmpty || (manifest exists (sub => sub.symbol != null && (sub.symbol == ManifestModule || sub.symbol.owner == ManifestModule)))
    if (notOk) EmptyTree else manifest
  }

  def materializeExpr(prefix: Tree, expr: Tree): Tree = {
    val result = translatingReificationErrors(c.reifyTree(prefix, expr))
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
