package scala.reflect

import scala.tools.nsc.Global
import scala.reflect.makro.ReificationError
import scala.reflect.makro.UnexpectedReificationError

package object reify {
  private def mkReifier(global: Global)(typer: global.analyzer.Typer, prefix: global.Tree, reifee: Any, dontSpliceAtTopLevel: Boolean = false, concrete: Boolean = false): Reifier { val mirror: global.type } = {
    val typer1: typer.type = typer
    val prefix1: prefix.type = prefix
    val reifee1 = reifee
    val dontSpliceAtTopLevel1 = dontSpliceAtTopLevel
    val concrete1 = concrete

    new {
      val mirror: global.type = global
      val typer = typer1
      val prefix = prefix1
      val reifee = reifee1
      val dontSpliceAtTopLevel = dontSpliceAtTopLevel1
      val concrete = concrete1
    } with Reifier
  }

  def reifyTree(global: Global)(typer: global.analyzer.Typer, prefix: global.Tree, tree: global.Tree): global.Tree =
    mkReifier(global)(typer, prefix, tree, false, false).reified.asInstanceOf[global.Tree]

  def reifyType(global: Global)(typer: global.analyzer.Typer, prefix: global.Tree, tpe: global.Type, dontSpliceAtTopLevel: Boolean = false, concrete: Boolean = false): global.Tree =
    mkReifier(global)(typer, prefix, tpe, dontSpliceAtTopLevel, concrete).reified.asInstanceOf[global.Tree]

  def reifyErasure(global: Global)(typer0: global.analyzer.Typer, tpe: global.Type, concrete: Boolean = true): global.Tree = {
    import global._
    import definitions._
    val positionBearer = analyzer.openMacros.find(c => c.macroApplication.pos != NoPosition).map(_.macroApplication).getOrElse(EmptyTree).asInstanceOf[Tree]
    val inScope = typer0.context.withMacrosDisabled(typer0.resolveErasureTag(positionBearer.pos, tpe, concrete = concrete), typer0.resolveArrayTag(positionBearer.pos, tpe))
    inScope match {
      case (success, _) if !success.isEmpty =>
        Select(success, nme.erasure)
      case (_, success) if !success.isEmpty =>
        Apply(Select(Ident(ScalaRunTimeModule), arrayElementClassMethod), List(success))
      case _ =>
        if (tpe.typeSymbol == ArrayClass) {
          val componentTpe = tpe.typeArguments(0)
          val componentErasure = reifyErasure(global)(typer0, componentTpe, concrete)
          Apply(Select(Ident(ScalaRunTimeModule), arrayClassMethod), List(componentErasure))
        } else {
          if (tpe.isSpliceable && concrete) throw new ReificationError(positionBearer.pos, "tpe %s is an unresolved spliceable type".format(tpe))
          var erasure = tpe.erasure
          if (tpe.typeSymbol.isDerivedValueClass && global.phase.id < global.currentRun.erasurePhase.id) erasure = tpe
          TypeApply(Select(Ident(PredefModule), nme.classOf), List(TypeTree(erasure)))
        }
    }
  }
}
