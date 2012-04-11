package scala.reflect

import scala.tools.nsc.Global

package object reify {
  def mkReifier(global: Global)(typer: global.analyzer.Typer, prefix: global.Tree, reifee: Any, dontSpliceAtTopLevel: Boolean = false, requireGroundTypeTag: Boolean = false): Reifier { val mirror: global.type } = {
    val typer1: typer.type = typer
    val prefix1: prefix.type = prefix
    val reifee1 = reifee
    val dontSpliceAtTopLevel1 = dontSpliceAtTopLevel
    val requireGroundTypeTag1 = requireGroundTypeTag

    new {
      val mirror: global.type = global
      val typer = typer1
      val prefix = prefix1
      val reifee = reifee1
      val dontSpliceAtTopLevel = dontSpliceAtTopLevel1
      val requireGroundTypeTag = requireGroundTypeTag1
    } with Reifier
  }
}
