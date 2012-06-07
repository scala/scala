package scala.reflect

import language.implicitConversions
import language.experimental.macros
import scala.reflect.base.{Universe => BaseUniverse}
import scala.reflect.makro.{Context, ReificationError, UnexpectedReificationError}
import scala.tools.nsc.Global

package object reify {
  private def mkReifier(global1: Global)(typer: global1.analyzer.Typer, universe: global1.Tree, mirror: global1.Tree, reifee: Any, concrete: Boolean = false): Reifier { val global: global1.type } = {
    val typer1: typer.type = typer
    val universe1: universe.type = universe
    val mirror1: mirror.type = mirror
    val reifee1 = reifee
    val concrete1 = concrete

    new {
      val global: global1.type = global1
      val typer = typer1
      val universe = universe1
      val mirror = mirror1
      val reifee = reifee1
      val concrete = concrete1
    } with Reifier
  }

  private[reify] def mkDefaultMirrorRef(global: Global)(universe: global.Tree, typer0: global.analyzer.Typer): global.Tree = {
    import global._
    import definitions._
    val enclosingErasure = reifyEnclosingRuntimeClass(global)(typer0)
    // JavaUniverse is defined in scala-reflect.jar, so we must be very careful in case someone reifies stuff having only scala-library.jar on the classpath
    val isJavaUniverse = JavaUniverseClass != NoSymbol && universe.tpe <:< JavaUniverseClass.asTypeConstructor
    if (isJavaUniverse && !enclosingErasure.isEmpty) Apply(Select(universe, nme.runtimeMirror), List(Select(enclosingErasure, sn.GetClassLoader)))
    else Select(universe, nme.rootMirror)
  }

  def reifyTree(global: Global)(typer: global.analyzer.Typer, universe: global.Tree, mirror: global.Tree, tree: global.Tree): global.Tree =
    mkReifier(global)(typer, universe, mirror, tree, concrete = false).reification.asInstanceOf[global.Tree]

  def reifyType(global: Global)(typer: global.analyzer.Typer,universe: global.Tree, mirror: global.Tree, tpe: global.Type, concrete: Boolean = false): global.Tree =
    mkReifier(global)(typer, universe, mirror, tpe, concrete = concrete).reification.asInstanceOf[global.Tree]

  def reifyRuntimeClass(global: Global)(typer0: global.analyzer.Typer, tpe: global.Type, concrete: Boolean = true): global.Tree = {
    import global._
    import definitions._
    import analyzer.enclosingMacroPosition

    if (tpe.isSpliceable) {
      val classTagInScope = typer0.resolveClassTag(enclosingMacroPosition, tpe, allowMaterialization = false)
      if (!classTagInScope.isEmpty) return Select(classTagInScope, nme.runtimeClass)
      if (concrete) throw new ReificationError(enclosingMacroPosition, "tpe %s is an unresolved spliceable type".format(tpe))
    }

    tpe.normalize match {
      case TypeRef(_, ArrayClass, componentTpe :: Nil) =>
        val componentErasure = reifyRuntimeClass(global)(typer0, componentTpe, concrete)
        gen.mkMethodCall(arrayClassMethod, List(componentErasure))
      case _ =>
        var erasure = tpe.erasure
        if (tpe.typeSymbol.isDerivedValueClass && global.phase.id < global.currentRun.erasurePhase.id) erasure = tpe
        gen.mkNullaryCall(Predef_classOf, List(erasure))
    }
  }

  def reifyEnclosingRuntimeClass(global: Global)(typer0: global.analyzer.Typer): global.Tree = {
    import global._
    import definitions._
    def isThisInScope = typer0.context.enclosingContextChain exists (_.tree.isInstanceOf[Template])
    if (isThisInScope) {
      val enclosingClasses = typer0.context.enclosingContextChain map (_.tree) collect { case classDef: ClassDef => classDef }
      val classInScope = enclosingClasses.headOption getOrElse EmptyTree
      if (!classInScope.isEmpty) reifyRuntimeClass(global)(typer0, classInScope.symbol.asTypeConstructor, concrete = true)
      else Select(This(tpnme.EMPTY), sn.GetClass)
    } else EmptyTree
  }
}
