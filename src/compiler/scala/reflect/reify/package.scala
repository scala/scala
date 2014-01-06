package scala
package reflect

import scala.reflect.macros.ReificationException
import scala.tools.nsc.Global

package object reify {
  private def mkReifier(global1: Global)(typer: global1.analyzer.Typer, universe: global1.Tree, mirror: global1.Tree, reifee: Any, concrete: Boolean): Reifier { val global: global1.type } = {
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
    import definitions.JavaUniverseClass

    val enclosingErasure = {
      val rClassTree = reifyEnclosingRuntimeClass(global)(typer0)
      // HACK around SI-6259
      // If we're in the constructor of an object or others don't have easy access to `this`, we have no good way to grab
      // the class of that object.  Instead, we construct an anonymous class and grab his class file, assuming
      // this is enough to get the correct class loadeer for the class we *want* a mirror for, the object itself.
      rClassTree orElse Apply(Select(gen.mkAnonymousNew(Nil), sn.GetClass), Nil)
    }
    // JavaUniverse is defined in scala-reflect.jar, so we must be very careful in case someone reifies stuff having only scala-library.jar on the classpath
    val isJavaUniverse = JavaUniverseClass != NoSymbol && universe.tpe <:< JavaUniverseClass.toTypeConstructor
    if (isJavaUniverse && !enclosingErasure.isEmpty) Apply(Select(universe, nme.runtimeMirror), List(Select(enclosingErasure, sn.GetClassLoader)))
    else Select(universe, nme.rootMirror)
  }

  def reifyTree(global: Global)(typer: global.analyzer.Typer, universe: global.Tree, mirror: global.Tree, tree: global.Tree): global.Tree =
    mkReifier(global)(typer, universe, mirror, tree, concrete = false).reification.asInstanceOf[global.Tree]

  def reifyType(global: Global)(typer: global.analyzer.Typer,universe: global.Tree, mirror: global.Tree, tpe: global.Type, concrete: Boolean = false): global.Tree =
    mkReifier(global)(typer, universe, mirror, tpe, concrete = concrete).reification.asInstanceOf[global.Tree]

  def reifyRuntimeClass(global: Global)(typer0: global.analyzer.Typer, tpe0: global.Type, concrete: Boolean = true): global.Tree = {
    import global._
    import definitions._
    import analyzer.enclosingMacroPosition

    // SI-7375
    val tpe = tpe0.dealiasWiden

    if (tpe.isSpliceable) {
      val classTagInScope = typer0.resolveClassTag(enclosingMacroPosition, tpe, allowMaterialization = false)
      if (!classTagInScope.isEmpty) return Select(classTagInScope, nme.runtimeClass)
      if (concrete) throw new ReificationException(enclosingMacroPosition, "tpe %s is an unresolved spliceable type".format(tpe))
    }

    tpe.dealiasWiden match {
      case TypeRef(_, ArrayClass, componentTpe :: Nil) =>
        val componentErasure = reifyRuntimeClass(global)(typer0, componentTpe, concrete)
        gen.mkMethodCall(currentRun.runDefinitions.arrayClassMethod, List(componentErasure))
      case _ =>
        var erasure = tpe.erasure
        if (tpe.typeSymbol.isDerivedValueClass && global.phase.id < global.currentRun.erasurePhase.id) erasure = tpe
        gen.mkNullaryCall(currentRun.runDefinitions.Predef_classOf, List(erasure))
    }
  }

  // Note: If  current context is inside the constructor of an object or otherwise not inside
  // a class/object body, this will return an EmptyTree.
  def reifyEnclosingRuntimeClass(global: Global)(typer0: global.analyzer.Typer): global.Tree = {
    import global._
    def isThisInScope = typer0.context.enclosingContextChain exists (_.tree.isInstanceOf[ImplDef])
    if (isThisInScope) {
      val enclosingClasses = typer0.context.enclosingContextChain map (_.tree) collect { case classDef: ClassDef => classDef }
      val classInScope = enclosingClasses.headOption getOrElse EmptyTree
      def isUnsafeToUseThis = {
        val isInsideConstructorSuper = typer0.context.enclosingContextChain exists (_.inSelfSuperCall)
        // Note: It's ok to check for any object here, because if we were in an enclosing class, we'd already have returned its classOf
        val isInsideObject = typer0.context.enclosingContextChain map (_.tree) exists {	case _: ModuleDef => true; case _ => false }
        isInsideConstructorSuper && isInsideObject
      }
      if (!classInScope.isEmpty) reifyRuntimeClass(global)(typer0, classInScope.symbol.toTypeConstructor, concrete = true)
      else if(!isUnsafeToUseThis) Select(This(tpnme.EMPTY), sn.GetClass)
      else EmptyTree
    } else EmptyTree
  }
}
