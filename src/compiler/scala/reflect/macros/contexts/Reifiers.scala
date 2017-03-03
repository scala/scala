/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Gilles Dubochet
 */

package scala.reflect.macros
package contexts

trait Reifiers {
  self: Context =>

  val global: universe.type = universe
  import universe._
  import definitions._

  def reifyTree(universe: Tree, mirror: Tree, tree: Tree): Tree = {
    assert(ExprClass != NoSymbol)
    val result = scala.reflect.reify.`package`.reifyTree(self.universe)(callsiteTyper, universe, mirror, tree)
    logFreeVars(enclosingPosition, result)
    result
  }

  def reifyType(universe: Tree, mirror: Tree, tpe: Type, concrete: Boolean = false): Tree = {
    assert(TypeTagsClass != NoSymbol)
    val result = scala.reflect.reify.`package`.reifyType(self.universe)(callsiteTyper, universe, mirror, tpe, concrete)
    logFreeVars(enclosingPosition, result)
    result
  }

  def reifyRuntimeClass(tpe: Type, concrete: Boolean = true): Tree =
    scala.reflect.reify.`package`.reifyRuntimeClass(universe)(callsiteTyper, tpe, concrete = concrete)

  def reifyEnclosingRuntimeClass: Tree =
    scala.reflect.reify.`package`.reifyEnclosingRuntimeClass(universe)(callsiteTyper)

  def unreifyTree(tree: Tree): Tree = {
    assert(ExprSplice != NoSymbol)
    Select(tree, ExprSplice)
  }

  // fixme: if I put utils here, then "global" from utils' early initialization syntax
  // and "global" that comes from here conflict with each other when incrementally compiling
  // the problem is that both are pickled with the same owner - trait Reifiers
  // and this upsets the compiler, so that oftentimes it throws assertion failures
  // Martin knows the details
  //
  // object utils extends {
  //   val global: self.global.type = self.global
  //   val typer: global.analyzer.Typer = self.callsiteTyper
  // } with scala.reflect.reify.utils.Utils
  // import utils._

  private def logFreeVars(position: Position, reification: Tree): Unit = {
    object utils extends {
      val global: self.global.type = self.global
      val typer: global.analyzer.Typer = self.callsiteTyper
    } with scala.reflect.reify.utils.Utils
    import utils._

    def logFreeVars(symtab: SymbolTable): Unit =
      // logging free vars only when they are untyped prevents avalanches of duplicate messages
      symtab.syms map (sym => symtab.symDef(sym)) foreach {
        case FreeTermDef(_, _, binding, _, origin) if universe.settings.logFreeTerms && binding.tpe == null =>
          reporter.echo(position, s"free term: ${showRaw(binding)} $origin")
        case FreeTypeDef(_, _, binding, _, origin) if universe.settings.logFreeTypes && binding.tpe == null =>
          reporter.echo(position, s"free type: ${showRaw(binding)} $origin")
        case _ =>
          // do nothing
      }

    if (universe.settings.logFreeTerms || universe.settings.logFreeTypes)
      reification match {
        case ReifiedTree(_, _, symtab, _, _, _, _) => logFreeVars(symtab)
        case ReifiedType(_, _, symtab, _, _, _) => logFreeVars(symtab)
      }
  }
}
