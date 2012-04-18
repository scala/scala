package scala.tools.nsc
package ast

trait FreeVars extends reflect.internal.FreeVars { self: Global =>

  import self._
  import definitions._
  import treeInfo._

  def logFreeVars(position: Position, reified: Tree): Unit = {
    if (settings.logFreeTerms.value || settings.logFreeTypes.value) {
      reified match {
        case Reified(_, symbolTable, _) =>
          // logging free vars only when they are untyped prevents avalanches of duplicate messages
          symbolTable foreach {
            case FreeTermDef(_, _, binding, _, origin) if settings.logFreeTerms.value && binding.tpe == null =>
              reporter.echo(position, "free term: %s %s".format(showRaw(binding), origin))
            case FreeTypeDef(_, _, binding, _, origin) if settings.logFreeTypes.value && binding.tpe == null =>
              reporter.echo(position, "free type: %s %s".format(showRaw(binding), origin))
            case _ =>
              // do nothing
          }
      }
    }
  }
}