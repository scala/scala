package scala.reflect
package reify

import scala.tools.nsc.Global

trait Errors {
  self: Reifier =>

  import mirror._
  import definitions._

  class ReificationError(var pos: Position, val msg: String) extends Throwable(msg)
  class UnexpectedReificationError(val pos: Position, val msg: String, val cause: Throwable = null) extends Throwable(msg)

  lazy val defaultErrorPosition: Position =
    mirror.analyzer.openMacros.find(c => c.macroApplication.pos != NoPosition).map(_.macroApplication.pos).getOrElse(NoPosition)

  // expected errors: these can happen if the user casually writes whatever.reify(...)
  // hence we don't crash here, but nicely report a typechecking error and bail out asap

  def CannotReifyReifeeThatHasTypeLocalToReifee(tree: Tree) = {
    val msg = "implementation restriction: cannot reify block of type %s that involves a type declared inside the block being reified. consider casting the return value to a suitable type".format(tree.tpe)
    throw new ReificationError(tree.pos, msg)
  }

  def CannotReifyType(tpe: Type) = {
    val msg = "implementation restriction: cannot reify type %s (%s)".format(tpe, tpe.kind)
    throw new ReificationError(defaultErrorPosition, msg)
  }

  def CannotReifySymbol(sym: Symbol) = {
    val msg = "implementation restriction: cannot reify symbol %s (%s)".format(sym, sym.accurateKindString)
    throw new ReificationError(defaultErrorPosition, msg)
  }

  def CannotReifyConcreteTypeTagHavingUnresolvedTypeParameters(tpe: Type) = {
    val msg = "cannot reify ConcreteTypeTag having unresolved type parameter %s".format(tpe)
    throw new ReificationError(defaultErrorPosition, msg)
  }

  // unexpected errors: these can never happen under normal conditions unless there's a bug in the compiler (or in a compiler plugin or in a macro)
  // hence, we fail fast and loudly and don't care about being nice - in this situation noone will appreciate our quiet nicety

  def CannotReifyUntypedPrefix(prefix: Tree) = {
    val msg = "internal error: untyped prefixes are not supported, consider typechecking the prefix before passing it to the reifier"
    throw new UnexpectedReificationError(defaultErrorPosition, msg)
  }

  def CannotReifyUntypedReifee(reifee: Any) = {
    val msg = "internal error: untyped trees are not supported, consider typechecking the reifee before passing it to the reifier"
    throw new UnexpectedReificationError(defaultErrorPosition, msg)
  }

  def CannotReifyErroneousPrefix(prefix: Tree) = {
    val msg = "internal error: erroneous prefixes are not supported, make sure that your prefix has typechecked successfully before passing it to the reifier"
    throw new UnexpectedReificationError(defaultErrorPosition, msg)
  }

  def CannotReifyErroneousReifee(reifee: Any) = {
    val msg = "internal error: erroneous reifees are not supported, make sure that your reifee has typechecked successfully before passing it to the reifier"
    throw new UnexpectedReificationError(defaultErrorPosition, msg)
  }
}