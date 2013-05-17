package scala.reflect.reify

import scala.reflect.macros.ReificationException
import scala.reflect.macros.UnexpectedReificationException

trait Errors {
  self: Reifier =>

  import global._

  def defaultErrorPosition = {
    val stack = currents collect { case t: Tree if t.pos != NoPosition => t.pos }
    stack.headOption getOrElse analyzer.enclosingMacroPosition
  }

  // expected errors: these can happen if the user casually writes whatever.reify(...)
  // hence we don't crash here, but nicely report a typechecking error and bail out asap

  def CannotReifyType(tpe: Type) = {
    val msg = "implementation restriction: cannot reify type %s (%s)".format(tpe, tpe.kind)
    throw new ReificationException(defaultErrorPosition, msg)
  }

  def CannotReifyCompoundTypeTreeWithNonEmptyBody(ctt: CompoundTypeTree) = {
    val msg = "implementation restriction: cannot reify refinement type trees with non-empty bodies"
    throw new ReificationException(ctt.pos, msg)
  }

  def CannotReifyWeakType(details: Any) = {
    val msg = "cannot create a TypeTag" + details + ": use WeakTypeTag instead"
    throw new ReificationException(defaultErrorPosition, msg)
  }

  def CannotConvertManifestToTagWithoutScalaReflect(tpe: Type, manifestInScope: Tree) = {
    val msg =
      sm"""to create a type tag here, it is necessary to interoperate with the manifest `$manifestInScope` in scope.
          |however manifest -> typetag conversion requires Scala reflection, which is not present on the classpath.
          |to proceed put scala-reflect.jar on your compilation classpath and recompile."""
    throw new ReificationException(defaultErrorPosition, msg)
  }

  def CannotReifyRuntimeSplice(tree: Tree) = {
    val msg = """
      |the splice cannot be resolved statically, which means there is a cross-stage evaluation involved.
      |cross-stage evaluations need to be invoked explicitly, so we're showing you this error.
      |if you're sure this is not an oversight, add scala-compiler.jar to the classpath,
      |import `scala.tools.reflect.Eval` and call `<your expr>.eval` instead.""".trim.stripMargin
    throw new ReificationException(tree.pos, msg)
  }

  // unexpected errors: these can never happen under normal conditions unless there's a bug in the compiler (or in a compiler plugin or in a macro)
  // hence, we fail fast and loudly and don't care about being nice - in this situation noone will appreciate our quiet nicety

  def CannotReifyUntypedPrefix(prefix: Tree) = {
    val msg = "internal error: untyped prefixes are not supported, consider typechecking the prefix before passing it to the reifier"
    throw new UnexpectedReificationException(defaultErrorPosition, msg)
  }

  def CannotReifyUntypedReifee(reifee: Any) = {
    val msg = "internal error: untyped trees are not supported, consider typechecking the reifee before passing it to the reifier"
    throw new UnexpectedReificationException(defaultErrorPosition, msg)
  }

  def CannotReifyErroneousPrefix(prefix: Tree) = {
    val msg = "internal error: erroneous prefixes are not supported, make sure that your prefix has typechecked successfully before passing it to the reifier"
    throw new UnexpectedReificationException(defaultErrorPosition, msg)
  }

  def CannotReifyErroneousReifee(reifee: Any) = {
    val msg = "internal error: erroneous reifees are not supported, make sure that your reifee has typechecked successfully before passing it to the reifier"
    throw new UnexpectedReificationException(defaultErrorPosition, msg)
  }

  def CannotReifyInvalidLazyVal(tree: ValDef) = {
    val msg = "internal error: could not reconstruct original lazy val due to missing accessor"
    throw new UnexpectedReificationException(tree.pos, msg)
  }
}
