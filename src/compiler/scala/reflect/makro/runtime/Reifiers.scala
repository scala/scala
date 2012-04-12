/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Gilles Dubochet
 */

package scala.reflect.makro
package runtime

trait Reifiers {
  self: Context =>

  import mirror._

  lazy val reflectMirrorPrefix: Tree = {
    // [Eugene] how do I typecheck this without undergoing this tiresome (and, in general, incorrect) procedure?
    val prefix: Tree = Select(Select(Ident(definitions.ScalaPackage), newTermName("reflect")), newTermName("mirror"))
    val prefixTpe = typeCheck(TypeApply(Select(prefix, newTermName("asInstanceOf")), List(SingletonTypeTree(prefix)))).tpe
    typeCheck(prefix) setType prefixTpe
  }

  def reifyTree(prefix: Tree, tree: Tree): Tree =
    reifyTopLevel(prefix, tree)

  def reifyType(prefix: Tree, tpe: Type, dontSpliceAtTopLevel: Boolean = false, requireConcreteTypeTag: Boolean = false): Tree =
    reifyTopLevel(prefix, tpe, dontSpliceAtTopLevel, requireConcreteTypeTag)

  def unreifyTree(tree: Tree): Tree =
    Select(tree, definitions.ExprEval)

  def reifyTopLevel(prefix: Tree, reifee: Any, dontSpliceAtTopLevel: Boolean = false, requireConcreteTypeTag: Boolean = false): Tree = {
    // [Eugene] the plumbing is not very pretty, but anyways factoring out the reifier seems like a necessary step to me
    import scala.reflect.reify._
    val reifier = mkReifier(mirror)(callsiteTyper, prefix, reifee, dontSpliceAtTopLevel, requireConcreteTypeTag)

    try {
      val result = reifier.reified
      logFreeVars(expandee.pos, result)
      result
    } catch {
      case ex: reifier.ReificationError =>
//        // this is a "soft" exception - it will normally be caught by the macro
//        // consequently, we need to log the stack trace here, so that it doesn't get lost
//        if (settings.Yreifydebug.value) {
//          val message = new java.io.StringWriter()
//          ex.printStackTrace(new java.io.PrintWriter(message))
//          println(scala.compat.Platform.EOL + message)
//        }
        val xlated = new ReificationError(ex.pos, ex.msg)
        xlated.setStackTrace(ex.getStackTrace)
        throw xlated
      case ex: reifier.UnexpectedReificationError =>
        val xlated = new UnexpectedReificationError(ex.pos, ex.msg, ex.cause)
        xlated.setStackTrace(ex.getStackTrace)
        throw xlated
    }
  }

  class ReificationError(var pos: Position, val msg: String) extends Throwable(msg)

  object ReificationError extends ReificationErrorExtractor {
    def unapply(error: ReificationError): Option[(Position, String)] = Some((error.pos, error.msg))
  }

  class UnexpectedReificationError(val pos: Position, val msg: String, val cause: Throwable = null) extends Throwable(msg, cause)

  object UnexpectedReificationError extends UnexpectedReificationErrorExtractor {
    def unapply(error: UnexpectedReificationError): Option[(Position, String, Throwable)] = Some((error.pos, error.msg, error.cause))
  }
}
