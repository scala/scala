/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Gilles Dubochet
 */

package scala.reflect.makro
package runtime

trait Reifiers {
  self: Context =>

  import mirror._
  import definitions._

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

  def reifyErasure(tpe: Type): Tree = {
    val positionBearer = enclosingMacros.find(c => c.macroApplication.pos != NoPosition).map(_.macroApplication).getOrElse(EmptyTree).asInstanceOf[Tree]
    val typetagInScope = callsiteTyper.context.withMacrosDisabled(callsiteTyper.resolveTypeTag(positionBearer, singleType(Reflect_mirror.owner.thisPrefix, Reflect_mirror), tpe, full = true))
    def typetagIsSynthetic(tree: Tree) = tree.isInstanceOf[Block] || (tree exists (sub => sub.symbol == TypeTagModule || sub.symbol == ConcreteTypeTagModule))
    typetagInScope match {
      case success if !success.isEmpty && !typetagIsSynthetic(success) =>
        val factory = TypeApply(Select(Ident(ClassTagModule), nme.apply), List(TypeTree(tpe)))
        Apply(factory, List(typetagInScope))
      case _ =>
        if (tpe.typeSymbol == ArrayClass) {
          val componentTpe = tpe.typeArguments(0)
          val componentTag = callsiteTyper.resolveClassTag(positionBearer, componentTpe)
          Select(componentTag, nme.wrap)
        } else {
          // [Eugene] what's the intended behavior? there's no spec on ClassManifests
          // for example, should we ban Array[T] or should we tag them with Array[AnyRef]?
          // if its the latter, what should be the result of tagging Array[T] where T <: Int?
          if (tpe.isSpliceable) throw new ReificationError(enclosingPosition, "tpe %s is an unresolved spliceable type".format(tpe))
          // [Eugene] imho this logic should be moved into `erasure`
          var erasure = tpe match {
            case tpe if tpe.typeSymbol.isDerivedValueClass => tpe // [Eugene to Martin] is this correct?
            case ConstantType(value) => tpe.widen.erasure
            case _ => {
              // [Eugene] magikz. needs review
              var result = tpe.erasure.normalize // necessary to deal with erasures of HK types, typeConstructor won't work
              result = result match {
                case PolyType(undets, underlying) => existentialAbstraction(undets, underlying) // we don't want undets in the result
                case _ => result
              }
              result
            }
          }
          val factory = TypeApply(Select(Ident(ClassTagModule), nme.apply), List(TypeTree(tpe)))
          Apply(factory, List(TypeApply(Select(Ident(PredefModule), nme.classOf), List(TypeTree(erasure)))))
        }
    }
 }

  def unreifyTree(tree: Tree): Tree =
    Select(tree, definitions.ExprEval)

  def reifyTopLevel(prefix: Tree, reifee: Any, dontSpliceAtTopLevel: Boolean = false, requireConcreteTypeTag: Boolean = false): Tree = {
    // [Eugene] the plumbing is not very pretty, but anyways factoring out the reifier seems like a necessary step to me
    import scala.reflect.reify._
    val reifier = mkReifier(mirror)(callsiteTyper, prefix, reifee, dontSpliceAtTopLevel, requireConcreteTypeTag)

    try {
      val result = reifier.reified
      logFreeVars(enclosingPosition, result)
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
