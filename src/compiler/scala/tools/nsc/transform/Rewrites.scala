package scala.tools.nsc
package transform

import scala.reflect.internal.util.{Position, RangePosition}
import scala.tools.nsc.Reporting.WarningCategory

abstract class Rewrites extends SubComponent {
  import global._

  val phaseName = "rewrites"

  def newPhase(prev: Phase): StdPhase = {
    if (!settings.Yrangepos) {
      runReporting.warning(NoPosition, "Enable `-Yrangepos` to use `-Yrewrites`", WarningCategory.Other, "")
      new StdPhase(prev) {
        override def apply(unit: global.CompilationUnit): Unit = ()
      }
    } else
      new RewritePhase(prev)
  }

  class RewritePhase(prev: Phase) extends StdPhase(prev) {
    override def apply(unit: CompilationUnit): Unit = {
      val rewriter = new RewriteTraverser()
      rewriter.traverse(unit.body)
      println(rewriter.patches)
    }
  }

  case class Patch(span: Position, replacement: String)

  lazy val breakOutSym = {
    import definitions._
    getMemberMethod(rootMirror.getPackageObject("scala.collection"), TermName("breakOut"))
  }

  def isInferredArg(tree: Tree) = tree match {
    case tt: TypeTree => tt.original eq null
    case _ =>
      val pos = tree.pos
      pos.isOffset && tree.forAll(t => {
        val tpos = t.pos
        tpos == NoPosition || tpos.isOffset && tpos.point == pos.point
      })
  }

  // Applied.unapply matches any tree, not just applications
  object Application {
    def unapply(t: Tree): Option[(Tree, List[Tree], List[List[Tree]])] = t match {
      case _: Apply | _: TypeApply =>
        val applied = treeInfo.dissectApplied(t)
        Some((applied.core, applied.targs, applied.argss))
      case _ => None
    }
  }

  class RewriteTraverser extends Traverser {
    val patches = collection.mutable.ArrayBuffer.empty[Patch]
    override def traverse(tree: Tree): Unit = tree match {
      case Application(fun, targs, argss) if fun.symbol == breakOutSym =>
        val inferredBreakOut = targs.forall(isInferredArg) && mforall(argss)(isInferredArg)
        if (inferredBreakOut)
          patches += Patch(Position.offset(tree.pos.source, fun.pos.end), targs.mkString("[", ", ", "]"))
      case _ => super.traverse(tree)
    }
  }
}
