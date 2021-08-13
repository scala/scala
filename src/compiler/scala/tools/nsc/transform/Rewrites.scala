package scala.tools.nsc
package transform

import scala.annotation.tailrec
import scala.reflect.internal.util.{Position, SourceFile}
import scala.tools.nsc.Reporting.WarningCategory

abstract class Rewrites extends SubComponent {
  import global._

  val phaseName = "rewrites"

  def newPhase(prev: Phase): StdPhase = {
    if (settings.Yrewrites.value.isEmpty) {
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
      writePatches(unit.source, rewriter.patches.toArray)
    }
  }

  case class Patch(span: Position, replacement: String) {
    def delta: Int = replacement.length - (span.end - span.start)
  }

  def checkNoOverlap(patches: Array[Patch]): Boolean = {
    var ok = true
    if (patches.nonEmpty)
      patches.reduceLeft { (p1, p2) =>
        if (p1.span.end > p2.span.start) {
          ok = false
          runReporting.warning(NoPosition, s"overlapping patches:\n - $p1\n - $p2", WarningCategory.Other, "")
        }
        p2
      }
    ok
  }

  def applyPatches(source: SourceFile, patches: Array[Patch]): String = {
    val sourceChars = source.content
    val patchedChars = new Array[Char](sourceChars.length + patches.foldLeft(0)(_ + _.delta))

    @tailrec def loop(pIdx: Int, inIdx: Int, outIdx: Int): Unit = {
      def copy(upTo: Int): Int = {
        val untouched = upTo - inIdx
        System.arraycopy(sourceChars, inIdx, patchedChars, outIdx, untouched)
        outIdx + untouched
      }
      if (pIdx < patches.length) {
        val p = patches(pIdx)
        val outNew = copy(p.span.start)
        p.replacement.copyToArray(patchedChars, outNew)
        loop(pIdx + 1, p.span.end, outNew + p.replacement.length)
      } else {
        val outNew = copy(sourceChars.length)
        assert(outNew == patchedChars.length, s"$outNew != ${patchedChars.length}")
      }
    }
    loop(0, 0, 0)
    new String(patchedChars)
  }

  def writePatches(source: SourceFile, patches: Array[Patch]): Unit = if (patches.nonEmpty) {
    java.util.Arrays.sort(patches, Ordering.by[Patch, Int](_.span.start))
    if (checkNoOverlap(patches)) {
      val bytes = applyPatches(source, patches).getBytes(settings.encoding.value)
      val out = source.file.output
      out.write(bytes)
      out.close()
    }
  }

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
