package scala.tools.nsc
package transform

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
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
      val patches = ArrayBuffer[Patch]()

      val reparseUnit = new CompilationUnit(unit.source)
      reparseUnit.body = newUnitParser(reparseUnit).parse()
      val treeByRangePos = mutable.HashMap[Position, Tree]()
      reparseUnit.body.foreach {
        tree =>
          if (tree.pos.isRange && !tree.pos.isTransparent)
            treeByRangePos(tree.pos) = tree
      }

      val settings = global.settings
      val rewritesSetting = settings.Yrewrites
      if (rewritesSetting.contains(rewritesSetting.domain.breakOutArgs)) {
        val rewriter = new BreakoutTraverser()
        rewriter.traverse(unit.body)
        patches ++= rewriter.patches
      }
      if (rewritesSetting.contains(rewritesSetting.domain.collectionSeq)) {
        val rewriter = new CollectionSeqTraverser(treeByRangePos)
        rewriter.traverse(unit.body)
        patches ++= rewriter.patches
      }
      writePatches(unit.source, patches.toArray)
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

  class BreakoutTraverser extends Traverser {
    val patches = collection.mutable.ArrayBuffer.empty[Patch]
    override def traverse(tree: Tree): Unit = tree match {
      case Application(fun, targs, argss) if fun.symbol == breakOutSym =>
        val inferredBreakOut = targs.forall(isInferredArg) && mforall(argss)(isInferredArg)
        if (inferredBreakOut)
          patches += Patch(Position.offset(tree.pos.source, fun.pos.end), targs.mkString("[", ", ", "]"))
      case _ => super.traverse(tree)
    }
  }
  class CollectionSeqTraverser(treeByRangePos: collection.Map[Position, Tree]) extends Traverser {
    case class Rewrite(name: String, typeAlias: Symbol, termAlias: Symbol, cls: Symbol, module: Symbol)

    def rewrite(name: String) = Rewrite(name,
      definitions.ScalaPackage.packageObject.info.decl(TypeName(name)),
      definitions.ScalaPackage.packageObject.info.decl(TermName(name)),
      rootMirror.getRequiredClass("scala.collection." + name),
      rootMirror.getRequiredModule("scala.collection." + name))
    val rewrites = List(rewrite("Seq"), rewrite("IndexedSeq"))
    val patches = collection.mutable.ArrayBuffer.empty[Patch]
    override def traverse(tree: Tree): Unit = {
      tree match {
        case tt: TypeTree if tt.original != null =>
          val saved = tt.original.tpe
          tt.original.tpe = tt.tpe
          try traverse(tt.original)
          finally tt.original.setType(saved)
        case ref: RefTree =>
          for (rewrite <- rewrites) {
            val sym = ref.symbol
            if (sym == rewrite.cls || sym == rewrite.module || sym == rewrite.termAlias || sym == rewrite.typeAlias) {
              treeByRangePos.get(ref.pos) match {
                case Some(Ident(name)) if name.string_==(rewrite.name) =>
                  patches += Patch(ref.pos, "scala.collection." + rewrite.name)
                case _ =>
              }
            }
          }
        case _ =>
      }
      super.traverse(tree)
    }
  }
}
