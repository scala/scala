/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * Author: Paul Phillips
 */

package scala.tools.nsc
package matching

import transform.ExplicitOuter
import PartialFunction._

/** Traits which are mixed into MatchMatrix, but separated out as
 *  (somewhat) independent components to keep them on the sidelines.
 */
trait MatrixAdditions extends ast.TreeDSL {
  self: ExplicitOuter with ParallelMatching =>

  import global.{ typer => _, _ }
  import symtab.Flags
  import CODE._
  import Debug._
  import treeInfo._
  import definitions.{ isPrimitiveValueClass }

  /** The Squeezer, responsible for all the squeezing.
   */
  private[matching] trait Squeezer {
    self: MatrixContext =>

    private val settings_squeeze = !settings.Ynosqueeze.value

    class RefTraverser(vd: ValDef) extends Traverser {
      private val targetSymbol = vd.symbol
      private var safeRefs     = 0
      private var isSafe       = true

      def canDrop   = isSafe && safeRefs == 0
      def canInline = isSafe && safeRefs == 1

      override def traverse(tree: Tree): Unit = tree match {
        case t: Ident if t.symbol eq targetSymbol =>
          // target symbol's owner should match currentOwner
          if (targetSymbol.owner == currentOwner) safeRefs += 1
          else isSafe = false

        case LabelDef(_, params, rhs) =>
          if (params exists (_.symbol eq targetSymbol))  // cannot substitute this one
            isSafe = false

          traverse(rhs)
        case _ if safeRefs > 1 => ()
        case _ =>
          super.traverse(tree)
      }
    }

    /** Compresses multiple Blocks. */
    private def combineBlocks(stats: List[Tree], expr: Tree): Tree = expr match {
      case Block(stats1, expr1) if stats.isEmpty => combineBlocks(stats1, expr1)
      case _                                     => Block(stats, expr)
    }
    def squeezedBlock(vds: List[Tree], exp: Tree): Tree =
      if (settings_squeeze) combineBlocks(Nil, squeezedBlock1(vds, exp))
      else                  combineBlocks(vds, exp)

    private def squeezedBlock1(vds: List[Tree], exp: Tree): Tree = {
      lazy val squeezedTail = squeezedBlock(vds.tail, exp)
      def default = squeezedTail match {
        case Block(vds2, exp2) => Block(vds.head :: vds2, exp2)
        case exp2              => Block(vds.head :: Nil,  exp2)
      }

      if (vds.isEmpty) exp
      else vds.head match {
        case vd: ValDef =>
          val rt = new RefTraverser(vd)
          rt.atOwner(owner)(rt traverse squeezedTail)

          if (rt.canDrop)
            squeezedTail
          else if (isConstantType(vd.symbol.tpe) || rt.canInline)
            new TreeSubstituter(List(vd.symbol), List(vd.rhs)) transform squeezedTail
          else
            default
        case _ => default
      }
    }
  }

  /** The Optimizer, responsible for some of the optimizing.
   */
  private[matching] trait MatchMatrixOptimizer {
    self: MatchMatrix =>

    import self.context._

    final def optimize(tree: Tree): Tree = {
      // Uses treeInfo extractors rather than looking at trees directly
      // because the many Blocks obscure our vision.
      object lxtt extends Transformer {
        override def transform(tree: Tree): Tree = tree match {
          case Block(stats, ld @ LabelDef(_, _, body)) if targets exists (_ shouldInline ld.symbol) =>
            squeezedBlock(transformStats(stats, currentOwner), body)
          case IsIf(cond, IsTrue(), IsFalse()) =>
            transform(cond)
          case IsIf(cond1, IsIf(cond2, thenp, elsep1), elsep2) if elsep1 equalsStructure elsep2 =>
            transform(typer typed If(gen.mkAnd(cond1, cond2), thenp, elsep2))
          case If(cond1, IsIf(cond2, thenp, Apply(jmp, Nil)), ld: LabelDef) if jmp.symbol eq ld.symbol =>
            transform(typer typed If(gen.mkAnd(cond1, cond2), thenp, ld))
          case _ =>
            super.transform(tree)
        }
      }
      try lxtt transform tree
      finally clearSyntheticSyms()
    }
  }

  /** The Exhauster.
   */
  private[matching] trait MatrixExhaustiveness {
    self: MatchMatrix =>

    import self.context._

    /** Exhaustiveness checking requires looking for sealed classes
     *  and if found, making sure all children are covered by a pattern.
     */
    class ExhaustivenessChecker(rep: Rep, matchPos: Position) {
      val Rep(tvars, rows) = rep

      import Flags.{ MUTABLE, ABSTRACT, SEALED }

      private case class Combo(index: Int, sym: Symbol) { }

      /* True if the patterns in 'row' cover the given type symbol combination, and has no guard. */
      private def rowCoversCombo(row: Row, combos: List[Combo]) =
        row.guard.isEmpty && combos.forall(c => row.pats(c.index) covers c.sym)

      private def requiresExhaustive(sym: Symbol) = {
         (sym.isMutable) &&                 // indicates that have not yet checked exhaustivity
        !(sym hasFlag NO_EXHAUSTIVE) &&     // indicates @unchecked
         (sym.tpe.typeSymbol.isSealed) &&
        !isPrimitiveValueClass(sym.tpe.typeSymbol)   // make sure it's not a primitive, else (5: Byte) match { case 5 => ... } sees no Byte
      }

      private lazy val inexhaustives: List[List[Combo]] = {
        // let's please not get too clever side-effecting the mutable flag.
        val toCollect = tvars.zipWithIndex filter { case (pv, i) => requiresExhaustive(pv.sym) }
        val collected = toCollect map { case (pv, i) =>
          // okay, now reset the flag
          pv.sym resetFlag MUTABLE

          i -> (
            pv.tpe.typeSymbol.sealedDescendants.toList sortBy (_.sealedSortName)
            // symbols which are both sealed and abstract need not be covered themselves, because
            // all of their children must be and they cannot otherwise be created.
            filterNot (x => x.isSealed && x.isAbstractClass && !isPrimitiveValueClass(x))
            // have to filter out children which cannot match: see ticket #3683 for an example
            filter (_.tpe matchesPattern pv.tpe)
          )
        }

        val folded =
          collected.foldRight(List[List[Combo]]())((c, xs) => {
            val (i, syms) = c match { case (i, set) => (i, set.toList) }
            xs match {
              case Nil  => syms map (s => List(Combo(i, s)))
              case _    => for (s <- syms ; rest <- xs) yield Combo(i, s) :: rest
            }
          })

        folded filterNot (combo => rows exists (r => rowCoversCombo(r, combo)))
      }

      private def mkPad(xs: List[Combo], i: Int): String = xs match {
        case Nil                    => pad("*")
        case Combo(j, sym) :: rest  => if (j == i) pad(sym.name.toString) else mkPad(rest, i)
      }
      private def mkMissingStr(open: List[Combo]) =
        "missing combination %s\n" format tvars.indices.map(mkPad(open, _)).mkString

      /** The only public method. */
      def check = {
        def errMsg = (inexhaustives map mkMissingStr).mkString
        if (inexhaustives.nonEmpty)
          cunit.warning(matchPos, "match is not exhaustive!\n" + errMsg)

        rep
      }
    }
  }
}