/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * Copyright 2007 Google Inc. All Rights Reserved.
 * Author: bqe@google.com (Burak Emir)
 */
// $Id$

package scala.tools.nsc.matching

import util.Position

/** Translation of pattern matching
 *
 *  @author Burak Emir
 */
trait TransMatcher extends ast.TreeDSL {
  self: transform.ExplicitOuter with PatternNodes with ParallelMatching with CodeFactory =>

  import global.{ typer => _, _ }
  import analyzer.Typer;
  import definitions._
  import symtab.Flags
  import CODE._

  var cunit: CompilationUnit = _  // memory leak?
  var resultType: Type = _

  def newName(pos: Position, s: String) = cunit.fresh.newName(pos, s)

  final val settings_squeeze = settings.Xsqueeze.value == "on"

  // check special case Seq(p1,...,pk,_*)
  protected def isRightIgnoring(p: ArrayValue): Boolean =
    !p.elems.isEmpty && cond(unbind(p.elems.last)) { case Star(q) => isDefaultPattern(q) }

  /** handles all translation of pattern matching
   */
  def handlePattern(
    selector: Tree,
    cases: List[CaseDef],
    doCheckExhaustive: Boolean,
    owner: Symbol,
    handleOuter: Tree => Tree)
    (implicit typer: Typer): Tree =
  {
    implicit val theOwner = owner
    implicit val rep = new RepFactory(handleOuter)
    val flags = if (doCheckExhaustive) Nil else List(Flags.TRANS_FLAG)

    def matchError(obj: Tree)   = atPos(selector.pos)(THROW(MatchErrorClass, obj))
    def caseIsOk(c: CaseDef)    = cond(c.pat) { case _: Apply | Ident(nme.WILDCARD) => true }

    def doApply(fn: Tree): Boolean =
      (fn.symbol eq (selector.tpe.decls lookup nme.CONSTRUCTOR)) &&
      (cases forall caseIsOk)

    def processTuple(app: Apply): (List[Symbol], List[Tree], Tree) = {
      val Apply(fn, args) = app
      val (tmps, vds) = List.unzip(
        for ((arg, typeArg) <- args zip selector.tpe.typeArgs) yield {
          val v = newVar(arg.pos, typeArg, flags, newName(arg.pos, "tp"))
          (v, typedValDef(v, arg))
        }
      )
      (tmps, vds, matchError(treeCopy.Apply(app, fn, tmps map ID)))
    }

    // sets temporaries, variable declarations, and the fail tree
    val (tmps, vds, theFailTree) = selector match {
      case app @ Apply(fn, _) if isTupleType(selector.tpe) && doApply(fn) =>
        processTuple(app)
      case _ =>
        val root: Symbol      = newVar(selector.pos, selector.tpe, flags)
        val vdef: Tree        = typer typed (VAL(root) === selector)
        val failTree: Tree    = matchError(ID(root))
        (List(root), List(vdef), failTree)
    }

    implicit val fail: Tree = theFailTree
    val irep = initRep(tmps, cases, rep)
    val mch = typer typed irep.toTree
    var dfatree = typer typed Block(vds, mch)

    // cannot use squeezedBlock because of side-effects, see t275
    for ((cs, bx) <- cases.zipWithIndex)
      if (!rep.isReached(bx)) cunit.error(cs.body.pos, "unreachable code")

    dfatree = rep cleanup dfatree
    resetTraverser traverse dfatree
    dfatree
  }

  private object resetTraverser extends Traverser {
    override def traverse(x: Tree): Unit = x match {
      case vd: ValDef =>
        if (vd.symbol hasFlag Flags.SYNTHETIC) {
          vd.symbol resetFlag (Flags.TRANS_FLAG | Flags.MUTABLE)
        }
      case _ =>
        super.traverse(x)
    }
  }
}
