/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * Copyright 2007 Google Inc. All Rights Reserved.
 * Author: bqe@google.com (Burak Emir)
 */
// $Id$

package scala.tools.nsc.matching

/** Translation of pattern matching
 *
 *  @author Burak Emir
 */
trait TransMatcher {
  self: transform.ExplicitOuter with PatternNodes with ParallelMatching with CodeFactory =>

  import global.{ typer => _, _ }
  import analyzer.Typer;
  import definitions._
  import symtab.Flags

  var cunit: CompilationUnit = _  // memory leak?
  var resultType: Type = _

  // cache these
  //final val settings_debug = settings.debug.value
  final val settings_squeeze = settings.Xsqueeze.value == "on"

  // check special case Seq(p1,...,pk,_*)
  protected def isRightIgnoring(p: ArrayValue): Boolean = {
    def isDefaultStar(tree: Tree): Boolean = tree match {
      case Bind(_, q)                 => isDefaultStar(q)
      case Star(q)                    => isDefaultPattern(q)
      case _                          => false
    }

    !p.elems.isEmpty && isDefaultStar(p.elems.last)
  }

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
    //DBG("****")
    //DBG("**** initalize, selector = "+selector+" selector.tpe = "+selector.tpe)
    //DBG("****    doCheckExhaustive == "+doCheckExhaustive)

    implicit val theOwner = owner
    implicit val rep = new RepFactory(handleOuter)
    val flags = if (doCheckExhaustive) Nil else List(Flags.TRANS_FLAG)

    def caseIsOk(c: CaseDef) = c match {
      case CaseDef(_: Apply, _, _)            => true
      case CaseDef(Ident(nme.WILDCARD), _, _) => true
      case _                                  => false
    }
    def doApply(fn: Tree): Boolean =
      (fn.symbol eq selector.tpe.decls.lookup(nme.CONSTRUCTOR)) &&
      (cases forall caseIsOk)

    def processApply(app: Apply): (List[Symbol], List[Tree], Tree) = {
      val Apply(fn, args) = app
      val (tmps, vds) = List.unzip(
        for ((ti, i) <- args.zipWithIndex) yield {
          // These type parameter vars were not being set as synthetic.
          // Temporarily noted on the off chance it was intentional.
          val v = newVar(ti.pos, cunit.fresh.newName(ti.pos, "tp"), selector.tpe.typeArgs(i), flags)
          (v, typedValDef(v, ti))
        }
      )
      (tmps, vds, ThrowMatchError(selector.pos, copy.Apply(app, fn, tmps map mkIdent)))
    }

    // sets temporaries, variable declarations, and the fail tree
    val (tmps, vds, theFailTree) = selector match {
      case app @ Apply(fn, _) if isTupleType(selector.tpe) && doApply(fn) =>
        processApply(app)
      case _ =>
        val root: Symbol      = newVar(selector.pos, selector.tpe, flags)
        val vdef: Tree        = typer.typed(ValDef(root, selector))
        val failTree: Tree    = ThrowMatchError(selector.pos, mkIdent(root))
        (List(root), List(vdef), failTree)
    }

    implicit val fail: Tree = theFailTree
    val irep = initRep(tmps, cases, rep)
    val mch = typer.typed(irep.toTree)
    var dfatree = typer.typed(Block(vds, mch))

    // cannot use squeezedBlock because of side-effects, see t275
    for ((cs, bx) <- cases.zipWithIndex)
      if (!rep.isReached(bx)) cunit.error(cs.body.pos, "unreachable code")

    dfatree = rep.cleanup(dfatree)
    resetTraverser.traverse(dfatree)
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
