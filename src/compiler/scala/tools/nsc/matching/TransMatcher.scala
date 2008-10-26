/* NSC -- new Scala compiler
 * Copyright 2005-2007 LAMP/EPFL
 * Copyright 2007 Google Inc. All Rights Reserved.
 * Author: bqe@google.com (Burak Emir)
 */
// $Id$

package scala.tools.nsc.matching

/** Translation of pattern matching
 *
 *  @author Burak Emir
 */
trait TransMatcher { self: transform.ExplicitOuter with PatternNodes with ParallelMatching with CodeFactory =>

  import global._
  import definitions._
  import posAssigner.atPos
  import symtab.Flags
  import typer.typed

  import collection.mutable.ListBuffer

  var cunit: CompilationUnit = _ // memory leak?
  def fresh = cunit.fresh
  var nPatterns = 0
  var resultType: Type = _

  // cache these
  final val settings_debug       = settings.debug.value
  final val settings_squeeze     = settings.Xsqueeze.value == "on"
  final val settings_casetags    = settings.Xcasetags.value == "on"

  // check special case Seq(p1,...,pk,_*)
  protected def isRightIgnoring(p: ArrayValue): Boolean = {
    def isDefaultStar(tree: Tree): Boolean = tree match {
      case Bind(_, q)                 => isDefaultStar(q)
      case Star(Ident(nme.WILDCARD))  => true
      case _                          => false
    }

    !p.elems.isEmpty && isDefaultStar(p.elems.last)
  }

  /** handles all translation of pattern matching
   */
  def handlePattern(selector: Tree, cases: List[CaseDef], doCheckExhaustive: Boolean, owner: Symbol, handleOuter: Tree => Tree): Tree = {
    implicit val theOwner = owner
    if (settings_debug) {
      Console.println("****")
      Console.println("**** initalize, selector = "+selector+" selector.tpe = "+selector.tpe)
      Console.println("****    doCheckExhaustive == "+doCheckExhaustive)
    }

    implicit val rep = new RepFactory(handleOuter)
    val tmps = new ListBuffer[Symbol]
    val vds  = new ListBuffer[Tree]
    var root:Symbol = newVar(selector.pos, selector.tpe)
    if (!doCheckExhaustive)
      root.setFlag(symtab.Flags.TRANS_FLAG)

    var vdef:Tree        = typed{ValDef(root, selector)}
    var theFailTree:Tree = ThrowMatchError(selector.pos, mkIdent(root))

    if (definitions.isTupleType(selector.tpe)) selector match {
      case app @ Apply(fn, args)
      if (fn.symbol eq selector.tpe.decls.lookup(nme.CONSTRUCTOR)) &&
      (cases forall { x => x match {
        case CaseDef(Apply(fn, pargs),_,_) => true ;
        case CaseDef(Ident(nme.WILDCARD),_,_) => true  ;
        case _ => false
      }}) =>
        var i = 0
        var as = args
        while(as ne Nil) {
          val ti = as.head
          val v = newVar(ti.pos, cunit.fresh.newName(ti.pos, "tp"), selector.tpe.typeArgs(i))
          if (!doCheckExhaustive)
            v.setFlag(symtab.Flags.TRANS_FLAG)
          vds  += typedValDef(v, ti)
          tmps += v
          i = i + 1
          as = as.tail
        }
      theFailTree = ThrowMatchError(selector.pos, copy.Apply(app, fn, tmps.toList map mkIdent))
      case _ =>
        tmps += root
        vds  += vdef
    } else {
      tmps += root
      vds  += vdef
    }
    val irep = initRep(tmps.toList, cases, rep)

    implicit val fail: Tree = theFailTree

    val mch  = typed{ repToTree(irep)}
    var dfatree = typed{Block(vds.toList, mch)}
    // cannot use squeezedBlock because of side-effects, see t275
    var bx = 0; var cs = cases; while(cs ne Nil) {
      if (!rep.isReached(bx)) {
        cunit.error(cs.head.asInstanceOf[CaseDef].body.pos, "unreachable code")
      }
      cs = cs.tail
      bx += 1
    }
    dfatree = rep.cleanup(dfatree)
    resetTrav.traverse(dfatree)
    dfatree
  }

  object resetTrav extends Traverser {
    override def traverse(x: Tree): Unit = x match {
      case vd @ ValDef(_, _, _, _) =>
        if (vd.symbol hasFlag symtab.Flags.SYNTHETIC) {
          vd.symbol resetFlag symtab.Flags.TRANS_FLAG
          vd.symbol resetFlag symtab.Flags.MUTABLE
        }
      case _ =>
        super.traverse(x)
    }
  }
}
