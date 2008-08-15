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

  final def hasRegularPattern(pats1: List[Tree]): Boolean = {
    var pats = pats1; while(pats ne Nil) {
      if(isRegularPattern(pats.head)) { return true; } else { pats = pats.tail }
    }
    return false
  }

  final def isRegularPattern(pat: Tree): Boolean = {
    pat match {
      case Alternative(trees)    => hasRegularPattern(trees)
      case Star(t)               => true
      case Ident(_)              => false
      case Bind(n, pat1)         => isRegularPattern(pat1)
      case Sequence(trees)       => true // cause there are ArrayValues now
      case ArrayValue(tt, trees) => hasRegularPattern(trees)
      case Apply(fn, trees)      => hasRegularPattern(trees)
      case Literal(_)            => false
      case Select(_, _)          => false
      case Typed(_, _)           => false
      case UnApply(_,trees)      => hasRegularPattern(trees)
    }
  }

  // @todo: this should be isNotRegular :-/ premature opt src of all evil
  // check special case Seq(p1,...,pk,_*) where pi not regular
  protected def isRightIgnoring(p: ArrayValue): Boolean = {
    def isDefaultStar(p: Tree): Boolean = p match {
      case Bind(_, q)                 => isDefaultStar(q)
      case Star(Ident(nme.WILDCARD))  => true
      case _                          => false
    }
    p match {
      case ArrayValue(s, trees) =>
        var ts = trees
	var c: Tree = null
	while ((ts ne Nil) && {c = ts.head; ts = ts.tail; !isRegularPattern(c)}) {}
	(ts eq Nil) && isDefaultStar(c)
    }
  }

  /** a casedef with sequence subpatterns like
   *
   *  case ..x @ ().. => body
   *
   * should be replaced straight away with
   *
   *  case    .. () .. => val x = Nil; body
   */
  def isRegular(pats: List[CaseDef]): (List[CaseDef],Boolean) = {
    var existsReg = false
    var isReg = false
    var nilVars: List[Symbol] = null

    def isRegular1(pat: Tree): Tree = pat match {
      case Alternative(trees) =>
        copy.Alternative(pat, trees map { isRegular1 })

      case Star(t) =>
        isReg = true; copy.Star(pat, isRegular1(t))

      case Ident(_) =>
        pat

      case Bind(id, empt @ Sequence(List())) =>
        nilVars = pat.symbol :: nilVars
        empt

      case Bind(n, pat1) =>
        copy.Bind(pat, n, isRegular1(pat1))

      case Sequence(trees) =>
        isReg = true
        copy.Sequence(pat, trees map { isRegular1 })

      case UnApply(fn, args) => copy.UnApply(pat, fn, args map { isRegular1 })

      case app @ Apply(fn, List(pat2@ ArrayValue( tt, List(b @ Bind(id, Star(wc @ Ident(nme.WILDCARD))))))) if (app.tpe.typeSymbol.flags & Flags.CASE) == 0 =>
        val tpe1:Type = pat2.tpe.widen.baseType( definitions.SeqClass ).typeArgs(0)
	val tpe = appliedType(definitions.SeqClass.typeConstructor, List(tpe1))
	b.symbol.setInfo(tpe)
	b.setType(tpe)
        copy.Bind(b, id, wc)

      case app @ Apply(fn, List(pat2@ ArrayValue( tt, List(b @ Bind(id, Star(wc @ Ident(nme.WILDCARD))))))) =>  // a pattern of the form MyCaseConstructor(foo@_*)
        val tpe1:Type = pat2.tpe.widen.baseType( definitions.SeqClass ).typeArgs(0)
	val tpe = appliedType(definitions.SeqClass.typeConstructor, List(tpe1))
	b.symbol.setInfo(tpe)
	b.setType(tpe)
        copy.Apply(pat, fn, List(copy.Bind(b, id, wc)))

      case av @ ArrayValue(s, trees) =>
        if (isRightIgnoring(av)) pat
        else copy.ArrayValue(pat, s, (trees map { isRegular1 }))

      case Apply(fn, List(Sequence(List()))) =>
        pat

      case Apply(fn, trees) =>
        copy.Apply(pat, fn, (trees map { isRegular1 }))

      case Literal(_) =>
        pat

      case Select(_, _) =>
        pat

      case Typed(_, _) =>
        pat

      case This(_) =>
        val stpe = mkThisType(pat.tpe.typeSymbol)
        Typed(Ident(nme.WILDCARD) setType stpe, TypeTree(stpe))
    }

    var res = new ListBuffer[CaseDef]
    val it = pats.elements; while (it.hasNext) {
      nilVars = Nil
      val cdef = it.next
      val newt = isRegular1(cdef.pat)
      existsReg = existsReg || isReg

      val nbody = if (nilVars.isEmpty) cdef.body else
        atPos(cdef.body.pos)(
          Block(nilVars map {
            x => ValDef(x, Ident(definitions.NilModule))
          }, cdef.body)
        )

      res += copy.CaseDef(cdef, newt, cdef.guard, nbody)
    }
    (res.toList, existsReg)
  }

  /** handles all translation of pattern matching
   */
  def handlePattern(selector: Tree, ocases: List[CaseDef], doCheckExhaustive: Boolean, owner: Symbol, handleOuter: Tree => Tree): Tree = {
    val (cases, containsReg) = isRegular(ocases)
    // @todo: remove unused variables
    if (containsReg) {
      cunit.error(selector.pos, "regular expressions not yet implemented")
      //sel
      EmptyTree
    } else {
      implicit val theOwner = owner
      if (settings_debug) {
        Console.println("****")
        Console.println("**** initalize, selector = "+selector+" selector.tpe = "+selector.tpe)
        Console.println("****    doCheckExhaustive == "+doCheckExhaustive)
      }

      implicit val rep = new RepFactory(handleOuter)
      try {
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
        //DEBUG("**** finished\n"+dfatree.toString)
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
      } catch {
        case ex: FatalError => ex.printStackTrace(); throw ex
      }
    }
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
