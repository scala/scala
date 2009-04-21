/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * Copyright 2007 Google Inc. All Rights Reserved.
 * Author: bqe@google.com (Burak Emir)
 */
// $Id$

package scala.tools.nsc.matching

import util.Position
import collection._
import mutable.BitSet
import immutable.IntMap
import MatchUtil.ListPlus._
import MatchUtil.Implicits._
import MatchUtil._

/** Translation of match expressions.
 *
 *  `p':  pattern
 *  `g':  guard
 *  `bx': body index
 *
 *   internal representation is (temp:List[Symbol], row:List[Row])
 *
 *         tmp1      tmp_n
 *    Row( p_11  ...  p_1n   g_1  b_1 ) + subst
 *
 *    Row( p_m1  ...  p_mn   g_m  b_m ) + subst
 *
 *  @author Burak Emir
 */
trait ParallelMatching {
  self: transform.ExplicitOuter with PatternNodes with CodeFactory =>

  import global.{typer => _, _}
  import analyzer.Typer;
  import symtab.Flags
  import Types._
  import Code.{ fn, Const }

  /**
   * Encapsulates a symbol being matched on.
   *
   * sym match { ... }
   *
   * results in Scrutinee(sym).
   *
   * Note that we only ever match on Symbols, not Trees: a temporary variable
   * is created for any expressions being matched on.
   */
  case class Scrutinee(val sym: Symbol) {
    import definitions._

    def mkList(xs: List[Symbol]) = sym :: xs
    def mkList(prefix: List[Symbol], suffix: List[Symbol]) = prefix ::: sym :: suffix
    def isDefined = sym ne NoSymbol
    def accessors = sym.caseFieldAccessors
    def id = mkIdent(sym)
    def tpe = sym.tpe
    def pos = sym.pos
    def isChar = tpe.widen.isChar
    def isSimple = isChar || tpe.widen.isInt
    lazy val sequenceType = tpe.widen.baseType(SeqClass)
    lazy val elementType = sequenceType match {
      case NoType => Predef.error("arg " + tpe + " not subtype of Seq[A]")
      case _      => tpe.typeArgs(0)
    }
    // for propagating "unchecked" to synthetic vars
    def flags: List[Long] = if (sym hasFlag Flags.TRANS_FLAG) List(Flags.TRANS_FLAG) else Nil

    def assertIsSubtype(other: Type) = assert(isSubType(tpe, other), "problem "+tpe+" not <: "+other)
    def casted(headType: Type)(implicit theOwner: Symbol) =
      if (tpe =:= headType) this
      else new Scrutinee(newVar(pos, headType, flags))
  }

  case class Pattern(tree: Tree) {
    implicit def mkPattern(t: Tree) = Pattern(t)
    import definitions._
    lazy val    sym = tree.symbol
    lazy val    tpe = tree.tpe
    lazy val symtpe = sym.tpe
    lazy val prefix = tpe.prefix
    lazy val stripped   = strip2
    lazy val tpeIfHead  = stripped.tree match {
      case p @ (_:Ident | _:Select) => singleType(stripped.prefix, stripped.sym) //should be singleton object
      case __UnApply(_,argtpe,_)    => argtpe
      case _                        => tpe
    }

    /**
     * Can this pattern be part of a switch statement?
     */
    lazy val isSimpleSwitchCandidate = stripped.tree match {
      case Literal(const : Constant) if isNumeric(const.tag) =>
        const.tag match {
          case FloatTag | DoubleTag | LongTag => false
          case _ => true
        }
      case _ => false
    }

    /** returns if pattern can be considered a no-op test ??for expected type?? */
    final def isDefault: Boolean = isDefaultPattern(tree);

    final def isEquals = tpe match {
      case TypeRef(_, sym, _) => sym eq EqualsPatternClass
      case _                  => false
    }

    final def isAlternative: Boolean = tree match {
      case Bind(_, t)         => t.isAlternative
      case _: Alternative     => true
      case _                  => false
    }

    final def getAlternativeBranches: List[Tree] = {
      def get_BIND(pctx: Tree => Tree, p: Tree): List[Tree] = p match {
        case b @ Bind(n, p)   => get_BIND((x: Tree) => pctx(copy.Bind(b, n, x) setType x.tpe), p)
        case Alternative(ps)  => ps map pctx
      }
      get_BIND(x => x, tree)
    }

    /** returns all variables that are binding the given pattern
     *  @param   x a pattern
     *  @return  vs variables bound, p pattern proper
     */
    final def strip: (immutable.Set[Symbol], Pattern) = tree match {
      case b @ Bind(_, t)   => val (vs, p) = Pattern(t).strip ; (vs + b.symbol, p)
      case _                => (emptySymbolSet, this)
    }
    final def strip1: Set[Symbol] = strip._1
    final def strip2: Pattern     = strip._2

    final def definedVars: List[Symbol] = ParallelMatching.this.definedVars(tree)

    /** returns true if pattern tests an object */
    final def isObjectTest(head: Type) =
      (sym ne null) && (sym != NoSymbol) && prefix.isStable && (head =:= singleType(prefix, sym))
  }

  case class Patterns(scrut: Scrutinee, ps: List[Pattern]) {
    private lazy val column = ps.map(_.tree)
    lazy val head = ps.head
    lazy val tail = Patterns(scrut, ps.tail)
    lazy val last = ps.last
    lazy val headType = head.tpeIfHead
    lazy val isCaseHead = headType.isCaseClass
    lazy val dummies = if (isCaseHead) getDummies(headType.typeSymbol.caseFieldAccessors.length) else Nil
    lazy val size = ps.length

    def apply(i: Int): Tree = ps(i).tree
    def zip() = column.zipWithIndex
    def zip[T](ys: List[T]) = column.zip(ys)

    def isObjectTest(pat: Pattern)  = pat.isObjectTest(headType)
    def isObjectTest(pat: Tree)     = Pattern(pat).isObjectTest(headType)
    // an unapply for which we don't need a type test
    def isUnapplyHead = head.tree match {
      case __UnApply(_,argtpe,_)  => scrut.tpe <:< argtpe
      case _                      => false
    }

    def isSimpleSwitch: Boolean =
      scrut.isSimple && ps.init.forall(_.isSimpleSwitchCandidate) &&
      // TODO: This needs to also allow the case that the last is a compatible type pattern.
      (last.isSimpleSwitchCandidate || last.isDefault)

    def mkRule(rest: Rep)(implicit rep: RepFactory): RuleApplication = head match {
      case x if x.isEquals          => new MixEquals(this, rest)
      case Pattern(x: ArrayValue)   => if (isRightIgnoring(x)) new MixSequenceStar(this, rest)
                                       else new MixSequence(this, rest)
      case _ if isSimpleSwitch      => new MixLiterals(this, rest)
      case _ if isUnapplyHead       => new MixUnapply(this, rest)
      case _                        => new MixTypes(this, rest)
    }
  }

  /**
   * Class encapsulating a guard expression in a pattern match:
   *   case ... if(tree) => ...
   */
  case class Guard(tree: Tree) {
    def isEmpty = tree eq EmptyTree
    def duplicate = Guard(tree.duplicate)

    def mkIf(thenPart: Tree, elsePart: Tree) = If(tree.duplicate, thenPart, elsePart)
  }
  val NoGuard = Guard(EmptyTree)

  /** picks which rewrite rule to apply
   *  @precondition: column does not contain alternatives (ensured by initRep)
   */
  def MixtureRule(scrut: Scrutinee, column: List[Tree], rest: Rep)(implicit rep: RepFactory): RuleApplication =
    Patterns(scrut, column map Pattern) mkRule rest

  sealed abstract class RuleApplication(rep: RepFactory) {
    def scrut: Scrutinee
    implicit def typer = rep.typer

    // used in MixEquals and MixSequence
    final protected def repWithoutHead(pats: Patterns, rest: Rep)(implicit theOwner: Symbol): Rep = {
      val nfailrow = List.map2(pats.tail.ps, rest.row.tail)((p, r) => r.insert(p))
      rep.make(scrut.mkList(rest.temp), nfailrow)
    }

    /** translate outcome of the rule application into code (possible involving recursive application of rewriting) */
    def tree(implicit theOwner: Symbol, failTree: Tree): Tree
  }

  case class ErrorRule(implicit rep:RepFactory) extends RuleApplication(rep) {
    def scrut: Scrutinee = impossible
    final def tree(implicit theOwner: Symbol, failTree: Tree) = failTree
  }

  /**  {case ... if guard => bx} else {guardedRest} */
  case class VariableRule(subst: Bindings, guard: Guard, guardedRest: Rep, bx: Int)(implicit rep:RepFactory) extends RuleApplication(rep) {
    def scrut: Scrutinee = impossible
    final def tree(implicit theOwner: Symbol, failTree: Tree): Tree = {
      val body = typer.typed { rep.requestBody(bx, subst) }
      lazy val vdefs = subst.targetParams
      lazy val typedElse = guardedRest.toTree
      lazy val typedIf = typer.typed(guard.mkIf(body, typedElse))

      if (guard.isEmpty) body
      else typer.typed(squeezedBlock(vdefs, typedIf))
    }
  }

  /** mixture rule for literals
   */
  class MixLiterals(val pats: Patterns, val rest:Rep)(implicit rep:RepFactory) extends RuleApplication(rep) {
    // e.g. (1,1) (1,3) (42,2) for column { case ..1.. => ;; case ..42..=> ;; case ..1.. => }
    var defaultV: immutable.Set[Symbol] = emptySymbolSet
    var defaultIndexSet = new BitSet(pats.size)

    def insertDefault(tag: Int, vs: Set[Symbol]) {
      defaultIndexSet += tag
      defaultV = defaultV ++ vs
    }

    def haveDefault: Boolean          = !defaultIndexSet.isEmpty
    def defaultRows: List[Row]   = defaultIndexSet.toList reverseMap grabRow

    protected var tagIndices = IntMap.empty[List[Int]]
    protected def grabRow(index: Int): Row = {
      val r = rest.row(index)
      if (defaultV.isEmpty) r
      else r.insert2(Nil, strip1(pats(index)), scrut.sym)  // get vars
    }

    /** inserts row indices using in to list of tagIndices */
    protected def tagIndicesToReps(implicit theOwner: Symbol) : List[(Int, Rep)] =
      tagIndices map { case (k, v) => (k, rep.make(rest.temp, v.reverseMap(grabRow) ::: defaultRows)) } toList

    protected def defaultsToRep(implicit theOwner: Symbol) = rep.make(rest.temp, defaultRows)

    protected def insertTagIndexPair(tag: Int, index: Int) =
      tagIndices = tagIndices.update(tag, index :: tagIndices.getOrElse(tag, Nil))

    /** returns
     *  @return list of continuations,
     *  @return variables bound to default continuation,
     *  @return optionally, a default continuation
     **/
    def getTransition(implicit theOwner: Symbol): (List[(Int,Rep)], Set[Symbol], Option[Rep]) =
      (tagIndicesToReps, defaultV, if (haveDefault) Some(defaultsToRep) else None)


    val Patterns(scrut, patterns) = pats

    val varMap: List[(Int, List[Symbol])] =
      (for ((x, i) <- pats.zip) yield strip2(x) match {
        case p @ Const(c: Int)        => insertTagIndexPair(c, i)       ; Some(c,       definedVars(x))
        case p @ Const(c: Char)       => insertTagIndexPair(c.toInt, i) ; Some(c.toInt, definedVars(x))
        case p if isDefaultPattern(p) => insertDefault(i, strip1(x))    ; None
      }) . flatMap(x => x) . reverse

    // lazy
    private def bindVars(Tag: Int, orig: Bindings): Bindings  = {
      def myBindVars(rest:List[(Int,List[Symbol])], bnd: Bindings): Bindings  = rest match {
        case Nil => bnd
        case (Tag,vs)::xs => myBindVars(xs, bnd.add(vs, scrut.sym))
        case (_,  vs)::xs => myBindVars(xs, bnd)
      }
      myBindVars(varMap, orig)
    }

    final def tree(implicit theOwner: Symbol, failTree: Tree): Tree = {
      val (branches, defaultV, defaultRep) = this.getTransition // tag body pairs
      val cases = for ((tag, r) <- branches) yield {
        val r2 = rep.make(r.temp, r.row.map(x => x.rebind(bindVars(tag, x.subst))))
        CaseDef(Literal(tag), EmptyTree, r2.toTree)
      }
      lazy val ndefault = defaultRep.map(_.toTree) getOrElse failTree
      lazy val casesWithDefault = cases ::: List(CaseDef(mk_(definitions.IntClass.tpe), EmptyTree, ndefault))

      cases match {
        case CaseDef(lit,_,body) :: Nil   => If(Equals(scrut.id, lit), body, ndefault)
        case _ if scrut.isChar            => Match(Select(scrut.id, nme.toInt), casesWithDefault) // chars to ints
        case _                            => Match(scrut.id, casesWithDefault)
      }
    }
  }

  /** mixture rule for unapply pattern
   */
  class MixUnapply(val pats: Patterns, val rest: Rep)(implicit rep: RepFactory)
  extends RuleApplication(rep) {
    val Patterns(scrut, patterns) = pats
    val Strip(vs, unapp) = pats.head.tree
    lazy val ua @ UnApply(app @ Apply(fxn, appargs), args) = unapp

    def newVarCapture(pos: Position,tpe: Type)(implicit theOwner:Symbol) = newVar(pos, tpe, scrut.flags)

    /** returns (unapply-call, success-rep, optional fail-rep*/
    final def getTransition(implicit theOwner: Symbol): (Tree, List[Tree], Rep, Option[Rep]) = {
      object sameUnapplyCall {
        def unapply(t: Tree) = t match {
          case UnApply(Apply(fn1,_), differentArgs) if (fxn.symbol == fn1.symbol) && fxn.equalsStructure(fn1) =>
            Some(differentArgs)
          case _ =>
            None
        }
      }
      val ures = newVarCapture(ua.pos, app.tpe)
      val rhs = Apply(fxn, scrut.id :: appargs.tail) setType ures.tpe
      val uacall = typedValDef(ures, rhs)
      val zipped = pats.zip(rest.row)
      val nrowsOther = zipped.tail flatMap
        { case (Strip2(sameUnapplyCall(_)), _) => Nil ; case (pat, r) => List(r.insert(pat)) }

      val nrepFail =
        if (nrowsOther.isEmpty) None
        else Some(rep.make(scrut.mkList(rest.temp), nrowsOther))

      def mkTransition(vdefs: List[Tree], ntemps: List[Symbol], nrows: List[Row]) =
        (uacall, vdefs, rep.make(scrut.mkList(ntemps, rest.temp), nrows), nrepFail)

      // Second argument is number of dummies to prepend in the default case
      def mkNewRows(sameFilter: (List[Tree]) => List[Tree], dum: Int) =
        for ((pat @ Strip(vs, p), r) <- zipped) yield p match {
          case sameUnapplyCall(args)  => r.insert2(sameFilter(args) ::: List(EmptyTree), vs, scrut.sym)
          case _                      => r.insert(getDummies(dum) ::: List(pat))
        }

      args.length match {
        case 0  => // special case for unapply(), app.tpe is boolean
          mkTransition(Nil, Nil, mkNewRows((xs) => Nil, 0))

        case 1 => // special case for unapply(p), app.tpe is Option[T]
          val vtpe = app.tpe.typeArgs(0)
          val vsym = newVarCapture(ua.pos, vtpe)
          val nrows = mkNewRows((xs) => List(xs.head), 1)
          val vdef = typedValDef(vsym, Get(mkIdent(ures)))
          mkTransition(List(vdef), List(vsym), nrows)

        case _ => // app.tpe is Option[? <: ProductN[T1,...,Tn]]
          val uresGet = newVarCapture(ua.pos, app.tpe.typeArgs(0))
          val vdefHead = typedValDef(uresGet, Get(mkIdent(ures)))
          val ts = definitions.getProductArgs(uresGet.tpe).get
          val nrows = mkNewRows(identity, ts.size)
          val (vdefs: List[Tree], vsyms: List[Symbol]) = List.unzip(
            for ((vtpe, i) <- ts.zip((1 to ts.size).toList)) yield {
              val vchild = newVarCapture(ua.pos, vtpe)
              val accSym = definitions.productProj(uresGet, i)
              val rhs = typer.typed(Code.fn(mkIdent(uresGet), accSym))

              (typedValDef(vchild, rhs), vchild)
            })
          mkTransition(vdefHead :: vdefs, vsyms, nrows)
      }
    } /* def getTransition(...) */

    final def tree(implicit theOwner: Symbol, failTree: Tree) = {
      val (uacall, vdefs, srep, frep) = this.getTransition
      val succ = srep.toTree
      val fail = frep.map(_.toTree) getOrElse failTree
      val cond =
        if (uacall.symbol.tpe.isBoolean) typer.typed(mkIdent(uacall.symbol))
        else nonEmptinessCheck(uacall.symbol)

      typer.typed( squeezedBlock(List(rep.handleOuter(uacall)), If(cond,squeezedBlock(vdefs,succ),fail)) )
    }
  }

  /** handle sequence pattern and ArrayValue (but not star patterns)
   */
  sealed class MixSequence(val pats: Patterns, val rest: Rep)(implicit rep: RepFactory) extends RuleApplication(rep) {
    val Patterns(scrut, patterns) = pats

    final def removeStar(xs: List[Tree]): List[Tree] =
      xs.init ::: makeBind(strip1(xs.last).toList, mk_(scrut.sequenceType)) :: Nil

    protected def getSubPatterns(len:Int, x:Tree):Option[List[Tree]] = x match {
      case av @ ArrayValue(_,xs) if (!isRightIgnoring(av) && xs.length == len)   => Some(xs ::: List(EmptyTree))
      case av @ ArrayValue(_,xs) if ( isRightIgnoring(av) && xs.length == len+1) => Some(removeStar(xs)) // (*)
      case EmptyTree | Ident(nme.WILDCARD)                                       => Some(getDummies(len+1))
      case _                                                                     => None
    }

    protected def makeSuccRep(vs: List[Symbol], tail: Symbol, nrows: List[Row])(implicit theOwner: Symbol) =
      rep.make(vs ::: tail :: rest.temp, nrows)

    /** True if x must be checked even if y failed to match after passing its length test
      * (the conditional supplied by getCond)
      */
    protected def mustCheck(x:Tree, y:Tree): Boolean =
      isDefaultPattern(x) || ((x, y) match {
        case (av @ ArrayValue(_,xs), bv @ ArrayValue(_,ys)) =>
          // if they are both right-ignoring, x must be more general if it has fewer literals - bug #889
          if (isRightIgnoring(av) && isRightIgnoring(bv) && xs.length < ys.length) true
          // otherwise, x is more general only if it is y plus a star
          else isRightIgnoring(av) && !isRightIgnoring(bv) && xs.length == ys.length+1                   // see (*)
        case _ =>
          false
      })
    // context (to be used in IF), success and failure Rep
    def getTransition(implicit theOwner: Symbol): (Tree => Tree => Tree, Rep, Rep) = {
      scrut.assertIsSubtype(pats.head.tpe)
      val treeAsSeq = scrut.id                // scrut.tpe <:< column.head.tpe confirmed by assertion
      val av @ ArrayValue(_, xs) = pats.head.tree
      val ys = if (isRightIgnoring(av)) xs.init else xs
      val vs = ys map(y => newVar(strip2(y).pos, scrut.elementType))

      lazy val tail = newVar(scrut.pos, scrut.sequenceType)
      lazy val lastBinding = if (ys.size > 0) seqDrop(treeAsSeq.duplicate, ys.size) else scrut.id
      val bindings =
        (for ((v, i) <- vs.zipWithIndex) yield typedValDef(v, seqElement(treeAsSeq.duplicate, i))) :::
        List(typedValDef(tail, lastBinding))

      val (nrows, frows) = List.unzip(
        for ((c, row) <- pats.zip(rest.row)) yield getSubPatterns(ys.size, c) match {
          case Some(ps) => (Some(row.insert(ps)), if (mustCheck(c, av)) Some(row.insert(c)) else None)
          case None     => (None, Some(row.insert(c)))
        })

      val succRep = makeSuccRep(vs, tail, nrows.flatMap(x => x))
      val failRep = rep.make(scrut.mkList(rest.temp), frows.flatMap(x => x))

      // fixed length
      val cond = getCond(treeAsSeq, xs.length)
      return ({thenp:Tree => {elsep:Tree =>
        If(cond, squeezedBlock(bindings, thenp), elsep)}}, succRep, failRep)
    }

    // lengthArg is exact length
    protected def getCond(tree:Tree, lengthArg:Int) = seqHasLength(tree.duplicate, pats.head.tpe, lengthArg)

    final def tree(implicit theOwner: Symbol, failTree: Tree) = {
      val (cx,srep,frep) = this.getTransition
      val succ = srep.toTree
      val fail = frep.toTree
      cx(succ)(fail)
    }
  }

  /** handle sequence pattern and ArrayValue with star patterns
   */
  final class MixSequenceStar(pats: Patterns, rest:Rep)(implicit rep:RepFactory) extends MixSequence(pats, rest) {
    // in principle, we could optimize more, but variable binding gets complicated (@todo use finite state methods instead)
    override def getSubPatterns(minlen:Int, x:Tree) = x match {
      case av @ ArrayValue(_,xs) if (!isRightIgnoring(av) && xs.length   == minlen) =>  // Seq(p1,...,pN)
        Some(xs ::: gen.mkNil :: EmptyTree :: Nil)
      case av @ ArrayValue(_,xs) if ( isRightIgnoring(av) && xs.length-1 == minlen) =>  // Seq(p1,...,pN,_*)
        Some(                                   removeStar(xs) ::: EmptyTree :: Nil)
      case av @ ArrayValue(_,xs) if ( isRightIgnoring(av) && xs.length-1  < minlen) =>  // Seq(p1..,pJ,_*)   J < N
        Some(                                  getDummies(minlen + 1) ::: x :: Nil)
      case EmptyTree | Ident(nme.WILDCARD)      =>
        Some(                                  getDummies(minlen + 1          + 1))
      case _                                    =>
        None

    }

    override protected def makeSuccRep(vs:List[Symbol], tail:Symbol, nrows:List[Row])(implicit theOwner: Symbol) =
      rep.make(scrut.mkList(vs ::: List(tail), rest.temp), nrows)

    // lengthArg is minimal length
    override protected def getCond(tree:Tree, lengthArg:Int) = seqLongerThan(tree.duplicate, pats.head.tpe, lengthArg - 1)
  }


  // @todo: equals test for same constant
  class MixEquals(val pats: Patterns, val rest: Rep)(implicit rep: RepFactory) extends RuleApplication(rep) {
    val Patterns(scrut, patterns) = pats
    val head = pats.head

    /** condition (to be used in IF), success and failure Rep */
    final def getTransition(implicit theOwner: Symbol): (Tree, Rep, Symbol, Rep) = {
      val vlue = (head.tpe: @unchecked) match {
        case TypeRef(_,_,List(SingleType(pre,sym))) => gen.mkAttributedRef(pre,sym)
        case TypeRef(_,_,List(PseudoType(o)))       => o.duplicate
      }
      assert(vlue.tpe ne null, "value tpe is null")
      val vs        = head.strip1.toList
      val nsuccFst  = rest.row.head.insert2(List(EmptyTree), vs, scrut.sym)
      val fLabel    = theOwner.newLabel(scrut.pos, cunit.fresh.newName(scrut.pos, "failCont%")) // warning, untyped
      val sx        = rep.shortCut(fLabel) // register shortcut
      val nsuccRow  = nsuccFst :: Row(getDummies( 1 /* scrutinee */ + rest.temp.length), NoBinding, NoGuard, sx) :: Nil

      // todo: optimize if no guard, and no further tests
      val nsucc = rep.make(scrut.mkList(rest.temp), nsuccRow)
      val nfail = repWithoutHead(pats, rest)

      (typer.typed(Equals(scrut.id, vlue)), nsucc, fLabel, nfail)
    }

    final def tree(implicit theOwner: Symbol, failTree: Tree) = {
      val (cond, srep, fLabel, frep) = this.getTransition
      val cond2 = typer.typed( rep.handleOuter(cond) )
      val fail = typer.typed( frep.toTree)
      fLabel setInfo MethodType(Nil, fail.tpe)

      typer.typed( If(cond2, srep.toTree, LabelDef(fLabel, Nil, fail)) )
    }
  }

  /** mixture rule for type tests
  **/
  class MixTypes(val pats: Patterns, val rest: Rep)(implicit rep: RepFactory) extends RuleApplication(rep) {
    val Patterns(scrut, patterns) = pats

    private def subpatterns(p: Tree): List[Tree] = p match {
      case Bind(_, p)                                                 => subpatterns(p)
      case app @ Apply(fn, ps) if app.tpe.isCaseClass && fn.isType    => if (pats.isCaseHead) ps else pats.dummies
      case Apply(fn, xs) if !xs.isEmpty || fn.isType                  => abort("strange Apply")
      case _                                                          => pats.dummies
    }

    // moreSpecific: more specific patterns
    //     subsumed: more general patterns (subsuming current), row index and subpatterns
    //    remaining: remaining, row index and pattern
    def join[T](xs: List[Option[T]]): List[T] = xs.flatMap(x => x)
    val (moreSpecific, subsumed, remaining) : (List[Tree], List[(Int, List[Tree])], List[(Int, Tree)]) = unzip3(
      for ((pat @ Strip2(spat), j) <- pats.zip) yield {
        def eqHead(tpe: Type) = pats.headType =:= tpe
        def alts(yes: Tree, no: Tree) = if (eqHead(pat.tpe)) yes else no

        lazy val isDef                = isDefaultPattern(pat)
        lazy val cmp: TypeComparison  = spat.tpe.cmp(pats.headType)  // contains type info about pattern's type vs. head pattern
        lazy val dummy                = (j, pats.dummies)
        lazy val pass                 = (j, pat)
        lazy val subs                 = (j, subpatterns(pat))

        import cmp._  // imports xIsaY, etc.

        // each pattern will yield a triple of options corresponding to the three lists,
        // which will be flattened down to the values
        implicit def mkOpt[T](x: T): Option[T] = Some(x)    // limits noise from Some(value)
        (spat match {
          case Const(null) if !eqHead(spat.tpe)           => (None, None, pass)           // special case for constant null
          case _ if pats.isObjectTest(pat)                => (EmptyTree, dummy, None)     // matching an object
          case Typed(p @ Strip2(_: UnApply), _) if xIsaY  => (p, dummy, None)             // <:< is never <equals>
          case q @ Typed(pp, _) if xIsaY                  => (alts(pp, q), dummy, None)   // never =:= for <equals>
          case z: UnApply                                 => (None, None, pass)
          case _ if erased.xIsaY || xIsaY && !isDef       => (alts(EmptyTree, pat), subs, None) // never =:= for <equals>
          case _ if erased.yIsaX || yIsaX || isDef        => (EmptyTree, dummy, pass)     // subsuming (matched *and* remaining pattern)
          case _                                          => (None, None, pass)
        }) : (Option[Tree], Option[(Int, List[Tree])], Option[(Int, Tree)])
      }
    ) match { case (x,y,z) => (join(x), join(y), join(z)) }

    override def toString = {
      "MixTypes("+scrut+":"+scrut.tpe+") {\n  moreSpecific:"+moreSpecific+"\n  subsumed:"+subsumed+"\n  remaining"+remaining+"\n}"
    }

    /** returns casted symbol, success matrix and optionally fail matrix for type test on the top of this column */
    final def getTransition(implicit theOwner: Symbol): (Scrutinee, Rep, Option[Rep]) = {
      val casted = scrut.casted(pats.headType)

      // succeeding => transition to translate(subsumed) (taking into account more specific)
      val nmatrix = {
        val ms = moreSpecific.exists(_ != EmptyTree)
        val accessorTemps =
          if (!pats.isCaseHead) Nil
          else casted.accessors.map(meth => newVar(scrut.pos, casted.tpe.memberType(meth).resultType, scrut.flags))
        val subtestTemps = if (!ms) Nil else List(casted.sym)
        val subtests =
          if (!ms) subsumed
          else moreSpecific.zip(subsumed) map { case (mspat, (j, pats)) => (j, mspat::pats) }
        val ntriples = for ((j, ps) <- subtests) yield {
          val (vs, thePat) = strip(pats(j))
          rest.row(j).insert2(ps, vs, casted.sym)
        }
        rep.make(subtestTemps ::: accessorTemps ::: rest.temp, ntriples)
      }

      // fails      => transition to translate(remaining)
      val nmatrixFail: Option[Rep] = {
        val ntemps   = scrut.mkList(rest.temp)
        val ntriples = for ((j, pat) <- remaining) yield rest.row(j).insert(pat)
        if (ntriples.isEmpty) None else Some(rep.make(ntemps, ntriples))
      }

      (casted, nmatrix, nmatrixFail)
    }

    final def tree(implicit theOwner: Symbol, failTree: Tree): Tree = {
      val (casted, srep, frep) = this.getTransition
      val cond = condition(casted.tpe, scrut)
      val succ = srep.toTree
      val fail = frep.map(_.toTree) getOrElse failTree

      // dig out case field accessors that were buried in (***)
      val cfa       = if (!pats.isCaseHead) Nil else casted.accessors
      val caseTemps = srep.temp match { case x :: xs if x == casted.sym => xs ; case x => x }

      var vdefs = for ((tmp, accessorMethod) <- caseTemps.zip(cfa)) yield {
        val untypedAccess = Code.fn(casted.id, accessorMethod)
        val typedAccess = typer.typed(untypedAccess)
        typedValDef(tmp, typedAccess)
      }

      if (casted.sym ne scrut.sym)
        vdefs = ValDef(casted.sym, gen.mkAsInstanceOf(scrut.id, casted.tpe)) :: vdefs

      typer.typed( If(cond, squeezedBlock(vdefs, succ), fail) )
    }
  }

  case class Row(pat: List[Tree], subst: Bindings, guard: Guard, bx: Int) {
    def insert(h: Tree)                               = Row(h :: pat, subst, guard, bx)
    def insert(hs: List[Tree])                        = Row(hs ::: pat, subst, guard, bx)           // prepends supplied tree
    def replace(hs: List[Tree])                       = Row(hs, subst, guard, bx)                   // substitutes for patterns
    def rebind(b: Bindings)                           = Row(pat, b, guard, bx)                      // substitutes for bindings
    def insert2(hs: List[Tree], vs: Iterable[Symbol], temp: Symbol) =                               // prepends and prepends
      Row(hs ::: pat, subst.add(vs, temp), guard, bx)

    def insert(p: Pattern)                            = Row(p.tree :: pat, subst, guard, bx)        // transitioning to patterns

    /** returns true if the patterns in this row cover a type symbols "combination" and there is no guard
     *  @param comb pairs of (column index, type symbol)
     */
    def covers(comb: List[(Int, Symbol)]) = {
      lazy val results = for ((i, sym) <- comb ; val p = strip2(pat(i))) yield p match {
        case _ if isDefaultPattern(p)   => true
        case _: UnApply | _: ArrayValue => true
        case _                          => p.tpe.coversSym(sym)
      }

      guard.isEmpty && results.forall(_ == true)
    }

    // returns this row with alternatives expanded
    def expand(classifyPat: (Tree, Int) => Tree): List[Row] = {
      def isAlternative(p: Tree): Boolean = p match {
        case Bind(_,p)       => isAlternative(p)
        case Alternative(ps) => true
        case _               => false
      }
      def getAlternativeBranches(p: Tree): List[Tree] = {
        def get_BIND(pctx:Tree => Tree, p:Tree): List[Tree] = p match {
          case b @ Bind(n,p)   => get_BIND((x: Tree) => pctx(copy.Bind(b, n, x) setType x.tpe), p)
          case Alternative(ps) => ps map pctx
        }
        get_BIND(x => x, p)
      }

      val indexOfAlternative            = pat findIndexOf isAlternative
      val pats: List[Tree]              = List.map2(pat, pat.indices)(classifyPat)
      lazy val (prefix, alts :: suffix) = pats.splitAt(indexOfAlternative)
      lazy val alternativeBranches      = getAlternativeBranches(alts) map { p => replace(prefix ::: p :: suffix) }

      if (indexOfAlternative == -1) List(replace(pats)) else alternativeBranches
    }
  }

  class RepFactory(val handleOuter: Tree => Tree)(implicit val typer : Typer) {
    var vss: List[List[Symbol]] = _
    var labels: Array[Symbol] = new Array[Symbol](4)
    var targets: List[Tree] = _
    var reached: BitSet = _
    var shortCuts: List[Symbol] = Nil

    final def make(temp:List[Symbol], row:List[Row], targets: List[Tree], vss:List[List[Symbol]])(implicit theOwner: Symbol): Rep = {
      // ensured that labels(i) eq null for all i, cleanup() has to be called after translation
      this.targets      = targets
      if (targets.length > labels.length)
        this.labels     = new Array[Symbol](targets.length)
      this.vss          = vss
      this.reached      = new BitSet(targets.length)
      make(temp, row)
    }

    final def shortCut(theLabel:Symbol): Int = {
      shortCuts = shortCuts ::: List(theLabel)
      -shortCuts.length
    }

    final def cleanup(tree: Tree)(implicit theOwner: Symbol): Tree = {
      object lxtt extends Transformer {
        override def transform(tree:Tree): Tree = tree match {
          case blck @ Block(vdefs, ld @ LabelDef(name,params,body)) =>
            val bx = labelIndex(ld.symbol)
            if (bx >= 0 && !isReachedTwice(bx)) squeezedBlock(vdefs,body)
            else blck

          case If(cond, Const(true), Const(false)) =>
            super.transform(cond)
          case If(cond1, If(cond2, thenp, elsep1), elsep2) if (elsep1 equalsStructure elsep2) =>
            super.transform(If(And(cond1,cond2), thenp, elsep1))
          case If(cond1, If(cond2, thenp, Apply(jmp,List())), ld:LabelDef) if (jmp.symbol eq ld.symbol) =>
            super.transform(If(And(cond1,cond2), thenp, ld))

          case t => super.transform(t)
        }
      }
      val res = lxtt.transform(tree)
      cleanup()
      res
    }

    final def cleanup() {
      for (i <- 0 until targets.length) labels(i) = null
      reached = null
      shortCuts = Nil
    }
    final def isReached(bx:Int)   = labels(bx) ne null
    final def markReachedTwice(bx:Int) { reached += bx }
    /** @pre bx < 0 || labelIndex(bx) != -1 */
    final def isReachedTwice(bx: Int) = (bx < 0) || reached(bx)
    /* @returns bx such that labels(bx) eq label, -1 if no such bx exists */
    final def labelIndex(label: Symbol): Int = labels.findIndexOf(_ eq label)

    /** first time bx is requested, a LabelDef is returned. next time, a jump.
     *  the function takes care of binding
     */
    final def requestBody(bx: Int, subst: Bindings)(implicit theOwner: Symbol): Tree = {
      if (bx < 0) { // is shortcut
        val jlabel = shortCuts(-bx-1)
        return Apply(mkIdent(jlabel), Nil)
      }
      if (!isReached(bx)) { // first time this bx is requested
        // might be bound elsewhere ( see `x @ unapply' ) <-- this comment refers to null check
        val (vsyms, argts, vdefs) : (List[Symbol], List[Type], List[Tree]) = unzip3(
          for (v <- vss(bx) ; substv <- subst(v)) yield
            (v, v.tpe, typedValDef(v, substv))
        )

        val body  = targets(bx)
        // @bug: typer is not able to digest a body of type Nothing being assigned result type Unit
        val tpe = if (body.tpe.isNothing) body.tpe else resultType
        val label = theOwner.newLabel(body.pos, "body%"+bx) setInfo MethodType(argts, tpe)
        // TODO - newLabel doesn't get a fresh name, is that okay? or should it be more like this:
        // val label = theOwner.newLabel(body.pos, cunit.fresh.newName(body.pos, "body%"+bx)) setInfo MethodType(argts, tpe)
        labels(bx) = label

        return body match {
          case _: Throw | _: Literal  => squeezedBlock(vdefs, body.duplicate setType tpe)
          case _                      => squeezedBlock(vdefs.reverse, LabelDef(label, vsyms, body setType tpe))
        }
      }

      // if some bx is not reached twice, its LabelDef is replaced with body itself
      markReachedTwice(bx)
      val args: List[Ident] = vss(bx).flatMap(subst(_))
      val label = labels(bx)
      val body = targets(bx)
      val MethodType(fmls, _) = label.tpe

      // sanity checks
      if (fmls.length != args.length) {
        cunit.error(body.pos, "consistency problem in target generation ! I have args "+
          args+" and need to jump to a label with fmls "+fmls)
        throw FatalError("consistency problem")
      }
      fmls.zip(args).find(x => !(x._2.tpe <:< x._1)) match {
        case Some(Tuple2(f, a)) => cunit.error(body.pos, "consistency problem ! "+a.tpe+" "+f) ; throw FatalError("consistency problem")
        case None =>
      }

      body match {
        case _: Throw | _: Literal =>       // might be bound elsewhere (see `x @ unapply')
          val vdefs = for (v <- vss(bx) ; substv <- subst(v)) yield typedValDef(v, substv)
          squeezedBlock(vdefs, body.duplicate setType resultType)
        case _ =>
          Apply(mkIdent(label),args)
      }
    }

    /** the injection here handles alternatives and unapply type tests */
    final def make(temp: List[Symbol], row1: List[Row])(implicit theOwner: Symbol): Rep = {
      // equals check: call singleType(NoPrefix, o.symbol) `stpe'. Then we could also return
      // `typeRef(definitions.ScalaPackageClass.tpe, definitions.EqualsPatternClass, List(stpe))'
      // and force an equality check. However, exhaustivity checking would not work anymore.
      // so first, extend exhaustivity check to equalspattern
      def sType(o: Tree) = singleType(o.tpe.prefix, o.symbol)
      def equalsCheck(o: Tree) = if (o.symbol.isValue) singleType(NoPrefix, o.symbol) else sType(o)
      def isModule(o: Tree) = o.symbol.isModule || o.tpe.termSymbol.isModule
      def applyType(o: Tree, fn: Tree): Type = fn match {
        case _ if isModule(o)     => sType(o)
        case Select(path, sym)    => (path, path.tpe) match {
          case (_, t: ThisType)     => singleType(t, o.symbol)            // cases 2/3 are e.g. `case Some(p._2)' in s.c.jcl.Map
          case (_: Apply, _)        => PseudoType(o)                      // outer-matching: test/files/pos/t154.scala
          case (_, _)               => singleType(sType(path), o.symbol)  // old
        }
        case o: Ident             => equalsCheck(o)
      }

      def classifyPat(opat: Tree, j: Int): Tree = {
        val (vs, strippedPat) = strip(opat) match { case (vset, pat) => (vset.toList, pat) }
        // @todo: rewrite UnApply(Apply(fn, _), _) case, using __UnApply instead of UnApply like so:
        // case  ua @ __UnApply(_,argtpe,_) =>
        //   val ua = prepat
        //   val npat = (if (temp(j).tpe <:< argtpe) ua else Typed(ua,TypeTree(argtpe)).setType(argtpe))
        //   pats = (makeBind(vs, npat) setType argtpe)::pats

        strippedPat match {
          case _: Alternative                       => opat
          case Typed(p @ Strip2(_: UnApply), tpt)   => if (temp(j).tpe <:< tpt.tpe) makeBind(vs, p)
                                                       else opat
          // case Ident_Or_Empty()                     => opat    // this doesn't work - see notes in PatternNode
          case Ident(nme.WILDCARD) | EmptyTree      => opat
          case _: Literal | _: Typed                => opat
          case o: Ident                             => mkTypedBind(vs, equalsCheck(o))  // Ident(_) != nme.WILDCARD
          case o: Select                            => mkTypedBind(vs, equalsCheck(o))
          case o: This                              => opat
          // @pre for UnApply_TypeApply: is not right-ignoring (no star pattern) ; no exhaustivity check
          case UnApply_TypeApply(tptArg, xs)        => temp(j) setFlag Flags.TRANS_FLAG
                                                       makeBind(vs, normalizedListPattern(xs, tptArg.tpe))
          case ua @ UnApply(Apply(fn, _), _)        => val MethodType(List(argtpe, _*), _) = fn.tpe
                                                       val npat = if (temp(j).tpe <:< argtpe) ua
                                                                  else Typed(ua, TypeTree(argtpe)) setType argtpe
                                                       makeBind(vs, npat) setType argtpe
          case o @ Apply_Function(fn)               => val stpe = applyType(o, fn)
                                                       val ttst = mkEqualsRef(List(stpe))
                                                       makeBind(vs, Typed(mk_(ttst), TypeTree(stpe)) setType ttst)
          case Apply_Value(pre, sym)                => mkEmptyTreeBind(vs, mkEqualsRef(List(singleType(pre, sym))))
          case Apply_CaseClass_NoArgs(tpe)          => mkEmptyTreeBind(vs, tpe)
          case Apply_CaseClass_WithArgs()           => opat
          case _: ArrayValue                        => opat
          case x                                    => throw new Exception("Unexpected pattern: " + x.getClass + " => " + x)
        }
      }

      val row = row1 flatMap { _.expand(classifyPat) }
      if (row.length != row1.length) make(temp, row)  // recursive call if any change
      else Rep(temp, row).init
    }
  }

  case class Rep(val temp: List[Symbol], val row: List[Row]) {
    /** converts this to a tree - performs recursive call to translation in the process to get sub reps
     */
    final def toTree(implicit theOwner: Symbol, failTree: Tree, rep: RepFactory): Tree =
      this.applyRule.tree

    private def setsToCombine: List[(Int, immutable.Set[Symbol])] = for {
      (sym, i) <- temp.zipWithIndex
      if sym hasFlag Flags.MUTABLE          // indicates that have not yet checked exhaustivity
      if !(sym hasFlag Flags.TRANS_FLAG)    // indicates @unchecked
      if sym.tpe.typeSymbol hasFlag Flags.SEALED
    } yield {
      sym resetFlag Flags.MUTABLE
      // this should enumerate all cases... however, also the superclass is taken if it is not abstract
      def candidates(tpesym: Symbol): immutable.Set[Symbol] = {
        def countCandidates(x: Symbol) = if (x hasFlag Flags.ABSTRACT) candidates(x) else candidates(x) + x
        if (tpesym hasFlag Flags.SEALED) tpesym.children.flatMap(countCandidates)
        else emptySymbolSet
      }
      (i, candidates(sym.tpe.typeSymbol))
    }

    // computes cartesian product, keeps indices available
    private def combine(colcom: List[(Int, Set[Symbol])]): List[List[(Int, Symbol)]] = colcom match {
      case Nil => Nil
      case (i,syms)::Nil => syms.toList.map { sym => List((i,sym)) }
      case (i,syms)::cs  => for (s <- syms.toList; rest <- combine(cs)) yield (i,s) :: rest
    }

    private def comboCovers(combo: List[(Int, Symbol)]) = row exists { r => r.covers(combo) }

    def init: this.type = {
      val allcomb = combine(setsToCombine)
      if (allcomb forall comboCovers) return this

      // if we reach here, patterns were not exhaustive
      def mkPad(xs: List[(Int, Symbol)], i: Int): String = xs match {
        case Nil                  => pad("*")
        case (j, sym) :: rest     => if (j == i) pad(sym.name.toString) else mkPad(rest, i)
      }
      def mkMissingStr(open: List[(Int, Symbol)]) =
        "missing combination " + temp.indices.map(mkPad(open, _)).mkString + "\n"

      val missingCombos = allcomb
        . filter(open => row.forall(!_.covers(open)))
        . map(mkMissingStr)
        . mkString

      cunit.warning(temp.head.pos, "match is not exhaustive!\n" + missingCombos)
      this
    }

    /*   internal representation is (temp:List[Symbol], row:List[Row])
     *
     *         tmp1       tmp_m
     */
    final def applyRule(implicit theOwner: Symbol, rep: RepFactory): RuleApplication = row match {
      case Nil                              => ErrorRule()
      case Row(pats, subst, g, bx) :: xs    =>
        var bnd = subst
        for (((rpat, t), px) <- pats.zip(temp).zipWithIndex) {
          val (vs, p) = strip(rpat)
          if (isDefaultPattern(p)) bnd = bnd.add(vs, t)
          else {
            // Row( _  ... _ p_1i  ...  p_1n   g_m  b_m ) :: rows
            // cut out column px that contains the non-default pattern
            val column    = rpat :: row.tail.map(_.pat(px))
            val restTemp  = temp.dropIndex(px)
            val restRows  = row.map(r => r.replace(r.pat.dropIndex(px)))
            val mr        = MixtureRule(new Scrutinee(t), column, rep.make(restTemp, restRows))
            //DBG("\n---\nmixture rule is = " + mr.getClass)
            return mr
          }
        }
        // Row(   _   ...   _     g_1  b_1 ) :: rows     it's all default patterns
        val rest = if (g.isEmpty) null else rep.make(temp, xs)    // TODO - why null?
        //DBG("\n---\nmixture rule is = VariableRule")
        VariableRule (bnd, g, rest, bx)
    }

    // a fancy toString method for debugging
    override final def toString = {
      val tempStr = temp.map(t => pad(t.name)).mkString + "\n"
      val rowStr = row.map(r => (r.pat ::: List(r.subst, r.guard, r.bx)).map(pad).mkString + "\n").mkString
      tempStr + rowStr
    }

    private val NPAD = 15
    private def pad(s: Any): String = pad(s.toString)
    private def pad(s: String): String = List.make(NPAD - s.length - 1, " ").mkString + s
  }

  /** creates initial clause matrix
   */
  final def initRep(roots: List[Symbol], cases: List[Tree], rep: RepFactory)(implicit theOwner: Symbol) = {
    // communicate whether exhaustiveness-checking is enabled via some flag
    val (rows, targets, vss): (List[Option[Row]], List[Tree], List[List[Symbol]]) = unzip3(
      for ((CaseDef(pat, g, b), bx) <- cases.zipWithIndex) yield {  // stash away pvars and bodies for later
        def rowForPat: Option[Row] = pat match {
          case _ if roots.length <= 1 => Some(Row(List(pat), NoBinding, Guard(g), bx))
          case Apply(fn, pargs)       => Some(Row(pargs, NoBinding, Guard(g), bx))
          case Ident(nme.WILDCARD)    => Some(Row(getDummies(roots.length), NoBinding, Guard(g), bx))
          case _                      => None
        }
        (rowForPat, b, definedVars(pat))
      }
    )

    // flatMap the list of options yields the list of values
    rep.make(roots, rows.flatMap(x => x), targets, vss)
  }

  final def newVar(pos: Position, name: Name, tpe: Type, flags: List[Long])(implicit theOwner: Symbol): Symbol = {
    assert(tpe ne null, "newVar("+name+", null)")
    val sym = theOwner.newVariable(pos, name) // careful: pos has special meaning
    sym setInfo tpe
    sym setFlag flags.foldLeft(Flags.SYNTHETIC.toLong)(_|_)
  }

  final def newVar(pos: Position, tpe: Type, flags: List[Long])(implicit theOwner: Symbol): Symbol =
    newVar(pos, cunit.fresh.newName(pos, "temp"), tpe, flags)

  final def newVar(pos: Position, tpe: Type)(implicit theOwner: Symbol): Symbol =
    newVar(pos, tpe, Nil)

  /** returns the condition in "if (cond) k1 else k2"
   */
  final def condition(tpe: Type, scrut: Scrutinee)(implicit typer: Typer, theOwner: Symbol, rep: RepFactory): Tree = {
    assert(scrut.isDefined)
    val cond = rep.handleOuter(typer.typed(condition(tpe, scrut.id)))

    if (!needsOuterTest(tpe, scrut.tpe, theOwner)) cond
    else addOuterCondition(cond, tpe, scrut.id, rep.handleOuter)
  }

  final def condition(tpe: Type, scrutTree: Tree)(implicit typer : Typer): Tree = {
    assert((tpe ne NoType) && (scrutTree.tpe ne NoType))
    lazy val equalsRef      = Equals(gen.mkAttributedRef(tpe.termSymbol), scrutTree)
    lazy val isInst         = gen.mkIsInstanceOf(scrutTree, tpe)

    tpe match {
      case _: SingletonType if !tpe.isInstanceOf[ConstantType] =>
        if (tpe.termSymbol.isModule)                            equalsRef               // object
        else if (tpe.prefix ne NoPrefix)                        typer.typed(isInst)
        else                                                    typer.typed(equalsRef)
      case ct: ConstantType => ct.value match {                                         // constant
          case v @ Constant(null) if scrutTree.tpe.isAnyRef     => Eq(scrutTree, Literal(v))
          case v                                                => Equals(scrutTree, Literal(v))
        }
      case _ if scrutTree.tpe <:< tpe && tpe.isAnyRef           => NotNull(scrutTree)
      case _                                                    => gen.mkIsInstanceOf(scrutTree, tpe)
    }
  }

  /** adds a test comparing the dynamic outer to the static outer */
  final def addOuterCondition(cond: Tree, tpe2test: Type, scrut: Tree, handleOuter: Tree=>Tree) = {
    val theRef = handleOuter(tpe2test match {
      case TypeRef(NoPrefix, _, _)          => abort("assertion failed: NoPrefix")
      case TypeRef(ThisType(clazz), _, _)   => gen.mkAttributedThis(clazz)
      case TypeRef(prefix, _, _)            => gen.mkAttributedRef(prefix.prefix, prefix.termSymbol)
    })

    outerAccessor(tpe2test.typeSymbol) match {
      case NoSymbol => if (settings.debug.value) cunit.warning(scrut.pos, "no outer acc for "+tpe2test.typeSymbol) ; cond
      case outerAcc => And(cond, Eq(Code.fn(gen.mkAsInstanceOf(scrut, tpe2test, true), outerAcc), theRef))
    }
  }

}
