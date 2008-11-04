/* NSC -- new Scala compiler
 * Copyright 2005-2007 LAMP/EPFL
 * Copyright 2007 Google Inc. All Rights Reserved.
 * Author: bqe@google.com (Burak Emir)
 */
// $Id$

package scala.tools.nsc.matching

import util.Position
import collection._
import collection.mutable.BitSet
import collection.immutable.IntMap
import MatchUtil.ListPlus._
import MatchUtil.Implicits._
import MatchUtil.Flags._
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
 */
trait ParallelMatching  {
  self: transform.ExplicitOuter with PatternNodes with CodeFactory =>

  import global.{typer => _, _}
  import analyzer.Typer;
  import symtab.Flags

  // used as argument to `EqualsPatternClass'
  case class PseudoType(o: Tree) extends SimpleTypeProxy {
    override def underlying: Type = o.tpe
    override def safeToString: String = "PseudoType("+o+")"
  }

  /** picks which rewrite rule to apply
   *  @precondition: column does not contain alternatives (ensured by initRep)
   */
  def MixtureRule(scrutinee: Symbol, column: List[Tree], rest: Rep)(implicit rep: RepFactory): RuleApplication = {

    def isSimpleSwitch: Boolean = {
      val simpleSwitchCandidate = (tree : Tree) => strip2(tree) match {
        case Literal(const : Constant) if isNumeric(const.tag) =>
          const.tag match {
            case FloatTag | DoubleTag | LongTag => false;
            case _ => true;
          }
        case _ => false;
      }

      (isSameType(scrutinee.tpe.widen, definitions.IntClass.tpe) ||
       isSameType(scrutinee.tpe.widen, definitions.CharClass.tpe)) &&
        column.init.forall(simpleSwitchCandidate) && {
          val last = column.last;
          // TODO: This needs to also allow the case that the last is a compatible
          // type pattern.
          simpleSwitchCandidate(last) || isDefaultPattern(last)
        }
    }

    // an unapply for which we don't need a type test
    def isUnapplyHead(): Boolean = column.head match {
      case __UnApply(_,argtpe,_)  => scrutinee.tpe <:< argtpe
      case _                      => false
    }

    // true if pattern type is direct subtype of scrutinee (can't use just <:< cause have to take variance into account)
    def directSubtype(ptpe: Type) =
      ptpe.parents.exists(x => (x.typeSymbol eq scrutinee.tpe.typeSymbol) && (x <:< scrutinee.tpe))

    // true if each pattern type is case and direct subtype of scrutinee
    def isFlatCases(col:List[Tree]): Boolean = (col eq Nil) || {
      strip2(col.head) match {
        case a @ Apply(fn,_) =>
          isCaseClass(a.tpe) && directSubtype( a.tpe ) && isFlatCases(col.tail)
        case t @ Typed(_,tpt) =>
          isCaseClass(tpt.tpe) && directSubtype( t.tpe ) && isFlatCases(col.tail)
        case Ident(nme.WILDCARD) =>
          isFlatCases(col.tail) // treat col.tail specially?
        case p =>
          false
      }
    }

    column.head match {
      case x if isEqualsPattern(x.tpe) => new MixEquals(scrutinee, column, rest);
      case (x : ArrayValue) => if (isRightIgnoring(x)) new MixSequenceStar(scrutinee, column, rest)
                               else new MixSequence(scrutinee, column, rest);
      case _ if isSimpleSwitch => new MixLiterals(scrutinee, column, rest)
      case _ if isUnapplyHead() => new MixUnapply(scrutinee, column, rest)
      case _ => new MixTypes(scrutinee, column, rest)
    }
  }

  sealed abstract class RuleApplication(rep: RepFactory) {
    def scrutinee:Symbol
    implicit def typer = rep.typer;

    // used in MixEquals and MixSequence
    final protected def repWithoutHead(col: List[Tree],rest: Rep)(implicit theOwner: Symbol): Rep = {
      val nfailrow = List.map2(col.tail, rest.row.tail)((p, r) => r.insert(p))
      rep.make(scrutinee::rest.temp, nfailrow)
    }

    /** translate outcome of the rule application into code (possible involving recursive application of rewriting) */
    def tree(implicit theOwner: Symbol, failTree: Tree): Tree
  }

  case class ErrorRule(implicit rep:RepFactory) extends RuleApplication(rep) {
    def scrutinee: Symbol = impossible
    final def tree(implicit theOwner: Symbol, failTree: Tree) = failTree
  }

  /**  {case ... if guard => bx} else {guardedRest} */
  case class VariableRule(subst:Binding, guard: Tree, guardedRest:Rep, bx: Int)(implicit rep:RepFactory) extends RuleApplication(rep) {
    def scrutinee: Symbol = impossible
    final def tree(implicit theOwner: Symbol, failTree: Tree): Tree = {
      val body = typer.typed { rep.requestBody(bx, subst) }
      if (guard eq EmptyTree)
        return body
      val vdefs = targetParams(subst)
      val typedElse = repToTree(guardedRest)
      val typedIf = typer.typed { If(guard.duplicate, body, typedElse) }

      typer.typed { Block(vdefs, typedIf) }
    }
  }

  /** superclass of mixture rules for case classes and literals (both translated to switch on an integer)
   */
  abstract class CaseRuleApplication(rep: RepFactory) extends RuleApplication(rep) {
    def column: List[Tree]
    def rest:   Rep

    // e.g. (1,1) (1,3) (42,2) for column {case ..1.. => ;; case ..42..=> ;; case ..1.. => }
    var defaultV: collection.immutable.Set[Symbol] = emptySymbolSet
    var defaultIndexSet = new BitSet(column.length)

    def insertDefault(tag: Int, vs: Set[Symbol]) {
      defaultIndexSet += tag
      defaultV = defaultV ++ vs
    }

    def haveDefault: Boolean = !defaultIndexSet.isEmpty
    lazy val defaultRows: List[Row]   = defaultIndexSet.toList.reverseMap(grabRow);

    protected var tagIndices = IntMap.empty[List[Int]]
    protected def grabTemps: List[Symbol] = rest.temp
    protected def grabRow(index: Int): Row = {
      val r @ Row(_,s,_,_) = rest.row(index)
      if (defaultV.isEmpty) r else {
        val vs = strip1(column(index))  // get vars
        r.insert2(Nil, s.add(vs, scrutinee))
      }
    }

    /** inserts row indices using in to list of tagIndices */
    protected def tagIndicesToReps(implicit theOwner: Symbol) : List[(Int, Rep)] =
      tagIndices map { case (k, v) => (k, rep.make(grabTemps, v.reverseMap(grabRow) ::: defaultRows)) } toList

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
  }

  /** mixture rule for literals
   */
  class MixLiterals(val scrutinee:Symbol, val column:List[Tree], val rest:Rep)(implicit rep:RepFactory) extends CaseRuleApplication(rep) {
    var varMap: List[(Int,List[Symbol])] = Nil

    private def sanity(pos:Position, tag: Int, pvars:List[Symbol]) {
      varMap = (tag,pvars)::varMap
    }

    //lazy
    private def bindVars(Tag:Int, orig: Binding): Binding  = {
      def myBindVars(rest:List[(Int,List[Symbol])], bnd: Binding): Binding  = rest match {
        case Nil => bnd
        case (Tag,vs)::xs => myBindVars(xs, bnd.add(vs, scrutinee))
        case (_,  vs)::xs => myBindVars(xs, bnd)
      }
      myBindVars(varMap, orig)
    }

    for ((x, i) <- column.zipWithIndex) strip(x) match {
      case (pvars, p @ Literal(Constant(c:Int)))  => sanity(p.pos,     c  , definedVars(x)); insertTagIndexPair(c,i)
      case (pvars, p @ Literal(Constant(c:Char))) => sanity(p.pos, c.toInt, definedVars(x)); insertTagIndexPair(c.toInt,i)
      case (pvars, p )     if isDefaultPattern(p) => insertDefault(i,pvars)
    }

    final def tree(implicit theOwner: Symbol, failTree: Tree): Tree = {
      val (branches, defaultV, defaultRep) = this.getTransition // tag body pairs
      val cases = for ((tag, r) <- branches) yield {
        val r2 = rep.make(r.temp, r.row.map(x => x.insert2(Nil, bindVars(tag, x.subst))))
        val t2 = repToTree(r2)
        CaseDef(Literal(tag), EmptyTree, t2)
      }

      lazy val ndefault = defaultRep.map(repToTree) getOrElse failTree
      lazy val defCase = CaseDef(mk_(definitions.IntClass.tpe), EmptyTree, ndefault)

      cases match {
        case CaseDef(lit,_,body) :: Nil =>
          If(Equals(mkIdent(this.scrutinee),lit), body, ndefault)
        case _ if isSameType(this.scrutinee.tpe.widen, definitions.CharClass.tpe) =>
          Match(Select(mkIdent(this.scrutinee), nme.toInt), cases ::: List(defCase))
        case _ =>
          Match(mkIdent(this.scrutinee), cases ::: List(defCase))
      }
    }
  }

  /** mixture rule for unapply pattern
   */
  class MixUnapply(val scrutinee: Symbol, val column: List[Tree], val rest: Rep)(implicit rep: RepFactory)
  extends RuleApplication(rep) {
    val (vs, unapp) = strip(column.head)
    lazy val ua @ UnApply(app @ Apply(fn, appargs), args) = unapp

    def newVarCapture(pos:Position,tpe:Type)(implicit theOwner:Symbol) = {
      val v = newVar(pos,tpe)
      propagateFlag(scrutinee, v, Flags.TRANS_FLAG) // propagate "unchecked"
      v
    }

    /** returns (unapply-call, success-rep, optional fail-rep*/
    final def getTransition(implicit theOwner: Symbol): (Tree, List[Tree], Rep, Option[Rep]) = {
      object sameUnapplyCall {
        def unapply(t: Tree) = t match {
          case UnApply(Apply(fn1,_), differentArgs) if (fn.symbol == fn1.symbol) && fn.equalsStructure(fn1) =>
            Some(differentArgs)
          case _ =>
            None
        }
      }
      val ures = newVarCapture(ua.pos, app.tpe)
      val rhs = Apply(fn, mkIdent(scrutinee) :: appargs.tail) setType ures.tpe
      val uacall = typedValDef(ures, rhs)
      val zipped = column.zip(rest.row)
      val nrowsOther = zipped.tail.flatMap { case (pat, r) =>
        strip2(pat) match { case sameUnapplyCall(_) => Nil ; case _ => List(r.insert(pat)) }
      }
      val nrepFail =
        if (nrowsOther.isEmpty) None
        else Some(rep.make(scrutinee::rest.temp, nrowsOther))

      def mkTransition(vdefs: List[Tree], ntemps: List[Symbol], nrows: List[Row]) =
        (uacall, vdefs, rep.make(ntemps ::: scrutinee :: rest.temp, nrows), nrepFail)

      def mkNewRows(sameFilter: (List[Tree]) => List[Tree], defaultTrees: List[Tree]) =
        for ((pat, r) <- zipped) yield strip2(pat) match {
          case sameUnapplyCall(args)  => r.insert2(sameFilter(args) ::: List(EmptyTree), r.subst.add(strip1(pat), scrutinee))
          case _                      => r.insert(defaultTrees ::: List(pat))
        }

      args.length match {
        case 0  => // special case for unapply(), app.tpe is boolean
          mkTransition(Nil, Nil, mkNewRows((xs) => Nil, Nil))

        case 1 => // special case for unapply(p), app.tpe is Option[T]
          val vtpe = app.tpe.typeArgs(0)
          val vsym = newVarCapture(ua.pos, vtpe)
          val nrows = mkNewRows((xs) => List(xs.head), List(EmptyTree))
          val vdef = typedValDef(vsym, Get(mkIdent(ures)))
          mkTransition(List(vdef), List(vsym), nrows)

        case _ => // app.tpe is Option[? <: ProductN[T1,...,Tn]]
          val uresGet = newVarCapture(ua.pos, app.tpe.typeArgs(0))
          val vdefHead = typedValDef(uresGet, Get(mkIdent(ures)))
          val ts = definitions.getProductArgs(uresGet.tpe).get
          val nrows = mkNewRows(identity, getDummies(ts.size))
          val (vdefs: List[Tree], vsyms: List[Symbol]) = List.unzip(
            for ((vtpe, i) <- ts.zip((1 to ts.size).toList)) yield {
              val vchild = newVarCapture(ua.pos, vtpe)
              val accSym = definitions.productProj(uresGet, i)
              val rhs = typer.typed(Apply(Select(mkIdent(uresGet), accSym), Nil))

              (typedValDef(vchild, rhs), vchild)
            })
          mkTransition(vdefHead :: vdefs, vsyms, nrows)
      }
    } /* def getTransition(...) */

    final def tree(implicit theOwner: Symbol, failTree: Tree) = {
      val (uacall, vdefs, srep, frep) = this.getTransition
      val succ = repToTree(srep)
      val fail = frep.map(repToTree) getOrElse failTree
      val cond =
        if (uacall.symbol.tpe.typeSymbol eq definitions.BooleanClass)
          typer.typed{ mkIdent(uacall.symbol) }
        else
          emptynessCheck(uacall.symbol)
      typer.typed { Block(List(rep.handleOuter(uacall)), If(cond,Block(vdefs,succ),fail)) }
    }
  }

  /** handle sequence pattern and ArrayValue (but not star patterns)
   */
  sealed class MixSequence(val scrutinee: Symbol, val column: List[Tree], val rest: Rep)(implicit rep: RepFactory) extends RuleApplication(rep) {

    private val sequenceType = scrutinee.tpe.widen.baseType(definitions.SeqClass)
    private val elementType  = getElemType_Sequence(scrutinee.tpe)

    final def removeStar(xs: List[Tree]): List[Tree] =
      xs.init ::: makeBind(strip1(xs.last).toList, mk_(sequenceType)) :: Nil

    protected def getSubPatterns(len:Int, x:Tree):Option[List[Tree]] = x match {
      case av @ ArrayValue(_,xs) if (!isRightIgnoring(av) && xs.length == len)   => Some(xs ::: List(EmptyTree))
      case av @ ArrayValue(_,xs) if ( isRightIgnoring(av) && xs.length == len+1) => Some(removeStar(xs)) // (*)
      case EmptyTree | Ident(nme.WILDCARD)                                       => Some(getDummies(len+1))
      case _                                                                     => None
    }

    protected def makeSuccRep(vs:List[Symbol], tail:Symbol, nrows:List[Row])(implicit theOwner: Symbol) =
      rep.make(vs ::: tail :: rest.temp, nrows.toList)

    /** returns true if x is more general than y */
    protected def subsumes(x:Tree, y:Tree): Boolean = (x,y) match {
      case (av @ ArrayValue(_,xs), bv @ ArrayValue(_,ys)) =>
        isRightIgnoring(av) && !isRightIgnoring(bv) && xs.length == ys.length+1                   // see (*)
      case _ =>
        false
    }
    // context (to be used in IF), success and failure Rep
    def getTransition(implicit theOwner: Symbol): (Tree => Tree => Tree, Rep, Rep) = {
      assert(isSubType(scrutinee.tpe, column.head.tpe), "problem "+scrutinee.tpe+" not <: "+column.head.tpe)

      val treeAsSeq =
        if (!isSubType(scrutinee.tpe, column.head.tpe))
          typer.typed(gen.mkAsInstanceOf(mkIdent(scrutinee), column.head.tpe, true))
        else
          mkIdent(scrutinee)

      val av @ ArrayValue(_, xs) = column.head
      val ys = if (isRightIgnoring(av)) xs.init else xs
      val vs = ys map(y => newVar(strip2(y).pos, elementType))

      lazy val tail = newVar(scrutinee.pos, sequenceType)
      lazy val lastBinding = if (ys.size > 0) seqDrop(treeAsSeq.duplicate, ys.size) else mkIdent(scrutinee)
      val bindings =
        (for ((v, i) <- vs.zipWithIndex) yield typedValDef(v, seqElement(treeAsSeq.duplicate, i))) :::
        List(typedValDef(tail, lastBinding))

      val (nrows, frows)/* : (List[Option[Row]], List[Option[Row]]) */ = List.unzip(
        for ((c, row) <- column.zip(rest.row)) yield getSubPatterns(ys.size, c) match {
          case Some(ps) => (Some(row.insert(ps)), if (isDefaultPattern(c) || subsumes(c, av)) Some(row.insert(c)) else None)
          case None     => (None, Some(row.insert(c)))
        })

      val succRep = makeSuccRep(vs, tail, nrows.flatMap(x => x))
      val failRep = rep.make(scrutinee :: rest.temp, frows.flatMap(x => x))

      // fixed length
      val cond = getCond(treeAsSeq, xs.length)
      return ({thenp:Tree => {elsep:Tree =>
        If(cond, Block(bindings, thenp), elsep)}}, succRep, failRep)
    }

    // lengthArg is exact length
    protected def getCond(tree:Tree, lengthArg:Int) = seqHasLength(tree.duplicate, column.head.tpe, lengthArg)

    final def tree(implicit theOwner: Symbol, failTree: Tree) = {
      val (cx,srep,frep) = this.getTransition
      val succ = repToTree(srep)
      val fail = repToTree(frep)
      cx(succ)(fail)
    }
  }

  /** handle sequence pattern and ArrayValue with star patterns
   */
  final class MixSequenceStar(scrutinee:Symbol, column:List[Tree], rest:Rep)(implicit rep:RepFactory) extends MixSequence(scrutinee,column,rest) {
    // in principle, we could optimize more, but variable binding gets complicated (@todo use finite state methods instead)
    override def getSubPatterns(minlen:Int, x:Tree) = x match {
      case av @ ArrayValue(_,xs) if (!isRightIgnoring(av) && xs.length   == minlen) =>  // Seq(p1,...,pN)
        Some(xs ::: gen.mkAttributedRef(definitions.NilModule)  :: EmptyTree :: Nil)
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
      rep.make(vs ::: tail :: scrutinee :: rest.temp, nrows)

    // lengthArg is minimal length
    override protected def getCond(tree:Tree, lengthArg:Int) = seqLongerThan(tree.duplicate, column.head.tpe, lengthArg - 1)
  }


  // @todo: equals test for same constant
  class MixEquals(val scrutinee: Symbol, val column: List[Tree], val rest: Rep)(implicit rep: RepFactory) extends RuleApplication(rep) {
    /** condition (to be used in IF), success and failure Rep */
    final def getTransition(implicit theOwner: Symbol): (Tree, Rep, Symbol, Rep) = {
      val nmatrix = rest
      val vlue = (column.head.tpe: @unchecked) match {
        case TypeRef(_,_,List(SingleType(pre,sym))) => gen.mkAttributedRef(pre,sym)
        case TypeRef(_,_,List(PseudoType(o)))       => o.duplicate
      }
      assert(vlue.tpe ne null, "value tpe is null")
      val vs        = strip1(column.head)
      val nsuccFst  = rest.row.head match { case r => r.insert2(List(EmptyTree), r.subst.add(vs, scrutinee)) }
      val fLabel    = theOwner.newLabel(scrutinee.pos, cunit.fresh.newName(scrutinee.pos, "failCont%")) // warning, untyped
      val sx        = rep.shortCut(fLabel) // register shortcut
      val nsuccRow  = nsuccFst :: Row(getDummies( 1 /*scrutinee*/ + rest.temp.length), NoBinding, EmptyTree, sx) :: Nil

      // todo: optimize if no guard, and no further tests
      val nsucc = rep.make(scrutinee :: rest.temp, nsuccRow)
      val nfail = repWithoutHead(column, rest)

      (typer.typed(Equals(mkIdent(scrutinee) setType scrutinee.tpe, vlue)), nsucc, fLabel, nfail)
    }

    final def tree(implicit theOwner: Symbol, failTree: Tree) = {
      val (cond, srep, fLabel, frep) = this.getTransition
      val cond2 = typer.typed( rep.handleOuter(cond) )
      val fail = typer.typed( repToTree(frep) )
      fLabel setInfo MethodType(Nil, fail.tpe)
      val succ = repToTree(srep)
      typer.typed( If(cond2, succ, LabelDef(fLabel, Nil, fail)) )
    }
  }

  /** mixture rule for type tests
  **/
  class MixTypes(val scrutinee: Symbol, val column: List[Tree], val rest: Rep)(implicit rep: RepFactory) extends RuleApplication(rep) {
    // TODO: this flag is never examined
    val isExhaustive = !scrutinee.tpe.typeSymbol.hasFlag(Flags.SEALED) || {
      val tpes = column.map(x => x.tpe.typeSymbol)
      scrutinee.tpe.typeSymbol.children.forall(sym => tpes.contains(sym))
    }

    private val headPatternType     = strip2(column.head) match {
      case p @ (_:Ident | _:Select) => singleType(p.symbol.tpe.prefix, p.symbol) //should be singleton object
      case __UnApply(_,argtpe,_)    => argtpe
      case _                        => column.head.tpe
    }

    private val isCaseHead = isCaseClass(headPatternType)
    private val dummies = if (!isCaseHead) Nil else getDummies(headPatternType.typeSymbol.caseFieldAccessors.length)

    private def subpatterns(pat: Tree): List[Tree] = pat match {
      case Bind(_,p)                                                  => subpatterns(p)
      case app @ Apply(fn, pats) if isCaseClass(app.tpe) && fn.isType => if (isCaseHead) pats else dummies
      case Apply(fn, xs)                                              => // named constant
        assert(xs.isEmpty && !fn.isType, "strange Apply"); dummies
      // case _: UnApply                                                 => dummies
      case _                                                          => dummies
    }

    /** an approximation of _tp1 <:< tp2 that ignores _ types. this code is wrong,
     *  ideally there is a better way to do it, and ideally defined in Types.scala
     */
    def subsumes_erased(_tp1:Type, tp2:Type) = {
      val tp1 = patternType_wrtEquals(_tp1)
      lazy val eqSymbolsNotArray = (tp1.typeSymbol eq tp2.typeSymbol) && (tp1.typeSymbol ne definitions.ArrayClass)
      tp1.isInstanceOf[TypeRef] &&
      tp2.isInstanceOf[TypeRef] &&
      ((tp1.prefix =:= tp2.prefix) && eqSymbolsNotArray || tp1.parents.exists(_.typeSymbol eq tp2.typeSymbol))
      // rather: tp1.baseTypes.exists...?
    }

    /** returns true if pattern tests an object */
    final def objectPattern(pat:Tree): Boolean = {
      (pat.symbol ne null) &&
      (pat.symbol != NoSymbol) &&
      pat.symbol.tpe.prefix.isStable &&
      headPatternType =:= singleType(pat.symbol.tpe.prefix, pat.symbol)
    }

    // moreSpecific: more specific patterns
    //     subsumed: more general patterns (subsuming current), row index and subpatterns
    //    remaining: remaining, row index and pattern
    def join[T](xs: List[Option[T]]): List[T] = xs.flatMap(x => x)
    val (moreSpecific, subsumed, remaining) : (List[Tree], List[(Int, List[Tree])], List[(Int, Tree)]) = unzip3(
      for ((pat, j) <- column.zipWithIndex) yield {
        val spat = strip2(pat)
        val patType = spat.tpe

        // each pattern will yield a triple of options corresponding to the three lists, which will be flattened down to the values
        spat match {
          case Literal(Constant(null)) if !(headPatternType =:= patType) => // special case for constant null pattern
            (None, None, Some((j, pat)))
          case _ if objectPattern(pat) => // matching an object
            (Some(EmptyTree), Some((j, dummies)), None)
          case Typed(p, _) if (strip2(p).isInstanceOf[UnApply] && (patType <:< headPatternType)) => // <:< is never <equals>
            (Some(p), Some((j, dummies)), None)
          case q @ Typed(pp, _) if patternType_wrtEquals(patType) <:< headPatternType =>
            (Some(if (pat.tpe =:= headPatternType) pp else q), Some((j, dummies)), None)  // never =:= for <equals>
          case z: UnApply =>
            (None, None, Some((j, pat)))
          case qq if subsumes_erased(patType, headPatternType) || (patternType_wrtEquals(patType) <:< headPatternType) && !isDefaultPattern(pat) =>
            (Some(if (pat.tpe =:= headPatternType) EmptyTree else pat), Some((j, subpatterns(pat))), None)  // never =:= for <equals>
          case _ if subsumes_erased(headPatternType, patType) || (headPatternType <:< patType) || isDefaultPattern(pat) =>  // never <:< for <equals>
            (Some(EmptyTree), Some((j, dummies)), Some((j, pat)))  // subsuming (matched *and* remaining pattern)
          case _ =>
            (None, None, Some((j, pat)))
        }
      }
    ) match { case (x,y,z) => (join(x), join(y), join(z)) }

    override def toString = {
      "MixTypes("+scrutinee+":"+scrutinee.tpe+") {\n  moreSpecific:"+moreSpecific+"\n  subsumed:"+subsumed+"\n  remaining"+remaining+"\n}"
    }

    /** returns casted symbol, success matrix and optionally fail matrix for type test on the top of this column */
    final def getTransition(implicit theOwner: Symbol): (Symbol, Rep, Option[Rep]) = {
      val casted = if (scrutinee.tpe =:= headPatternType) scrutinee else newVar(scrutinee.pos, headPatternType)
      propagateFlag(scrutinee, casted, Flags.TRANS_FLAG)
      // succeeding => transition to translate(subsumed) (taking into account more specific)
      val nmatrix = {
        var ntemps =
          if (!isCaseHead) Nil
          else for (meth <- casted.caseFieldAccessors) yield {
            val ctemp = newVar(scrutinee.pos, casted.tpe.memberType(meth).resultType)
            propagateFlag(scrutinee, ctemp, Flags.TRANS_FLAG)
            ctemp
          }
        val subtests =
          if (!moreSpecific.exists(_ != EmptyTree)) subsumed
          else {
            ntemps = casted :: ntemps
            moreSpecific.zip(subsumed) map { case (mspat, (j, pats)) => (j, mspat::pats) }
          }

        ntemps = ntemps ::: rest.temp
        val ntriples = for ((j, pats) <- subtests) yield {
          val (vs, thePat) = strip(column(j))
          val r = rest.row(j)
          val nsubst = r.subst.add(vs, casted)
          r.insert2(pats, nsubst)
        }
        rep.make(ntemps, ntriples)
      }
      // fails      => transition to translate(remaining)
      val nmatrixFail: Option[Rep] = {
        val ntemps   = scrutinee :: rest.temp
        val ntriples = for ((j, pat) <- remaining) yield rest.row(j).insert(pat)
        if (ntriples.isEmpty) None else Some(rep.make(ntemps, ntriples))
      }
      (casted, nmatrix, nmatrixFail)
    }

    final def tree(implicit theOwner: Symbol, failTree: Tree): Tree = {
      val (casted, srep, frep) = this.getTransition
      val condUntyped = condition(casted.tpe, this.scrutinee)
      var cond = rep.handleOuter(typer.typed { condUntyped })
      if (needsOuterTest(casted.tpe, this.scrutinee.tpe, theOwner)) {  // @todo merge into def condition
        cond = addOuterCondition(cond, casted.tpe, mkIdent(this.scrutinee), rep.handleOuter)
      }
      val succ = repToTree(srep)
      val fail = frep.map(repToTree) getOrElse failTree

      // dig out case field accessors that were buried in (***)
      val cfa  = if (!isCaseHead) Nil else casted.caseFieldAccessors
      val caseTemps = (if (!srep.temp.isEmpty && srep.temp.head == casted) srep.temp.tail else srep.temp).zip(cfa)

      var vdefs = for ((tmp, accessorMethod) <- caseTemps) yield {
        val untypedAccess = Apply(Select(mkIdent(casted), accessorMethod), Nil)
        val typedAccess = typer.typed(untypedAccess)
        typedValDef(tmp, typedAccess)
      }

      if (casted ne this.scrutinee)
        vdefs = ValDef(casted, gen.mkAsInstanceOf(mkIdent(this.scrutinee), casted.tpe)) :: vdefs

      return typer.typed( If(cond, Block(vdefs, succ), fail) )
    }
  }

  /** converts given rep to a tree - performs recursive call to translation in the process to get sub reps
   */
  final def repToTree(r: Rep)(implicit theOwner: Symbol, failTree: Tree, rep: RepFactory): Tree =
    r.applyRule.tree

  case class Row(pat:List[Tree], subst: Binding, guard: Tree, bx: Int) {
    def insert(h: Tree) = Row(h :: pat, subst, guard, bx)                   // prepends supplied tree
    def insert(hs: List[Tree]) = Row(hs ::: pat, subst, guard, bx)
    def insert2(hs: List[Tree], b: Binding) = Row(hs ::: pat, b, guard, bx) // prepends and substitutes
    def replace(hs: List[Tree]) = Row(hs, subst, guard, bx)                 // replaces pattern list
  }

  object Rep {
    type RepType = Product2[List[Symbol], List[Row]]
    final def unapply(x:Rep)(implicit rep:RepFactory): Option[RepType] =
      if (x.isInstanceOf[rep.RepImpl]) Some(x.asInstanceOf[RepType]) else None
  }

  class RepFactory(val handleOuter: Tree => Tree)(implicit val typer : Typer) {
    case class RepImpl(val temp:List[Symbol], val row:List[Row]) extends Rep with Rep.RepType {
      (row.find { case Row(pats, _, _, _) => temp.length != pats.length }) match {
        case Some(row) => assert(false, "temp == "+temp+" row.pats == "+row.pat);
        case _ =>
      }
      def _1 = temp
      def _2 = row
    }

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
            if (bx >= 0 && !isReachedTwice(bx)) Block(vdefs,body)
            else blck

          case If(cond, Literal(Constant(true)), Literal(Constant(false))) =>
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
    final def requestBody(bx:Int, subst:Binding)(implicit theOwner: Symbol): Tree = {
      if (bx < 0) { // is shortcut
        val jlabel = shortCuts(-bx-1)
        return Apply(mkIdent(jlabel), Nil)
      }
      if (!isReached(bx)) { // first time this bx is requested
        // might be bound elsewhere ( see `x @ unapply' ) <-- this comment refers to null check
        val (vsyms, argts, vdefs) : (List[Symbol], List[Type], List[Tree]) = unzip3(
          for (v <- vss(bx) ; val substv = subst(v) ; if substv ne null) yield
            (v, v.tpe, typedValDef(v, substv))
        )

        val body  = targets(bx)
        // @bug: typer is not able to digest a body of type Nothing being assigned result type Unit
        val tpe = if (body.tpe.typeSymbol eq definitions.NothingClass) body.tpe else resultType
        val label = theOwner.newLabel(body.pos, "body%"+bx) setInfo MethodType(argts, tpe)
        labels(bx) = label

        return body match {
          case _: Throw | _: Literal => Block(vdefs, body.duplicate setType tpe)
          case _ => Block(vdefs.reverse, LabelDef(label, vsyms, body setType tpe))
        }
      }

      // if some bx is not reached twice, its LabelDef is replaced with body itself
      markReachedTwice(bx)
      val args: List[Ident] = vss(bx).map(subst)
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
          val vdefs = for (v <- vss(bx) ; val substv = subst(v) ; if substv ne null) yield typedValDef(v, substv)
          Block(vdefs, body.duplicate setType resultType)
        case _ =>
          Apply(mkIdent(label),args)
      }
    }

    /** the injection here handles alternatives and unapply type tests */
    final def make(temp: List[Symbol], row1: List[Row])(implicit theOwner: Symbol): Rep = {
      var unchanged: Boolean = true
      // equals check: call singleType(NoPrefix, o.symbol) `stpe'. Then we could also return
      // `typeRef(definitions.ScalaPackageClass.tpe, definitions.EqualsPatternClass, List(stpe))'
      // and force an equality check. However, exhaustivity checking would not work anymore.
      // so first, extend exhaustivity check to equalspattern
      def sType(o: Tree) = singleType(o.tpe.prefix, o.symbol)
      def equalsCheck(o: Tree) =if (o.symbol.isValue) singleType(NoPrefix, o.symbol) else sType(o)

      def classifyPat(opat: Tree, j: Int): Tree = {
        val (vs, strippedPat) = strip(opat) match { case (vset, pat) => (vset.toList, pat) }

        (strippedPat: @unchecked) match {
          case p @ Alternative(ps) =>
            DBG("Alternative") ; opat
          case typat @ Typed(p, tpt) if strip2(p).isInstanceOf[UnApply]=>
            DBG("Typed")
            if (temp(j).tpe <:< tpt.tpe) makeBind(vs, p) else opat

          case Ident(nme.WILDCARD) | EmptyTree | _:Literal | _:Typed =>
            DBG("Ident(_)|EmptyTree") ; opat
          case o @ Ident(n) => // n != nme.WILDCARD
            DBG("Ident")
            val tpe = equalsCheck(o)
            val p = Ident(nme.WILDCARD) setType tpe
            val q = Typed(p, TypeTree(tpe)) setType tpe
            makeBind(vs, q) setType tpe

          case o @ Select(stor,_) =>
            DBG("Select")
            val stpe = equalsCheck(o)
            val p = Ident(nme.WILDCARD) setType stpe
            makeBind(vs, Typed(p, TypeTree(stpe)) setType stpe) setType stpe

          case UnApply(Apply(TypeApply(sel @ Select(stor, nme.unapplySeq), List(tptArg)),_),ArrayValue(_,xs)::Nil)
          if (stor.symbol eq definitions.ListModule) =>
            DBG("Unapply(...TypeApply...)")
            // @pre: is not right-ignoring (no star pattern)
            // no exhaustivity check, please
            temp(j) setFlag Flags.TRANS_FLAG
            val listType = typeRef(mkThisType(definitions.ScalaPackage), definitions.ListClass, List(tptArg.tpe))
            makeBind(vs, normalizedListPattern(xs, tptArg.tpe))

          // @todo: rewrite, using __UnApply instead of UnApply like so:
          // case  ua @ __UnApply(_,argtpe,_) =>
          //   val ua = prepat
          //   val npat = (if (temp(j).tpe <:< argtpe) ua else Typed(ua,TypeTree(argtpe)).setType(argtpe))
          //   pats = (makeBind(vs, npat) setType argtpe)::pats
          case ua @ UnApply(Apply(fn, _), _) =>
            DBG("Unapply(Apply())")
            val MethodType(List(argtpe, _*), _) = fn.tpe
            val npat = if (temp(j).tpe <:< argtpe) ua else Typed(ua, TypeTree(argtpe)).setType(argtpe)
            makeBind(vs, npat) setType argtpe

          case o @ Apply(fn, Nil) if !isCaseClass(o.tpe) || /*see t301*/ !Apply_Value.unapply(o).isEmpty =>
            DBG("Apply !isCaseClass")
            val stpe: Type = fn match {
              case _ if o.symbol.isModule || o.tpe.termSymbol.isModule => sType(o)
              case Select(path, sym) => path.tpe match {
                case t @ ThisType(sym) => singleType(t, o.symbol)
                // next two cases: e.g. `case Some(p._2)' in scala.collection.jcl.Map
                case _ if path.isInstanceOf[Apply] => PseudoType(o)       // outer-matching: test/files/pos/t154.scala
                case _ => singleType(sType(path), o.symbol)               // old
              }
              case o: Ident => equalsCheck(o)
            }
            val ttst = typeRef(NoPrefix, definitions.EqualsPatternClass, List(stpe))
            val p = Ident(nme.WILDCARD) setType ttst
            makeBind(vs, Typed(p, TypeTree(stpe)) setType ttst)

          case Apply_Value(pre, sym) =>
            DBG("Apply_Value")
            val tpe = typeRef(NoPrefix, definitions.EqualsPatternClass, List(singleType(pre, sym)))
            makeBind(vs, Typed(EmptyTree, TypeTree(tpe)) setType tpe)

          case Apply_CaseClass_NoArgs(tpe) => // no-args case class pattern
            DBG("Apply_CaseClass_NoArgs")
            makeBind(vs, Typed(EmptyTree, TypeTree(tpe)) setType tpe)

          case Apply_CaseClass_WithArgs() =>  // case class pattern with args
            DBG("Apply_CaseClass_WithArgs") ; opat
          case ArrayValue(_,_) =>
            DBG("ArrayValue") ; opat
        }
      }

      val row = row1 flatMap { xx =>
        def isAlternative(p: Tree): Boolean = p match {
          case Bind(_,p)       => isAlternative(p)
          case Alternative(ps) => true
          case _               => false
        }
        def getAlternativeBranches(p: Tree): List[Tree] = {
          def get_BIND(pctx:Tree => Tree, p:Tree):List[Tree] = p match {
            case b @ Bind(n,p)   => get_BIND({ x:Tree => pctx(copy.Bind(b, n, x) setType x.tpe) }, p)
            case Alternative(ps) => ps map pctx
          }
          get_BIND(x => x, p)
        }
        val Row(opats, subst, g, bx) = xx
        val indexOfAlternative = opats.findIndexOf(isAlternative)
        if (indexOfAlternative != -1) unchanged = false
        val pats: List[Tree] = opats.zipWithIndex.map { case (opat, j) => classifyPat(opat, j) }

        if (indexOfAlternative == -1) List(xx.replace(pats))
        else {
          val (prefix, alts :: suffix) = pats.splitAt(indexOfAlternative)
          getAlternativeBranches(alts) map { p => xx.replace(prefix ::: p :: suffix) }
        }
      }

      if (unchanged) RepImpl(temp, row).init
      else this.make(temp, row) // recursive call
    }
  }

  abstract class Rep {
    val temp: List[Symbol]
    val  row: List[Row]

    final def init: this.type = {
      val setsToCombine: List[(Int, immutable.Set[Symbol])] =
        for {
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
        } // .reverse ? XXX

      if (setsToCombine.isEmpty) return this

      //  computes cartesian product, keeps indices available
      def combine(colcom: List[(Int, Set[Symbol])]): List[List[(Int, Symbol)]] = colcom match {
        case Nil => Nil
        case (i,syms)::Nil => syms.toList.map { sym => List((i,sym)) }
        case (i,syms)::cs  => for (s <- syms.toList; rest <- combine(cs)) yield (i,s) :: rest
      }

      val allcomb = combine(setsToCombine)

      /** returns true if pattern vector pats covers a type symbols "combination"
       *  @param pats pattern vector
       *  @param comb pairs of (column index, type symbol)
       */
      def covers(pats: List[Tree], comb: List[(Int, Symbol)]) = {
        val results = for ((i, sym) <- comb ; val p = strip2(pats(i))) yield p match {
          case _ if isDefaultPattern(p)   => true
          case _: UnApply | _: ArrayValue => true
          case _ =>
            val ptpe = patternType_wrtEquals(p.tpe)
            val symtpe =
              if ((sym hasFlag Flags.MODULE) && (sym.linkedModuleOfClass ne NoSymbol))
                singleType(sym.tpe.prefix, sym.linkedModuleOfClass)   // e.g. None, Nil
              else sym.tpe

            (ptpe.typeSymbol == sym) ||
            (symtpe <:< ptpe) ||
            (symtpe.parents.exists(_.typeSymbol eq ptpe.typeSymbol)) || // e.g. Some[Int] <: Option[&b]
            (ptpe.prefix.memberType(sym) <:< ptpe)  // outer, see combinator.lexical.Scanner
        }
        results.forall(_ == true)
      }

      def comboCovers(combo: List[(Int, Symbol)]) = row exists { r => (r.guard eq EmptyTree) && covers(r.pat, combo) }

      if (!(allcomb forall comboCovers)) {
        def mkMissingStr(xs: List[(Int, Symbol)], i: Int) = xs.find(_._1 == i) match {
          case None => pad("*")
          case Some(pair) => pad(pair._2.name.toString)
        }

        val missingCombos =
          (for (open <- allcomb ; if row.forall(r => !covers(r.pat, open))) yield
            "missing combination " +
              (for (i <- 0 until temp.length) yield
                mkMissingStr(open, i)).mkString + "\n").mkString

        cunit.warning(temp.head.pos, "match is not exhaustive!\n" + missingCombos)
      }

      return this
    }

    /*   internal representation is (temp:List[Symbol], row:List[Row])
     *
     *         tmp1       tmp_m
     */
    final def applyRule(implicit theOwner: Symbol, rep: RepFactory): RuleApplication = row match {
      case Nil =>
        ErrorRule()
      case Row(pats, subst, g, bx) :: xs =>
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
            val mr        = MixtureRule(t, column, rep.make(restTemp, restRows))
            DBG("\n---\nmixture rule is = " + mr.getClass)
            return mr
          }
        }
        //Row(   _   ...   _     g_1  b_1 ) :: rows     it's all default patterns
        val rest = if (g eq EmptyTree) null else rep.make(temp, xs)
        DBG("\n---\nmixture rule is = VariableRule")
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
          case _ if roots.length <= 1 => Some(Row(List(pat), NoBinding, g, bx))
          case Apply(fn, pargs)       => Some(Row(pargs, NoBinding, g, bx))
          case Ident(nme.WILDCARD)    => Some(Row(getDummies(roots.length), NoBinding, g, bx))
          case _                      => None
        }
        (rowForPat, b, definedVars(pat))
      }
    )

    // flatMap the list of options yields the list of values
    rep.make(roots, rows.flatMap(x => x), targets, vss)
  }

  final def newVar(pos: Position, name: Name, tpe: Type)(implicit theOwner: Symbol): Symbol = {
    assert(tpe ne null, "newVar("+name+", null)")
    val sym = theOwner.newVariable(pos, name) // careful: pos has special meaning
    sym setInfo tpe
    sym
  }

  final def newVar(pos: Position, tpe: Type)(implicit theOwner: Symbol): Symbol =
    newVar(pos, cunit.fresh.newName(pos, "temp"), tpe) setFlag Flags.SYNTHETIC

  /** returns the condition in "if (cond) k1 else k2"
   */
  final def condition(tpe: Type, scrut: Symbol)(implicit typer : Typer): Tree = {
    assert(scrut ne NoSymbol)
    condition(tpe, mkIdent(scrut))
  }

  final def condition(tpe: Type, scrutineeTree: Tree)(implicit typer : Typer): Tree = {
    assert((tpe ne NoType) && (scrutineeTree.tpe ne NoType))

    tpe match {
      case _: SingletonType if !tpe.isInstanceOf[ConstantType] =>
        lazy val equalsRef = Equals(gen.mkAttributedRef(tpe.termSymbol), scrutineeTree)
        if (tpe.termSymbol.isModule) equalsRef      // object
        else if (tpe.prefix ne NoPrefix) typer.typed(gen.mkIsInstanceOf(scrutineeTree, tpe))
        else typer.typed(equalsRef)

      case ct: ConstantType => ct.value match {     // constant
          case v @ Constant(null) if scrutineeTree.tpe <:< definitions.AnyRefClass.tpe => Eq(scrutineeTree, Literal(v))
          case v => Equals(scrutineeTree, Literal(v))
        }
      case _ if scrutineeTree.tpe <:< tpe && tpe <:< definitions.AnyRefClass.tpe =>
        NotNull(scrutineeTree)
      case _ =>
        gen.mkIsInstanceOf(scrutineeTree, tpe)
    }
  }

  /** adds a test comparing the dynamic outer to the static outer */
  final def addOuterCondition(cond:Tree, tpe2test: Type, scrutinee: Tree, handleOuter: Tree=>Tree) = {
    val TypeRef(prefix,_,_) = tpe2test
    assert(prefix ne NoPrefix)
    var theRef = prefix match {
      case ThisType(clazz) => gen.mkAttributedThis(clazz)
      case _               => gen.mkAttributedRef(prefix.prefix, prefix.termSymbol)
    }
    // needs explicitouter treatment
    theRef = handleOuter(theRef)

    val outerAcc = outerAccessor(tpe2test.typeSymbol)
    if (outerAcc == NoSymbol) {
      if (settings.debug.value) cunit.warning(scrutinee.pos, "no outer acc for "+tpe2test.typeSymbol)
      cond
    } else
      And(cond,
          Eq(Apply(Select(
            gen.mkAsInstanceOf(scrutinee, tpe2test, true), outerAcc), Nil), theRef))
  }

}
