/* NSC -- new Scala compiler
 * Copyright 2005-2007 LAMP/EPFL
 * Copyright 2007 Google Inc. All Rights Reserved.
 * Author: bqe@google.com (Burak Emir)
 */
// $Id$

package scala.tools.nsc.matching

import util.Position
import collection.mutable.{ListBuffer, BitSet}
import collection.immutable.IntMap

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

  import global._
  import typer.typed
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
          simpleSwitchCandidate(last) || isDefaultPattern(last);
        }}

    // an unapply for which we don't need a type test
    def isUnapplyHead(): Boolean = column.head match {
      case __UnApply(_,argtpe,_) => scrutinee.tpe <:< argtpe
      case _                   => false
    }

    // true if pattern type is direct subtype of scrutinee (can't use just <:< cause have to take variance into account)
    def directSubtype(ptpe: Type) =
      (ptpe.parents.exists { x => ((x.typeSymbol eq scrutinee.tpe.typeSymbol) && (x <:< scrutinee.tpe))});

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

    if (isEqualsPattern(column.head.tpe))
      new MixEquals(scrutinee, column, rest)
    else if (column.head.isInstanceOf[ArrayValue]) {
      if (isRightIgnoring(column.head.asInstanceOf[ArrayValue]))
        new MixSequenceStar(scrutinee, column, rest)
      else
        new MixSequence(scrutinee, column, rest)
    }
    else if (isSimpleSwitch)
      new MixLiterals(scrutinee, column, rest)
    else if (settings_casetags && (column.length > 1) && isFlatCases(column))
      new MixCases(scrutinee, column, rest)
    else if (isUnapplyHead())
      new MixUnapply(scrutinee, column, rest)
    else
      new MixTypes(scrutinee, column, rest)
  }

  sealed abstract class RuleApplication(rep: RepFactory) {
    def scrutinee:Symbol

    // used in MixEquals and MixSequence
    final protected def repWithoutHead(col: List[Tree],rest: Rep)(implicit theOwner: Symbol): Rep = {
      val nfailrow = List.map2(col.tail, rest.row.tail)((p, r) => r match {
        case Row(pats, binds, g, bx) => Row(p::pats, binds, g, bx);
      });
      rep.make(scrutinee::rest.temp, nfailrow)
    }

    /** translate outcome of the rule application into code (possible involving recursive application of rewriting) */
    def tree(implicit theOwner: Symbol, failTree: Tree): Tree
  }

  case class ErrorRule(implicit rep:RepFactory) extends RuleApplication(rep) {
    def scrutinee:Symbol = throw new RuntimeException("this never happens")
    final def tree(implicit theOwner: Symbol, failTree: Tree) = failTree
  }

  /**  {case ... if guard => bx} else {guardedRest} */
  case class VariableRule(subst:Binding, guard: Tree, guardedRest:Rep, bx: Int)(implicit rep:RepFactory) extends RuleApplication(rep) {
    def scrutinee:Symbol = throw new RuntimeException("this never happens")
    final def tree(implicit theOwner: Symbol, failTree: Tree): Tree = {
      val body = typed { rep.requestBody(bx, subst) }
      if (guard eq EmptyTree)
        return body
      val vdefs = targetParams(subst)
      val typedElse = repToTree(guardedRest)
      val typedIf = typed { If(guard.duplicate, body, typedElse) }

      typer.typed { squeezedBlock(vdefs, typedIf) }
    }
  }

  /** superclass of mixture rules for case classes and literals (both translated to switch on an integer)
   */
  abstract class CaseRuleApplication(rep:RepFactory) extends RuleApplication(rep) {
    def column: List[Tree]
    def rest:Rep

    // e.g. (1,1) (1,3) (42,2) for column {case ..1.. => ;; case ..42..=> ;; case ..1.. => }
    protected var defaults: List[Int]    = Nil
    var defaultV: collection.immutable.Set[Symbol] = emptySymbolSet

    lazy val defaultRows: List[Row] =
      defaults.reverseMap(grabRow);

    // sorted e.g. case _ => 7,5,1
    protected def insertDefault(tag: Int,vs:Set[Symbol]) {
      defaultV = defaultV ++ vs
      def insertSorted(tag: Int, xs:List[Int]):List[Int] = xs match {
        case y::ys if y > tag => y::insertSorted(tag, ys)
        case ys               => tag :: ys
      }

      defaults = insertSorted(tag, defaults)
    }

    protected def haveDefault: Boolean = !defaults.isEmpty
    protected var tagIndices = IntMap.empty[List[Int]]

    protected def grabTemps: List[Symbol] = rest.temp
    protected def grabRow(index:Int): Row = rest.row(index) match {
      case r @ Row(pats, s, g, bx) => if (defaultV.isEmpty) r else {
        val vs = strip1(column(index))  // get vars
        val nbindings = s.add(vs.elements, scrutinee)
        Row(pats, nbindings, g, bx)
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
     *  @return optionally, a default continuation,
     **/
    def getTransition(implicit theOwner: Symbol): (List[(Int,Rep)],Set[Symbol],Option[Rep]) =
      (tagIndicesToReps, defaultV, {if (haveDefault) Some(defaultsToRep) else None})
  }

  /** mixture rule for flat case class (using tags)
   *  this rule gets translated to a switch of _.$tag()
  **/
  class MixCases(val scrutinee:Symbol, val column:List[Tree], val rest:Rep)(implicit rep:RepFactory) extends CaseRuleApplication(rep) {

    /** insert row indices into list of tagIndices */
    for ((x, i) <- column.zipWithIndex; val p = strip2(x))
      if (isDefaultPattern(p))
        insertDefault(i, strip1(x))
      else
        insertTagIndexPair(getCaseTag(p.tpe), i)

    override def grabTemps = scrutinee::rest.temp

    override def grabRow(index: Int) = {
      val firstValue = tagIndices(tagIndices.firstKey).head
      rest.row(firstValue) match {
        case Row(pats, s, g, bx) =>
          val nbindings = s.add(strip1(column(index)).elements, scrutinee)
          Row(column(firstValue)::pats, nbindings, g, bx)
      }
    }

    final def tree(implicit theOwner: Symbol, failTree: Tree): Tree = {
      val (branches, defaultV, default) = getTransition // tag body pairs

      var ndefault = if (default.isEmpty) failTree else repToTree(default.get)
      var cases = branches map {
        case (tag, r) =>
          CaseDef(Literal(tag),
                  EmptyTree,
                  {
                    val pat  = this.column(tagIndices(tag).head);
                    val ptpe = pat.tpe
                    if (this.scrutinee.tpe.typeSymbol.hasFlag(Flags.SEALED) && strip2(pat).isInstanceOf[Apply]) {
                      //cast
                      val vtmp = newVar(pat.pos, ptpe)
                      squeezedBlock(
                        List(typedValDef(vtmp, gen.mkAsInstanceOf(mkIdent(this.scrutinee), ptpe))),
                        repToTree(rep.make(vtmp :: r.temp.tail, r.row))
                      )
                    } else repToTree(r)
                  }
                )}

      // make first case a default case.
      if (this.scrutinee.tpe.typeSymbol.hasFlag(Flags.SEALED) && defaultV.isEmpty) {
        ndefault = cases.head.body
        cases = cases.tail
      }

      cases.length match {
        case 0 => ndefault
        case 1 => val CaseDef(lit,_,body) = cases.head
                  If(Equals(Select(mkIdent(this.scrutinee), nme.tag), lit), body, ndefault)
        case _ => val defCase = CaseDef(mk_(definitions.IntClass.tpe), EmptyTree, ndefault)
                  Match(Select(mkIdent(this.scrutinee),nme.tag), cases :::  defCase :: Nil)
      }
    }
  }

  /** mixture rule for literals
   */
  class MixLiterals(val scrutinee:Symbol, val column:List[Tree], val rest:Rep)(implicit rep:RepFactory) extends CaseRuleApplication(rep) {

    private var defaultIndexSet = new BitSet(column.length)

    override def insertDefault(tag: Int, vs: Set[Symbol]) {
      defaultIndexSet += tag
      defaultV = defaultV ++ vs
    }

    protected override def haveDefault: Boolean = !defaultIndexSet.isEmpty

    override lazy val defaultRows: List[Row] =
        defaultIndexSet.filter(defaultIndexSet(_)).toList.reverseMap(grabRow)

    var varMap: List[(Int,List[Symbol])] = Nil

    private def sanity(pos:Position, tag: Int, pvars:List[Symbol]) {
      varMap = (tag,pvars)::varMap
    }

    //lazy
    private def bindVars(Tag:Int, orig: Binding): Binding  = {
      def myBindVars(rest:List[(Int,List[Symbol])], bnd: Binding): Binding  = rest match {
        case Nil => bnd
        case (Tag,vs)::xs => myBindVars(xs, bnd.add(vs.elements, scrutinee))
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
      val (branches, defaultV, defaultRepOpt) = this.getTransition // tag body pairs
      val cases = branches map {
        case (tag, r) =>
          val r2 = rep.make(r.temp, r.row map { case Row(pat, bnd, g, bx) => Row(pat, bindVars(tag, bnd), g, bx) })
          val t2 = repToTree(r2)
          CaseDef(Literal(tag), EmptyTree, t2)
      }

      lazy val ndefault = defaultRepOpt.map(repToTree) getOrElse failTree
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
  class MixUnapply(val scrutinee: Symbol, val column: List[Tree], val rest: Rep)(implicit rep: RepFactory) extends RuleApplication(rep) {

    def newVarCapture(pos:Position,tpe:Type)(implicit theOwner:Symbol) = {
      val v = newVar(pos,tpe)
      if (scrutinee.hasFlag(Flags.TRANS_FLAG))
        v.setFlag(Flags.TRANS_FLAG) // propagate "unchecked"
      v
    }

    private def bindToScrutinee(x:Symbol) = typedValDef(x,mkIdent(scrutinee))

    val (vs,unapp) = strip(column.head)

    /** returns (unapply-call, success-rep, optional fail-rep*/
    final def getTransition(implicit theOwner: Symbol): (Tree, List[Tree], Rep, Option[Rep]) = {
      unapp match {
        case ua @ UnApply(app @ Apply(fn, appargs), args) =>
          object sameUnapplyCall {
            def unapply(t:Tree) = t match {
              case UnApply(Apply(fn1,_), differentArgs) if (fn.symbol == fn1.symbol) && fn.equalsStructure(fn1) =>
                Some(differentArgs)
              case _ =>
                None
            }
          }
          val ures = newVarCapture(ua.pos, app.tpe)
          val arg0 = mkIdent(scrutinee)
          val rhs = Apply(fn, arg0 :: appargs.tail) setType ures.tpe
          val uacall = typedValDef(ures, rhs)

          val nrowsOther = column.tail.zip(rest.row.tail) flatMap {
            case (pat, Row(ps, subst, g, bx)) =>
              strip2(pat) match {
                case sameUnapplyCall(_) => Nil
                case _                  => List(Row(pat::ps, subst, g, bx))
              }}
          val nrepFail = if (nrowsOther.isEmpty)
                           None
                         else
                           Some(rep.make(scrutinee::rest.temp, nrowsOther))
          args.length match {
            case 0  => // special case for unapply(), app.tpe is boolean
              val ntemps = scrutinee :: rest.temp
              val nrows  = column.zip(rest.row) map {
                case (pat, Row(ps, subst, g, bx)) =>
                  strip2(pat) match {
                    case sameUnapplyCall(args) =>
                      val nsubst = subst.add(strip1(pat).elements, scrutinee)
                      Row(EmptyTree::ps, nsubst, g, bx)
                    case _ =>
                      Row(     pat ::ps, subst, g, bx)
                  }}
            (uacall, Nil, rep.make(ntemps, nrows), nrepFail)

            case  1 => // special case for unapply(p), app.tpe is Option[T]
              val vtpe = app.tpe.typeArgs(0)
              val vsym = newVarCapture(ua.pos, vtpe)
              val ntemps = vsym :: scrutinee :: rest.temp
              val nrows = column.zip(rest.row) map {
                case (pat, Row(ps, subst, g, bx)) =>
                  strip2(pat) match {
                    case sameUnapplyCall(args) =>
                      val nsubst = subst.add(strip1(pat).elements, scrutinee)
                      Row(args(0)   :: EmptyTree :: ps, nsubst, g, bx)
                    case _ =>
                      Row(EmptyTree ::  pat      :: ps, subst, g, bx)
                  }}
              val vdef = typedValDef(vsym, Select(mkIdent(ures), nme.get))
              (uacall, List(vdef), rep.make(ntemps, nrows), nrepFail)

            case _ => // app.tpe is Option[? <: ProductN[T1,...,Tn]]
              val uresGet = newVarCapture(ua.pos, app.tpe.typeArgs(0))
              val vdefs = new ListBuffer[Tree]
              vdefs += typedValDef(uresGet, Select(mkIdent(ures), nme.get))
              var ts = definitions.getProductArgs(uresGet.tpe).get
              var i = 1;
              val vsyms = new ListBuffer[Symbol]
              while(ts ne Nil) {
                val vtpe = ts.head
                val vchild = newVarCapture(ua.pos, vtpe)
                val accSym = definitions.productProj(uresGet, i)
                val rhs = typed(Apply(Select(mkIdent(uresGet), accSym), List()))
                vdefs += typedValDef(vchild, rhs)
                vsyms += vchild
                ts = ts.tail
                i += 1
              }
              val ntemps  = vsyms.toList ::: scrutinee :: rest.temp
              val dummies = getDummies(i - 1)
              val nrows = column.zip(rest.row) map {
                case (pat, Row(ps, subst, g, bx)) =>
                  strip2(pat) match {
                    case sameUnapplyCall(args) =>
                      val nsubst = subst.add(strip1(pat).elements, scrutinee)
                      Row(   args::: EmptyTree ::ps, nsubst, g, bx)
                    case _ =>
                      Row(dummies:::     pat   ::ps, subst, g, bx)
                  }}

              (uacall, vdefs.toList, rep.make(ntemps, nrows), nrepFail)
          }}
    } /* def getTransition(...) */

    final def tree(implicit theOwner: Symbol, failTree: Tree) = {
      val (uacall , vdefs,srep,frep) = this.getTransition
      val succ = repToTree(srep)
      val fail = if (frep.isEmpty) failTree else repToTree(frep.get)
      val cond =
        if (uacall.symbol.tpe.typeSymbol eq definitions.BooleanClass)
          typed{ mkIdent(uacall.symbol) }
        else
          emptynessCheck(uacall.symbol)
      typed { squeezedBlock(List(rep.handleOuter(uacall)), If(cond,squeezedBlock(vdefs,succ),fail)) }
    }
  }

  /** handle sequence pattern and ArrayValue (but not star patterns)
   */
  sealed class MixSequence(val scrutinee: Symbol, val column: List[Tree], val rest: Rep)(implicit rep: RepFactory) extends RuleApplication(rep) {

    private val sequenceType = scrutinee.tpe.widen.baseType(definitions.SeqClass)
    private val elementType  = getElemType_Sequence(scrutinee.tpe)

    final def removeStar(xs:List[Tree]):List[Tree] =
      xs.take(xs.length-1) ::: makeBind(strip1(xs.last).toList, mk_(sequenceType)) :: Nil

    protected def getSubPatterns(len:Int, x:Tree):Option[List[Tree]] = x match {
      case av @ ArrayValue(_,xs) if (!isRightIgnoring(av) && xs.length == len)   => Some(xs ::: List(EmptyTree))
      case av @ ArrayValue(_,xs) if ( isRightIgnoring(av) && xs.length == len+1) => Some(removeStar(xs)) // (*)
      case EmptyTree | Ident(nme.WILDCARD)                                       => Some(getDummies(len+1))
      case _                                                                     => None
    }

    protected def makeSuccRep(vs:List[Symbol], tail:Symbol, nrows:List[Row])(implicit theOwner: Symbol) =
      rep.make( vs ::: tail :: rest.temp, nrows.toList)

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
          typed(gen.mkAsInstanceOf(mkIdent(scrutinee), column.head.tpe, true))
        else
          mkIdent(scrutinee)

      val av @ ArrayValue(_, xs) = column.head
      val ys = if (isRightIgnoring(av)) xs.init else xs
      val vs = ys map(y => newVar(strip2(y).pos, elementType))

      lazy val tail = newVar(scrutinee.pos, sequenceType)
      lazy val lastBinding = if (ys.size > 0) seqDrop(treeAsSeq.duplicate, ys.size) else mkIdent(scrutinee)
      val bindings =
        (vs.zipWithIndex map { case (v, i) => typedValDef(v, seqElement(treeAsSeq.duplicate, i)) }) :::
        List(typedValDef(tail, lastBinding))

      val nrows = new ListBuffer[Row]
      val frows = new ListBuffer[Row]

      for ((c, Row(pats,subst,g,b)) <- column.zip(rest.row)) {
        def add(hs: Tree*) : Row = Row(hs.toList ::: pats, subst, g, b)

        getSubPatterns(ys.size, c) match {
          case Some(ps) => nrows += add(ps:_*) ; if (isDefaultPattern(c) || subsumes(c, av)) frows += add(c)
          case None => frows += add(c)
        }
      }

      val succRep = makeSuccRep(vs, tail, nrows.toList)
      val failRep = rep.make(scrutinee :: rest.temp, frows.toList)

      // fixed length
      val cond = getCond(treeAsSeq, xs.length)
      return ({thenp:Tree => {elsep:Tree =>
        If(cond, squeezedBlock(bindings, thenp), elsep)}}, succRep, failRep)
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
      rep.make( vs ::: tail :: scrutinee :: rest.temp, nrows)

    // lengthArg is minimal length
    override protected def getCond(tree:Tree, lengthArg:Int) = seqLongerThan(tree.duplicate, column.head.tpe, lengthArg - 1)
  }


  // @todo: equals test for same constant
  class MixEquals(val scrutinee: Symbol, val column: List[Tree], val rest: Rep)(implicit rep: RepFactory) extends RuleApplication(rep) {
    /** condition (to be used in IF), success and failure Rep */
    final def getTransition(implicit theOwner: Symbol): (Tree, Rep, Symbol, Rep) = {
      val nmatrix = rest
      val vlue = (column.head.tpe: @unchecked) match {
        case TypeRef(_,_,List(SingleType(pre,sym))) =>
          gen.mkAttributedRef(pre,sym)
        case TypeRef(_,_,List(PseudoType(o))) =>
          o.duplicate
      }
      assert(vlue.tpe ne null, "value tpe is null")
      val vs = strip1(column.head)
      val nsuccFst = rest.row.head match { case Row(pats,bnd,g,b) => Row(EmptyTree::pats, bnd.add(vs.elements, scrutinee),g,b) }
      val fLabel = theOwner.newLabel(scrutinee.pos, cunit.fresh.newName(scrutinee.pos, "failCont%")) // warning, untyped
      val sx     = rep.shortCut(fLabel) // register shortcut
      val nsuccRow = nsuccFst :: Row(getDummies( 1 /*scrutinee*/ + rest.temp.length), NoBinding, EmptyTree, sx) :: Nil

      // todo: optimize if no guard, and no further tests
      val nsucc = rep.make(scrutinee :: rest.temp, nsuccRow)
      val nfail = repWithoutHead(column, rest)
      return (typed{ Equals(mkIdent(scrutinee) setType scrutinee.tpe, vlue) }, nsucc, fLabel, nfail)
    }

    final def tree(implicit theOwner: Symbol, failTree: Tree) = {
      val (cond, srep, fLabel, frep) = this.getTransition
      val cond2 = typed { rep.handleOuter(cond) }
      val fail = typed { repToTree(frep) }
      fLabel setInfo (new MethodType(Nil, fail.tpe))
      val succ = repToTree(srep)
      typed{ If(cond2, succ, LabelDef(fLabel, Nil, fail)) }
    }
  }

  /** mixture rule for type tests
  **/
  class MixTypes(val scrutinee: Symbol, val column: List[Tree], val rest: Rep)(implicit rep: RepFactory) extends RuleApplication(rep) {

    var casted: Symbol = null
    var moreSpecific: List[Tree] = Nil
    var subsumed:  List[(Int,List[Tree])] = Nil  // row index and subpatterns
    var remaining: List[(Int,Tree)] = Nil  // row index and pattern

    val isExhaustive = !scrutinee.tpe.typeSymbol.hasFlag(Flags.SEALED) || {
      val tpes = column.map {x => x.tpe.typeSymbol}
      scrutinee.tpe.typeSymbol.children.forall { sym => tpes.contains(sym) }
    }

    private val headPatternType     = strip2(column.head) match {
      case p @ (_:Ident | _:Select) => singleType(p.symbol.tpe.prefix, p.symbol) //should be singleton object
      case __UnApply(_,argtpe,_)    => argtpe
      case _                        => column.head.tpe
    }

    private val isCaseHead = isCaseClass(headPatternType)
    private val dummies = if (!isCaseHead) Nil else getDummies(headPatternType.typeSymbol.caseFieldAccessors.length)

    private def subpatterns(pat:Tree): List[Tree] = {
      pat match {
        case Bind(_,p)                                                          =>
          subpatterns(p)
        case app @ Apply(fn, pats) if isCaseClass(app.tpe) && fn.isType =>
          if (isCaseHead) pats else dummies
        case Apply(fn,xs) => assert((xs.isEmpty) && (!fn.isType), "strange Apply"); dummies // named constant
        case _: UnApply                                                         =>
          dummies
        case pat                                                                =>
          dummies
      }
    }

    /** an approximation of _tp1 <:< tp2 that ignores _ types. this code is wrong,
     *  ideally there is a better way to do it, and ideally defined in Types.scala
     */
    def subsumes_erased(_tp1:Type, tp2:Type) = {
      val tp1 = patternType_wrtEquals(_tp1)
      tp1.isInstanceOf[TypeRef] && tp2.isInstanceOf[TypeRef] &&
      ((tp1.prefix =:= tp2.prefix) &&
       ((tp1.typeSymbol eq tp2.typeSymbol) &&
        (tp1.typeSymbol ne definitions.ArrayClass)) ||
       tp1.parents.exists(_.typeSymbol eq tp2.typeSymbol))
      // rather: tp1.baseTypes.exists...?
    }

    /** returns true if pattern tests an object */
    final def objectPattern(pat:Tree): Boolean = {
      (pat.symbol ne null) &&
      (pat.symbol != NoSymbol) &&
      pat.symbol.tpe.prefix.isStable &&
      headPatternType =:= singleType(pat.symbol.tpe.prefix, pat.symbol)
    }

    /*init block*/ {
      var sr = (moreSpecific,subsumed,remaining)
      var j = 0; var pats = column; while(pats ne Nil) {
        val (ms,ss,rs) = sr // more specific, more general(subsuming current), remaining patterns
        val pat = pats.head
        val strippedPattern = strip2(pat)
        val patternType = strippedPattern.tpe
        sr = strippedPattern match {
          case Literal(Constant(null)) if !(headPatternType =:= patternType) => // special case for constant null pattern
            (ms,ss,(j,pat)::rs);
          case _ if objectPattern(pat) =>
            (EmptyTree::ms, (j,dummies)::ss, rs);                                 // matching an object

          case Typed(p, _) if (strip2(p).isInstanceOf[UnApply] && (patternType /*is never <equals>*/ <:< headPatternType)) =>
            (p::ms, (j, dummies)::ss, rs);

          case q @ Typed(pp,_) if (patternType_wrtEquals(patternType) <:< headPatternType) =>
            ({if (pat.tpe =:= headPatternType /*never true for <equals>*/) pp else q}::ms, (j, dummies)::ss, rs);

          case z:UnApply =>
            (ms,ss,(j,pat)::rs)

          case qq if subsumes_erased(patternType, headPatternType) || (patternType_wrtEquals(patternType) <:< headPatternType) && !isDefaultPattern(pat) =>
            ({if (pat.tpe =:= headPatternType /*never true for <equals>*/) EmptyTree else pat}::ms, (j,subpatterns(pat))::ss, rs);

          case _ if subsumes_erased(headPatternType, patternType) || (headPatternType <:< patternType /*never true for <equals>*/) || isDefaultPattern(pat) =>
            (EmptyTree::ms, (j, dummies)::ss, (j,pat)::rs)  // subsuming (matched *and* remaining pattern)

          case _ =>
            (ms,ss,(j,pat)::rs)
        }
        j += 1
        pats = pats.tail
      }
      this.moreSpecific = sr._1.reverse
      this.subsumed     = sr._2.reverse
      this.remaining    = sr._3.reverse
      sr = null
    } /* init block */

    override def toString = {
      "MixTypes("+scrutinee+":"+scrutinee.tpe+") {\n  moreSpecific:"+moreSpecific+"\n  subsumed:"+subsumed+"\n  remaining"+remaining+"\n}"
    }

    /** returns casted symbol, success matrix and optionally fail matrix for type test on the top of this column */
    final def getTransition(implicit theOwner: Symbol): (Symbol, Rep, Option[Rep]) = {
      casted = if (scrutinee.tpe =:= headPatternType) scrutinee else newVar(scrutinee.pos, headPatternType)
      if (scrutinee.hasFlag(Flags.TRANS_FLAG))
        casted.setFlag(Flags.TRANS_FLAG)
      // succeeding => transition to translate(subsumed) (taking into account more specific)
      val nmatrix = {
        var ntemps  = if (!isCaseHead) Nil else casted.caseFieldAccessors map {
          meth =>
            val ctemp = newVar(scrutinee.pos, casted.tpe.memberType(meth).resultType)
            if (scrutinee.hasFlag(Flags.TRANS_FLAG))
              ctemp.setFlag(Flags.TRANS_FLAG)
            ctemp
        } // (***) flag needed later
        var subtests = subsumed
        if (moreSpecific.exists { x => x != EmptyTree }) {
          ntemps   = casted::ntemps
          subtests = moreSpecific.zip(subsumed) map {
            case (mspat, (j,pats)) => (j,mspat::pats)
          }
        }
        ntemps = ntemps ::: rest.temp
        val ntriples = subtests map {
          case (j,pats) =>
            val (vs,thePat) = strip(column(j))
            val Row(opats, osubst, og, bx) = rest.row(j)
            val nsubst = osubst.add(vs.elements, casted)
            Row(pats ::: opats, nsubst, og, bx)
        }
        rep.make(ntemps, ntriples)
      }
      // fails      => transition to translate(remaining)
      val nmatrixFail: Option[Rep] = {
        val ntemps   = scrutinee :: rest.temp
        val ntriples = remaining map {
          case (j, pat) => val r = rest.row(j);  Row(pat :: r.pat, r.subst, r.guard, r.bx)
        }
        if (ntriples.isEmpty) None else Some(rep.make(ntemps, ntriples))
      }
      (casted, nmatrix, nmatrixFail)
    }

    final def tree(implicit theOwner: Symbol, failTree: Tree): Tree = {
      val (casted,srep,frep) = this.getTransition
      val condUntyped = condition(casted.tpe, this.scrutinee)
      var cond = rep.handleOuter(typed { condUntyped })
      if (needsOuterTest(casted.tpe, this.scrutinee.tpe, theOwner)) {  // @todo merge into def condition
        cond = addOuterCondition(cond, casted.tpe, mkIdent(this.scrutinee), rep.handleOuter)
      }
      val succ = repToTree(srep)

      val fail = if (frep.isEmpty) failTree else repToTree(frep.get)

      // dig out case field accessors that were buried in (***)
      val cfa  = if (!isCaseHead) Nil else casted.caseFieldAccessors
      val caseTemps = (if (!srep.temp.isEmpty && srep.temp.head == casted) srep.temp.tail else srep.temp).zip(cfa)

      var vdefs = caseTemps map {
        p =>
          val tmp = p._1;
          val accessorMethod = p._2
          val untypedAccess = Apply(Select(mkIdent(casted), accessorMethod),List())
          val typedAccess = typed { untypedAccess }
          typedValDef(tmp, typedAccess)
      }

      if (casted ne this.scrutinee)
        vdefs = ValDef(casted, gen.mkAsInstanceOf(mkIdent(this.scrutinee), casted.tpe)) :: vdefs

      return typed { If(cond, squeezedBlock(vdefs, succ), fail) }
    }
  }

  /** converts given rep to a tree - performs recursive call to translation in the process to get sub reps
   */
  final def repToTree(r: Rep)(implicit theOwner: Symbol, failTree: Tree, rep: RepFactory): Tree = {
    r.applyRule.tree
  }

  case class Row(pat:List[Tree], subst:Binding, guard:Tree, bx:Int)

  object Rep {
    type RepType = Product2[List[Symbol], List[Row]]
    final def unapply(x:Rep)(implicit rep:RepFactory):Option[RepType] =
      if (x.isInstanceOf[rep.RepImpl]) Some(x.asInstanceOf[RepType]) else None
  }
  class RepFactory(val handleOuter: Tree => Tree) {
  case class RepImpl(val temp:List[Symbol], val row:List[Row]) extends Rep with Rep.RepType {
    (row.find { case Row(pats, _, _, _) => temp.length != pats.length }) match {
      case Some(row) => assert(false, "temp == "+temp+" row.pats == "+row.pat);
      case _ =>
    }
    def _1 = temp
    def _2 = row
  }

  var vss: List[SymList] = _
  var labels:  Array[Symbol] = new Array[Symbol](4)
  var targets: List[Tree] = _
  var reached : BitSet = _;
  var shortCuts: List[Symbol] = Nil;

  final def make(temp:List[Symbol], row:List[Row], targets: List[Tree], vss:List[SymList])(implicit theOwner: Symbol): Rep = {
    // ensured that labels(i) eq null for all i, cleanup() has to be called after translation
    this.targets   = targets
    if (targets.length > labels.length)
      this.labels    = new Array[Symbol](targets.length)
    this.vss       = vss
    this.reached = new BitSet(targets.length);
    return make(temp, row)
  }

  final def shortCut(theLabel:Symbol): Int = {
    this.shortCuts = shortCuts:::theLabel::Nil;
    return -shortCuts.length
  }

  final def cleanup(tree: Tree)(implicit theOwner: Symbol): Tree = {
    object lxtt extends Transformer {
      override def transform(tree:Tree): Tree = tree match {
        case blck @ Block(vdefs, ld @ LabelDef(name,params,body)) =>
          val bx = labelIndex(ld.symbol)
          if ((bx >= 0) && !isReachedTwice(bx)) {
            squeezedBlock(vdefs,body)
          }
          else
            blck

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
    var i = targets.length;
    while (i>0) { i-=1; labels(i) = null; };
    reached = null;
    shortCuts = Nil
  }
  final def isReached(bx:Int)   = { labels(bx) ne null }
  final def markReachedTwice(bx:Int) { reached += bx }
  /** @pre bx < 0 || labelIndex(bx) != -1 */
  final def isReachedTwice(bx:Int) = (bx < 0) || reached(bx)
  /* @returns bx such that labels(bx) eq label, -1 if no such bx exists */
  final def labelIndex(label:Symbol): Int = {
    var bx = 0; while((bx < labels.length) && (labels(bx) ne label)) { bx += 1 }
    if (bx >= targets.length) bx = -1
    return bx
  }
  /** first time bx is requested, a LabelDef is returned. next time, a jump.
   *  the function takes care of binding
   */
  final def requestBody(bx:Int, subst:Binding)(implicit theOwner: Symbol): Tree = {
    if (bx < 0) { // is shortcut
      val jlabel = shortCuts(-bx-1)
      val jump = Apply(mkIdent(jlabel), Nil)
      return jump
    }
    if (!isReached(bx)) { // first time this bx is requested
      val argts = new ListBuffer[Type] // types of
      var vrev: List[Symbol] = Nil
      var vdefs:List[Tree] = Nil
      val it = vss(bx).elements; while(it.hasNext) {
        val v = it.next
        val substv = subst(v)
        if (substv ne null) { // might be bound elsewhere ( see `x @ unapply' )
          vrev   = v :: vrev
          argts += v.tpe
          vdefs  = typedValDef(v, substv)::vdefs
        }
      }
      val body  = targets(bx)
      // @bug: typer is not able to digest a body of type Nothing being assigned result type Unit
      val tpe = if (body.tpe.typeSymbol eq definitions.NothingClass) body.tpe else resultType
      val label = theOwner.newLabel(body.pos, "body%"+bx).setInfo(new MethodType(argts.toList, tpe))
      labels(bx) = label

      return body match {
        case _: Throw | _: Literal => squeezedBlock(vdefs.reverse, body.duplicate setType tpe)
        case _ => squeezedBlock(vdefs, LabelDef(label, vrev.reverse, body setType tpe))
      }
    }

    // jump
    markReachedTwice(bx) // if some bx is not reached twice, its LabelDef
    val args = new ListBuffer[Ident] // is replaced with body itself
    var vs   = vss(bx).elements; while(vs.hasNext) {
      val v = vs.next
      val substv = subst(v)
      assert(substv ne null, "subst("+v+") is null"+cunit.toString) // if sharing takes place, then 'binding elsewhere' is not allowed
      args += substv
    }
    val label = labels(bx)
    label.tpe match {
      case MethodType(fmls,_) =>
        if (fmls.length != args.length) { // sanity check
          cunit.error(targets(bx).pos, "consistency problem in target generation ! I have args "+args+" and need to jump to a label with fmls "+fmls)
          throw FatalError("consistency problem")
        }
        for((f,a) <- fmls.zip(args.toList)) {
          if (!(a.tpe <:< f)) {
            cunit.error(targets(bx).pos, "consistency problem ! "+a.tpe+" "+f)
            throw FatalError("consistency problem")
          }
        }
    }

    val body = targets(bx)
    if (body.isInstanceOf[Throw] || body.isInstanceOf[Literal]) {
      val vdefs = new ListBuffer[Tree]
      val it = vss(bx).elements; while(it.hasNext) {
        val v = it.next
        val substv = subst(v)
        if (substv ne null) { // might be bound elsewhere ( see `x @ unapply' )
          vdefs  += typedValDef(v, substv)
        }
      }
      squeezedBlock(vdefs.toList, body.duplicate setType resultType)
    } else {
      Apply(mkIdent(label),args.toList)
    }
  }

  /** the injection here handles alternatives and unapply type tests */
  final def make(temp:List[Symbol], row1:List[Row])(implicit theOwner: Symbol): Rep = {
    var unchanged: Boolean = true
    val row = row1 flatMap {
      xx =>
        def isAlternative(p: Tree): Boolean = p match {
          case Bind(_,p)       => isAlternative(p)
          case Alternative(ps) => true
          case _               => false
        }
        def getAlternativeBranches(p:Tree): List[Tree] = {
          def get_BIND(pctx:Tree => Tree, p:Tree):List[Tree] = p match {
            case b @ Bind(n,p)   => get_BIND({ x:Tree => pctx(copy.Bind(b, n, x) setType x.tpe) }, p)
            case Alternative(ps) => ps map pctx
          }
          get_BIND({x=>x}, p)
        }
        val Row(opatso, subst, g, bx) = xx
        var opats = opatso
        var pats:List[Tree] = Nil
        var indexOfAlternative = -1
        var j = 0; while(opats ne Nil) {
          var opat = opats.head // original pattern
          val (vars, strippedPat) = strip(opat)
          val vs = vars.toList
          (strippedPat: @unchecked) match {

            case p @ Alternative(ps) =>
              DBG("Alternative")
              if (indexOfAlternative == -1) {
                unchanged = false
                indexOfAlternative = j
              }
              pats = opat :: pats

            case typat @ Typed(p,tpt) if strip2(p).isInstanceOf[UnApply]=>
              DBG("Typed")
              pats = (if (temp(j).tpe <:< tpt.tpe) makeBind(vs, p) else opat)::pats

            case Ident(nme.WILDCARD) | EmptyTree | _:Literal | _:Typed =>
              DBG("Ident(_)|EmptyTree")
              pats = opat :: pats

            case o @ Ident(n) => // n != nme.WILDCARD
              DBG("Ident")
              val tpe =
                if (!o.symbol.isValue) {
                  singleType(o.tpe.prefix, o.symbol)
                } else {
                  singleType(NoPrefix, o.symbol) // equals-check
                  // call the above `stpe'. Then we could also return
                  // `typeRef(definitions.ScalaPackageClass.tpe, definitions.EqualsPatternClass, List(stpe))'
                  // and force an equality check. However, exhaustivity checking would not work anymore.
                  // so first, extend exhaustivity check to equalspattern
                }
              val p = Ident(nme.WILDCARD) setType tpe
              val q = Typed(p, TypeTree(tpe)) setType tpe
              pats = (makeBind( vs, q) setType tpe) :: pats


            case o @ Select(stor,_) =>
              DBG("Select")
              val stpe =
                if (!o.symbol.isValue) {
                  singleType(o.tpe.prefix, o.symbol)
                } else {
                  singleType(NoPrefix, o.symbol) // equals-check
                }
              val p = Ident(nme.WILDCARD) setType stpe
              val q = makeBind(vs,Typed(p, TypeTree(stpe)) setType stpe) setType stpe
              pats = q::pats

            case UnApply(Apply(TypeApply(sel @ Select(stor, nme.unapplySeq),List(tptArg)),_),ArrayValue(_,xs)::Nil) if (stor.symbol eq definitions.ListModule) =>
              DBG("Unapply(...TypeApply...)")
              //@pre: is not right-ignoring (no star pattern)
              // no exhaustivity check, please
              temp(j).setFlag(Flags.TRANS_FLAG)
              val listType = typeRef(mkThisType(definitions.ScalaPackage), definitions.ListClass, List(tptArg.tpe))
              val nmlzdPat = normalizedListPattern(xs, tptArg.tpe)
              pats = makeBind(vs, nmlzdPat) :: pats

            //@todo: rewrite, using __UnApply instead of UnApply like so:
            //case  ua @ __UnApply(_,argtpe,_) =>
              //val ua = prepat
            //  val npat = (if (temp(j).tpe <:< argtpe) ua else Typed(ua,TypeTree(argtpe)).setType(argtpe))
            //  pats = (makeBind(vs, npat) setType argtpe)::pats


            case ua @ UnApply(Apply(fn, _), _) =>
              DBG("Unapply(Apply())")
              fn.tpe match {
                case MethodType(List(argtpe,_*),_) =>
                  val npat = (if (temp(j).tpe <:< argtpe) ua else Typed(ua,TypeTree(argtpe)).setType(argtpe))
                  pats = (makeBind(vs, npat) setType argtpe)::pats
              }

            case o @ Apply(fn, List()) if !isCaseClass(o.tpe) || /*see t301*/ !Apply_Value.unapply(o).isEmpty =>
              DBG("Apply !isCaseClass")
              val stpe: Type = fn match {
                case _ if (o.symbol.isModule) =>
                  singleType(o.tpe.prefix, o.symbol)
                case _ if (o.tpe.termSymbol.isModule) =>
                  singleType(o.tpe.prefix, o.symbol)
                case Select(path,sym) =>
                  path.tpe match {
                    case ThisType(sym) =>
                      singleType(path.tpe, o.symbol)

                    case _ => // e.g. `case Some(p._2)' in scala.collection.jcl.Map
                      if (path.isInstanceOf[Apply])
                        new PseudoType(o) // outer-matching, see test/files/pos/t154.scala
                      else
                        singleType(singleType(path.tpe.prefix, path.symbol), o.symbol)  // old

                  }
                case o @ Ident(_) =>
                  if (!o.symbol.isValue)
                    singleType(o.tpe.prefix, o.symbol)
                  else
                    singleType(NoPrefix, o.symbol)
              }
              val ttst = typeRef(NoPrefix, definitions.EqualsPatternClass, List(stpe))
              val p = Ident(nme.WILDCARD) setType ttst
              val q = makeBind(vs,Typed(p, TypeTree(stpe)) setType ttst)
              pats = q::pats

            case Apply_Value(pre, sym) =>
              DBG("Apply_Value")
              val tpe = typeRef(NoPrefix, definitions.EqualsPatternClass, singleType(pre, sym)::Nil)
              val q = makeBind(vs,Typed(EmptyTree, TypeTree(tpe)) setType tpe)
              pats = q :: pats

            case Apply_CaseClass_NoArgs(tpe) =>  // no-args case class pattern
              DBG("Apply_CaseClass_NoArgs")
              val q = makeBind(vs, Typed(EmptyTree, TypeTree(tpe)) setType tpe)
              pats = q :: pats

            case Apply_CaseClass_WithArgs() =>  // case class pattern with args
              DBG("Apply_CaseClass_WithArgs")
              pats = opat :: pats

            case ArrayValue(_,xs) =>
              DBG("ArrayValue")
              pats = opat :: pats

          }
          opats = opats.tail
          j += 1
        }
        pats = pats.reverse
        if (indexOfAlternative == -1) {
          val res = List(Row(pats, subst, g, bx))
          DBG("finished: result "/*+res*/)
          res
        }
        else {
          val prefix = pats.take( indexOfAlternative )
          val alts   = getAlternativeBranches(pats( indexOfAlternative ))
          val suffix = pats.drop(indexOfAlternative + 1)
          val intermediary_result = alts map { p => Row(prefix ::: p :: suffix, subst, g, bx) }
          DBG("not finished: intermediary_result = "/*+intermediary_result*/)
          intermediary_result
        }
    }

    if (unchanged) RepImpl(temp,row).init
    else this.make(temp,row) // recursive call
  }
}

  abstract class Rep {
    val temp:List[Symbol]
    val row:List[Row]
    var sealedCols = List[Int]()
    var sealedComb = List[Set[Symbol]]()

    final def init: this.type = {
      temp.zipWithIndex.foreach {
      case (sym,i) =>
        if (sym.hasFlag(Flags.MUTABLE) &&  // indicates that have not yet checked exhaustivity
            !sym.hasFlag(Flags.TRANS_FLAG) &&  // indicates @unchecked
            sym.tpe.typeSymbol.hasFlag(Flags.SEALED)) {

              sym.resetFlag(Flags.MUTABLE)
              sealedCols = i::sealedCols
              // this should enumerate all cases... however, also the superclass is taken if it is not abstract
              def candidates(tpesym: Symbol): SymSet =
                if (!tpesym.hasFlag(Flags.SEALED)) emptySymbolSet else
                  tpesym.children.flatMap { x =>
                    val z = candidates(x)
                    if (x.hasFlag(Flags.ABSTRACT)) z else z + x
                  }
              val cases = candidates(sym.tpe.typeSymbol)
              sealedComb = cases::sealedComb
            }
      }
      //  computes cartesian product, keeps indices available
      def combine(colcom: List[(Int,Set[Symbol])]): List[List[(Int,Symbol)]] = colcom match {
        case Nil => Nil
        case (i,syms)::Nil => syms.toList.map { sym => List((i,sym)) }
        case (i,syms)::cs  => for (s <- syms.toList; rest <- combine(cs)) yield (i,s) :: rest
      }

      if (!sealedCols.isEmpty) {
        val allcomb = combine(sealedCols zip sealedComb)
        /** returns true if pattern vector pats covers a type symbols "combination"
         *  @param pats pattern vector
         *  @param comb pairs of (column index, type symbol)
         */
        def covers(pats: List[Tree], comb:List[(Int,Symbol)]) =
          comb forall {
            case (i,sym) =>
              val p = strip2(pats(i));
            val res =
              isDefaultPattern(p) || p.isInstanceOf[UnApply] || p.isInstanceOf[ArrayValue] || {
                val ptpe = patternType_wrtEquals(p.tpe)
                val symtpe = if (sym.hasFlag(Flags.MODULE) && (sym.linkedModuleOfClass ne NoSymbol)) {
                  singleType(sym.tpe.prefix, sym.linkedModuleOfClass) // e.g. None, Nil
                } else sym.tpe
                (ptpe.typeSymbol == sym) || (symtpe <:< ptpe) ||
                (symtpe.parents.exists(_.typeSymbol eq ptpe.typeSymbol)) || // e.g. Some[Int] <: Option[&b]
                /* outer, see scala.util.parsing.combinator.lexical.Scanner */
                (ptpe.prefix.memberType(sym) <:< ptpe)
              }
            res
          }

        val coversAll = allcomb forall { combination => row exists { r => (r.guard eq EmptyTree) && covers(r.pat, combination)}}
        if (!coversAll) {
          val sb = new StringBuilder()
          sb.append("match is not exhaustive!\n")
          for (open <- allcomb if !(row exists { r => covers(r.pat, open)})) {
            sb.append("missing combination ")
            val NPAD = 15
            def pad(s:String) = { 1.until(NPAD - s.length).foreach { x => sb.append(" ") }; sb.append(s) }
            List.range(0, temp.length) foreach {
              i => open.find { case (j,sym) => j==i } match {
                case None => pad("*")
                case Some((_,sym)) => pad(sym.name.toString)
              }
            }
            sb.append('\n')
          }
          cunit.warning(temp.head.pos, sb.toString)
        }
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
      case Row(pats, subst, g, bx)::xs =>
        var px = 0; var rpats = pats; var bnd = subst; var temps = temp; while((bnd ne null) && (rpats ne Nil)) {
          val (vs,p) = strip(rpats.head);
          if (!isDefaultPattern(p)) { /*break*/ bnd = null; } else {
            bnd = bnd.add(vs.elements,temps.head)
            rpats = rpats.tail
            temps = temps.tail
            px += 1 // pattern index
          }
        }
        /*    Row(   _   ...   _     g_1  b_1 ) :: rows     it's all default patterns
         */
        if (bnd ne null) {    // all default patterns
          val rest = if (g eq EmptyTree) null else rep.make(temp, xs)
          DBG("\n---\nmixture rule is = VariableRule")
          return VariableRule (bnd, g, rest, bx)
        }

      /*    Row( _  ... _ p_1i  ...  p_1n   g_m  b_m ) :: rows
       */
        // cut out column px that contains the non-default pattern
        val column   = rpats.head :: (row.tail map { case Row(pats,_,_,_) => pats(px) })
        val restTemp =                                               temp.take(px) ::: temp.drop(px+1)
        val restRows = row map { case Row(pats, subst, g, bx) => Row(pats.take(px) ::: pats.drop(px+1), subst, g, bx) }
        val mr = MixtureRule(temps.head, column, rep.make(restTemp,restRows))
        DBG("\n---\nmixture rule is = "/*+mr.getClass.toString*/)
        mr
    }

    // a fancy toString method for debugging
    override final def toString = {
      val sb   = new StringBuilder
      val NPAD = 15
      def pad(s:String) = { 1.until(NPAD - s.length).foreach { x => sb.append(" ") }; sb.append(s) }
      for (tmp <- temp) pad(tmp.name.toString)
      sb.append('\n')
      for ((r,i) <- row.zipWithIndex) {
        for (c <- r.pat ::: List(r.subst, r.guard, r.bx)) {
          pad(c.toString)
        }
        sb.append('\n')
      }
      sb.toString
    } /* def toString */
  } /* class Rep */

  /** creates initial clause matrix
   */
  final def initRep(roots: List[Symbol], cases: List[Tree], rep:RepFactory)(implicit theOwner: Symbol) = {
    // communicate whether exhaustiveness-checking is enabled via some flag
    var bx = 0;
    val targets = new ListBuffer[Tree]
    val vss = new ListBuffer[SymList]
    val row = new ListBuffer[Row]

    var cs = cases; while (cs ne Nil) cs.head match {  // stash away pvars and bodies for later
      case CaseDef(pat,g,b) =>
        vss     += definedVars(pat)
        targets += b
        if (roots.length > 1) pat match {
          case Apply(fn, pargs)    =>
            row += Row(pargs, NoBinding, g, bx)
          case Ident(nme.WILDCARD) =>
            row += Row(getDummies(roots.length), NoBinding, g, bx)
        } else
          row     += Row(List(pat), NoBinding, g, bx)
        bx      += 1
        cs = cs.tail
    }
    rep.make(roots, row.toList, targets.toList, vss.toList)
  }

  final def newVar(pos: Position, name: Name, tpe: Type)(implicit theOwner: Symbol): Symbol = {
    if (tpe eq null) assert(tpe ne null, "newVar("+name+", null)")
    val sym = theOwner.newVariable(pos, name) // careful: pos has special meaning
    sym setInfo tpe
    sym
  }

  final def newVar(pos: Position, tpe: Type)(implicit theOwner: Symbol): Symbol =
    newVar(pos, cunit.fresh.newName(pos, "temp"), tpe) setFlag Flags.SYNTHETIC

  /** returns the condition in "if (cond) k1 else k2"
   */
  final def condition(tpe: Type, scrut: Symbol): Tree = {
    assert(scrut ne NoSymbol)
    condition(tpe, mkIdent(scrut))
  }

  final def condition(tpe: Type, scrutineeTree: Tree): Tree = {
    assert(tpe ne NoType)
    assert(scrutineeTree.tpe ne NoType)
    if (tpe.isInstanceOf[SingletonType] && !tpe.isInstanceOf[ConstantType]) {
      if (tpe.termSymbol.isModule) {// object
        //if (scrutineeTree.tpe <:< definitions.AnyRefClass.tpe)
        //  Eq(gen.mkAttributedRef(tpe.termSymbol), scrutineeTree)             // object
        //else
          Equals(gen.mkAttributedRef(tpe.termSymbol), scrutineeTree)         // object
      } else {
        val x =
          if (tpe.prefix ne NoPrefix) gen.mkIsInstanceOf(scrutineeTree, tpe)
          else
          Equals(gen.mkAttributedRef(tpe.termSymbol), scrutineeTree)
        typed { x }
      }
    } else if (tpe.isInstanceOf[ConstantType]) {
      val value = tpe.asInstanceOf[ConstantType].value
      if (value == Constant(null) && scrutineeTree.tpe <:< definitions.AnyRefClass.tpe)
        Eq(scrutineeTree, Literal(value))             // constant
      else
        Equals(scrutineeTree, Literal(value))             // constant
    } else if (scrutineeTree.tpe <:< tpe && tpe <:< definitions.AnyRefClass.tpe) {
      NotNull(scrutineeTree)
    } else {
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
      if (settings_debug) cunit.warning(scrutinee.pos, "no outer acc for "+tpe2test.typeSymbol)
      cond
    } else
      And(cond,
          Eq(Apply(Select(
            gen.mkAsInstanceOf(scrutinee, tpe2test, true), outerAcc),List()), theRef))
  }

}
