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
 * Implementation based on the algorithm described in
 *
 *   "A Term Pattern-Match Compiler Inspired by Finite Automata Theory"
 *   Mikael Pettersson
 *   ftp://ftp.ida.liu.se/pub/labs/pelab/papers/cc92pmc.ps.gz
 *
 *  @author Burak Emir
 */
trait ParallelMatching extends ast.TreeDSL {
  self: transform.ExplicitOuter with PatternNodes =>

  // debugging var, set to true to see how sausages are made
  private var trace = false

  import global.{ typer => _, _ }
  import definitions.{ AnyRefClass, EqualsPatternClass, IntClass, getProductArgs, productProj }
  import analyzer.Typer;
  import symtab.Flags
  import Types._
  import CODE._

  object Implicits {
    implicit def mkPattern(t: Tree) = Pattern(t)
  }
  import Implicits._

  def ifDebug(body: => Unit): Unit          = { if (settings.debug.value) body }
  def DBG(msg: => String): Unit             = { ifDebug(println(msg)) }

  def TRACE(f: String, xs: Any*): Unit      = { if (trace) println(if (xs.isEmpty) f else f.format(xs : _*)) }
  def logAndReturn[T](s: String, x: T): T   = { log(s + x.toString) ; x }
  def traceAndReturn[T](s: String, x: T): T = { TRACE(s + x.toString) ; x }

  // Tests on misc
  def isSwitchableTag(tag: Int)   = cond(tag)       { case ByteTag | ShortTag | IntTag | CharTag => true }
  def isSwitchableConst(t: Tree)  = cond(t)         { case Literal(x: Constant) => isSwitchableTag(x.tag) }

  // Tests on Trees
  def isStar(t: Tree)             = cond(unbind(t)) { case Star(q) => isDefaultPattern(q) }
  def isAlternative(t: Tree)      = cond(unbind(t)) { case Alternative(_) => true }
  def isRightIgnoring(t: Tree)    = cond(unbind(t)) { case ArrayValue(_, xs) if !xs.isEmpty => isStar(xs.last) }
  def isDefaultPattern(t: Tree)   = cond(unbind(t)) { case EmptyTree | WILD() => true }
  def isLabellable(t: Tree)       = !cond(t)        { case _: Throw | _: Literal => true }
  def isModule(t: Tree)           = t.symbol.isModule || t.tpe.termSymbol.isModule

  // For isDefaultPattern, to do?
  // cond(tree) { case Typed(WILD(), _) if tree.tpe <:< scrut.tpe  => true }
  // null check?

  // Tests on Types
  def isEquals(t: Type)           = cond(t)         { case TypeRef(_, EqualsPatternClass, _) => true }
  def isAnyRef(t: Type)           = t <:< AnyRefClass.tpe
  def isCaseClass(t: Type)        = t.typeSymbol hasFlag Flags.CASE

  // this won't compile in compiler, but works in REPL - ?
  // val List(isInt, isChar, isBoolean, isArray, isNothing) = {
  //   import definitions._
  //   def testFor(s: Symbol): Type => Boolean = (tpe: Type) => tpe.typeSymbol eq s
  //
  //   List(IntClass, CharClass, BooleanClass, ArrayClass, NothingClass) map testFor
  // }

  case class Pattern(tree: Tree) {
    import definitions._
    lazy val    sym = tree.symbol
    lazy val    tpe = tree.tpe
    lazy val prefix = tpe.prefix
    lazy val tpeIfHead  = unbind(tree) match {
      case p @ (_:Ident | _:Select) => singleType(stripped.prefix, stripped.sym) //should be singleton object
      case __UnApply(_,argtpe,_)    => argtpe
      case _                        => tpe
    }

    /**
     * Can this pattern be part of a switch statement?
     */
    lazy val isSimpleSwitchCandidate = isSwitchableConst(unbind(tree))

    final def isDefault       = isDefaultPattern(tree)

    /* a Seq ending in _* */

    final def getAlternativeBranches: List[Tree] = {
      def get_BIND(pctx: TreeFunction1, p: Tree): List[Tree] = p match {
        case b @ Bind(n, p)   => get_BIND((x: Tree) => pctx(treeCopy.Bind(b, n, x) setType x.tpe), p)
        case Alternative(ps)  => ps map pctx
      }
      get_BIND(x => x, tree)
    }

    // XXX move right place
    def allBindings: List[Bind] = allBindingsInt(tree)
    private def allBindingsInt(t: Tree): List[Bind] = t match {
      case b @ Bind(_, t) => b :: allBindingsInt(t)
      case _              => Nil
    }

    /** All variables binding the given pattern. */
    def boundVariables: List[Symbol] = allBindings.foldRight(List[Symbol]())(_.symbol :: _)

    /** The pattern with its variable bindings stripped. */
    def stripped: Tree = if (allBindings.isEmpty) tree else allBindings.last.body

    final def definedVars: List[Symbol] = ParallelMatching.this.definedVars(tree)

    /** returns true if pattern tests an object */
    final def isObjectTest(head: Type) =
      (sym ne null) && (sym != NoSymbol) && prefix.isStable && (head =:= singleType(prefix, sym))
  }

  import collection.mutable.{ HashMap, ListBuffer }
  class MatchMatrix(context: MatchMatrixContext, data: MatchMatrixInit) {
    import context._
    val MatchMatrixInit(roots, cases, failTree) = data

    val labels    = new HashMap[Int, Symbol]()
    val shortCuts = new ListBuffer[Symbol]()
    lazy val reached   = new BitSet(targets.size)

    private lazy val expandResult     = expand(roots, cases)
    lazy val targets: List[Tree]      = expandResult._2
    lazy val vss: List[List[Symbol]]  = expandResult._3
    lazy val expansion: Rep           = make(roots, expandResult._1)

    final def shortCut(theLabel: Symbol): Int = {
      shortCuts += theLabel
      -shortCuts.length
    }

    final def cleanup(tree: Tree): Tree = {
      // Extractors which can spot pure true/false expressions
      // even through the haze of braces
      abstract class SeeThroughBlocks[T] {
        protected def unapplyImpl(x: Tree): T
        def unapply(x: Tree): T = x match {
          case Block(Nil, expr)         => unapply(expr)
          case _                        => unapplyImpl(x)
        }
      }
      object IsTrue extends SeeThroughBlocks[Boolean] {
        protected def unapplyImpl(x: Tree): Boolean = x equalsStructure TRUE
      }
      object IsFalse extends SeeThroughBlocks[Boolean] {
        protected def unapplyImpl(x: Tree): Boolean = x equalsStructure FALSE
      }
      object lxtt extends Transformer {
        override def transform(tree: Tree): Tree = tree match {
          case blck @ Block(vdefs, ld @ LabelDef(name, params, body)) =>
            val bx = labelIndex(ld.symbol)
            if (bx >= 0 && !isReachedTwice(bx)) squeezedBlock(vdefs, body)
            else blck

          case t =>
            super.transform(t match {
              // note - it is too early for any other true/false related optimizations
              case If(cond, IsTrue(), IsFalse())  => cond

              case If(cond1, If(cond2, thenp, elsep1), elsep2) if (elsep1 equalsStructure elsep2) =>
                IF (cond1 AND cond2) THEN thenp ELSE elsep1
              case If(cond1, If(cond2, thenp, Apply(jmp, Nil)), ld: LabelDef) if jmp.symbol eq ld.symbol =>
                IF (cond1 AND cond2) THEN thenp ELSE ld
              case t => t
          })
        }
      }
      object resetTraverser extends Traverser {
        import Flags._
        def reset(vd: ValDef) =
          if (vd.symbol hasFlag SYNTHETIC) vd.symbol resetFlag (TRANS_FLAG|MUTABLE)

        override def traverse(x: Tree): Unit = x match {
          case vd: ValDef => reset(vd)
          case _          => super.traverse(x)
        }
      }

      applyAndReturn[Tree](resetTraverser traverse _)(lxtt transform tree)
    }

    final def isReached(bx: Int)        = labels contains bx
    final def markReachedTwice(bx: Int) { reached += bx }

    /** @pre bx < 0 || labelIndex(bx) != -1 */
    final def isReachedTwice(bx: Int) = (bx < 0) || reached(bx)

    /* @returns bx such that labels(bx) eq label, -1 if no such bx exists */
    final def labelIndex(label: Symbol) = labels find (_._2 eq label) map (_._1) getOrElse (-1)

    /** first time bx is requested, a LabelDef is returned. next time, a jump.
     *  the function takes care of binding
     */
    final def requestBody(bx: Int, subst: Bindings): Tree = {
      if (bx < 0) { // is shortcut
        val jlabel = shortCuts(-bx-1)
        return Apply(ID(jlabel), Nil)
      }
      if (!isReached(bx)) { // first time this bx is requested
        // might be bound elsewhere
        val (vsyms, vdefs) : (List[Symbol], List[Tree]) = List.unzip(
          for (v <- vss(bx) ; substv <- subst(v)) yield
            (v, typedValDef(v, substv))
        )

        val body    = targets(bx)
        // @bug: typer is not able to digest a body of type Nothing being assigned result type Unit
        val tpe     = if (body.tpe.isNothing) body.tpe else resultType
        val newType = MethodType(vsyms, tpe)
        val label   = owner.newLabel(body.pos, "body%"+bx) setInfo newType
        labels(bx)  = label

        return logAndReturn("requestBody(%d) first time: ".format(bx), squeezedBlock(vdefs, (
          if (isLabellable(body)) LabelDef(label, vsyms, body setType tpe)
          else body.duplicate setType tpe
        )))
      }

      // if some bx is not reached twice, its LabelDef is replaced with body itself
      markReachedTwice(bx)

      val args  = vss(bx) map subst flatten
      val label = labels(bx)
      val body  = targets(bx)
      val fmls  = label.tpe.paramTypes

      def debugConsistencyFailure(): String = {
        val xs =
          ( for ((vs, i) <- vss.zipWithIndex) yield "vss(%d) = %s\nargs = %s".format(i, vs mkString ", ", args) ) ++
          ( for ((t, i) <- targets.zipWithIndex) yield "targets(%d) = %s".format(i, t) ) ++
          ( for ((i, l) <- labels) yield "labels(%d) = %s".format(i, l) ) ++
          ( for ((s, v) <- List("bx" -> bx, "label.tpe" -> label.tpe)) yield "%s = %s".format(s, v) )

        xs mkString "\n"
      }
      // sanity checks: same length lists and args are conformant with formals
      def isConsistent() = (fmls.length == args.length) && List.forall2(args, fmls)(_.tpe <:< _)

      if (!isConsistent()) {
        val msg = (
          """Consistency problem compiling %s!
            |Trying to call %s(%s) with arguments (%s)""" .
            stripMargin.format(cunit.source, label, fmls, args)
        )
        TRACE(debugConsistencyFailure())
        cunit.error(body.pos, msg)
      }

      def vds = for (v <- vss(bx) ; substv <- subst(v)) yield typedValDef(v, substv)

      if (isLabellable(body)) ID(label) APPLY (args)
      else                    squeezedBlock(vds, body.duplicate setType resultType)
    }

    /** the injection here handles alternatives and unapply type tests */
    final def make(temp: List[Symbol], row1: List[Row]): Rep = {
      // Martin: I am not really sure what stype is doing, but it caused aliases.scala to fail
      // The problem is if a val has a singleType of some other module. Then isModule is true and
      // sType is called. But it ends up mixing the prefix of the other module with the val symbol
      // which then causes erasure to be confused.
      def mkSingletonType(o: Tree) = o.tpe match {
        case st: SingleType => st
        case _              => singleType(o.tpe.prefix, o.symbol)
      }
      def equalsCheck(o: Tree) = if (o.symbol.isValue) singleType(NoPrefix, o.symbol) else mkSingletonType(o)

      def doSelect(o: Tree, path: Tree) = (path, path.tpe) match {
        case (_, t: ThisType)     => singleType(t, o.symbol)            // cases 2/3 are e.g. `case Some(p._2)' in s.c.jcl.Map
        case (_: Apply, _)        => PseudoType(o)                      // outer-matching: test/files/pos/t154.scala
        case _                    => singleType(mkSingletonType(path), o.symbol)  // old
      }
      def applyType(o: Tree, fn: Tree): Type = fn match {
        case _ if isModule(o)   => mkSingletonType(o)
        case Select(path, _)    => doSelect(o, path) // XXX ?
        case o: Ident           => equalsCheck(o)
      }

      def classifyPat(opat: Tree, j: Int): Tree = {
        def vars                    = opat.boundVariables
        def rebind(t: Tree)         = makeBind(vars, t)
        def rebindEmpty(tpe: Type)  = mkEmptyTreeBind(vars, tpe)
        def rebindTyped()           = mkTypedBind(vars, equalsCheck(unbind(opat)))

        // @pre for UnApply_TypeApply: is not right-ignoring (no star pattern) ; no exhaustivity check
        def doUnapplyTypeApply(tptArg: Tree, xs: List[Tree]) = {
          temp(j) setFlag Flags.TRANS_FLAG
          rebind(normalizedListPattern(xs, tptArg.tpe))
        }

        def doUnapplyApply(ua: UnApply, fn: Tree) = {
          val MethodType(List(arg, _*), _) = fn.tpe
          val npat =
            if (temp(j).tpe <:< arg.tpe) ua
            else Typed(ua, TypeTree(arg.tpe)) setType arg.tpe

          // TRACE("doUnapplyApply: %s <:< %s == %s", temp(j).tpe, argtpe, (temp(j).tpe <:< argtpe))
          logAndReturn("doUnapplyApply: ", rebind(npat) setType arg.tpe)
        }
        def doApplyFunction(o: Tree, fn: Tree) = {
          val stpe = applyType(o, fn)
          val ttst = mkEqualsRef(List(stpe))

          // TRACE("doApplyFunction: stpe = %s, ttst = %s", stpe, ttst)
          logAndReturn("doApplyFunction: ", rebind(Typed(WILD(ttst), TypeTree(stpe)) setType ttst))
        }

        def doReturnOriginal(t: Tree) = cond(t) {
          case EmptyTree | WILD() | _: Literal | _: Typed | _: ArrayValue   => true
        }
        def doRebindTyped(t: Tree) = cond(t) { case _: Ident | _: Select => true }

        // NOTE - this seemingly pointless representation has a point.  Until extractors
        // can be trusted, I only feel safe using them by using one to a match, because it is
        // in the transitions they are broken.  This will return to a more traditional
        // pattern match before the final curtain falls.
        val f = List[PartialFunction[Tree, Tree]](
          { case _: Alternative                       => opat } ,
          { case Typed(p @ Stripped(_: UnApply), tpt) => if (temp(j).tpe <:< tpt.tpe) rebind(p) else opat } ,
          { case x if doReturnOriginal(x)             => opat } ,
          { case x if doRebindTyped(x)                => rebindTyped() } ,  // Ident(_) != nme.WILDCARD
          { case _: This                              => opat } ,
          { case UnApply_TypeApply(tptArg, xs)        => doUnapplyTypeApply(tptArg, xs) } ,
          { case ua @ UnApply(Apply(fn, _), _)        => doUnapplyApply(ua, fn) } ,
          { case o @ Apply_Function(fn)               => doApplyFunction(o, fn) } ,
          { case Apply_Value(pre, sym)                => rebindEmpty(mkEqualsRef(List(singleType(pre, sym)))) } ,
          { case Apply_CaseClass(tpe, args)           => if (args.isEmpty) rebindEmpty(tpe) else opat } ,
          { case x                                    => abort("Unexpected pattern: " + x.getClass + " => " + x) }
        ) reduceLeft (_ orElse _)

        f(unbind(opat))
      }

      val row = row1 flatMap (_ expand classifyPat)
      if (row.length != row1.length) make(temp, row)  // recursive call if any change
      else Rep(temp, row).checkExhaustive
    }

    override def toString() = {
      val toPrint: List[(Any, Traversable[Any])] = (
        (vss.zipWithIndex map (_.swap)) :::
        List[(Any, Traversable[Any])](
          "labels" -> labels,
          "targets" -> targets,
          "reached" -> reached,
          "shortCuts" -> shortCuts.toList
        ) filterNot (_._2.isEmpty)
      )

      val strs = toPrint map { case (k, v) => "    %s = %s\n".format(k, v) }
      if (toPrint.isEmpty) "MatchMatrix()"
      else "MatchMatrix(\n%s)".format(strs mkString)
    }

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

      // presenting a face of our symbol
      def tpe       = sym.tpe
      def pos       = sym.pos
      def accessors = sym.caseFieldAccessors
      def id        = ID(sym)   // attributed ident

      // tests
      def isDefined = sym ne NoSymbol
      def isSimple  = tpe.isChar || tpe.isInt

      // sequences
      def seqType   = tpe.widen baseType SeqClass
      def elemType  = tpe typeArgs 0  // can this happen? if (seqType == NoType) error("...")

      // for propagating "unchecked" to synthetic vars
      def flags: List[Long] = List(Flags.TRANS_FLAG) filter (sym hasFlag _)

      def assertIsSubtype(other: Type) = assert(isSubType(tpe, other), "problem "+tpe+" not <: "+other)
      def casted(headType: Type) =
        if (tpe =:= headType) this
        else new Scrutinee(newVar(pos, headType, flags = flags))
    }

    case class Patterns(scrut: Scrutinee, ps: List[Pattern]) {
      private lazy val column = ps map (_.tree)
      lazy val head = ps.head
      lazy val tail = Patterns(scrut, ps.tail)
      lazy val last = ps.last
      lazy val headType = head.tpeIfHead
      lazy val isCaseHead = isCaseClass(headType)
      lazy val dummies = if (isCaseHead) getDummies(headType.typeSymbol.caseFieldAccessors.length) else Nil
      lazy val size = ps.length

      def apply(i: Int): Tree = ps(i).tree
      def zip() = column.zipWithIndex
      def zip[T](ys: List[T]) = column zip ys

      def isObjectTest(pat: Pattern)  = pat isObjectTest headType
      def isObjectTest(pat: Tree)     = Pattern(pat) isObjectTest headType
      // an unapply for which we don't need a type test
      def isUnapplyHead = cond (head.tree) { case __UnApply(_,tpe,_)  => scrut.tpe <:< tpe }

      def isSimpleSwitch: Boolean =
        scrut.isSimple && ps.init.forall(_.isSimpleSwitchCandidate) &&
        // TODO: This needs to also allow the case that the last is a compatible type pattern.
        (last.isSimpleSwitchCandidate || last.isDefault)

      def mkRule(rest: Rep): RuleApplication =
        logAndReturn("mkRule: ", head match {
            case x if isEquals(x.tpe)     => new MixEquals(this, rest)
            case Pattern(x: ArrayValue)   => if (isRightIgnoring(x)) new MixSequenceStar(this, rest)
                                             else new MixSequence(this, rest)
            case _ if isSimpleSwitch      => new MixLiterals(this, rest)
            case _ if isUnapplyHead       => new MixUnapply(this, rest)
            case _                        => new MixTypes(this, rest)
          }
        )
    }

    /**
     * Class encapsulating a guard expression in a pattern match:
     *   case ... if(tree) => ...
     */
    case class Guard(tree: Tree) {
      def isEmpty   = tree eq EmptyTree
      def duplicate = Guard(tree.duplicate)
      override def toString() = if (isEmpty) "" else " // if %s" format tree
    }
    val NoGuard = Guard(EmptyTree)

    /** "The Mixture Rule"

          {v=pat1, pats1 .. } {q1}
    match {..               } {..}
          {v=patn, patsn .. } {qn}

    The is the real work-horse of the algorithm. There is some column whose top-most pattern is a
    constructor. (Forsimplicity, itisdepicted above asthe left-most column, but anycolumn will do.)
    The goal is to build a test state with the variablevand some outgoing arcs (one for each construc-
    tor and possibly a default arc). Foreach constructorcin the selected column, its arc is deﬁned as
    follows:

    Let {i1,...,ij} be the row-indices of the patterns in the column that matchc. Since the pat-
    terns are viewed as regular expressions, this will be the indices of the patterns that either
    have the same constructor c, or are wildcards.

    Let {pat1,...,patj} be the patterns in the column corresponding to the indices computed
    above, and let nbe the arity of the constructor c, i.e. the number of sub-patterns it has. For
    eachpati, its n sub-patterns are extracted; if pat i is a wildcard, nwildcards are produced
    instead, each tagged with the right path variable. This results in a pattern matrix with n
    columns and j rows. This matrix is then appended to the result of selecting, from each col-
    umn in the rest of the original matrix, those rows whose indices are in {i1,...,ij}. Finally
    the indices are used to select the corresponding ﬁnal states that go with these rows. Note
    that the order of the indices is signiﬁcant; selected rows do not change their relative orders.
    The arc for the constructor c is now deﬁned as (c’,state), where c’ is cwith any
    immediate sub-patterns replaced by their path variables (thus c’ is a simple pattern), and
    state is the result of recursively applying match to the new matrix and the new sequence
    of ﬁnal states.

    Finally, the possibility for matching failure is considered. If the set of constructors is exhaustive,
    then no more arcs are computed. Otherwise, a default arc(_,state)is the last arc. If there are
    any wildcard patterns in the selected column, then their rows are selected from the rest of the
    matrix and the ﬁnal states, and the state is the result of applying match to the new matrix and
    states. Otherwise,the error state is used after its reference count has been incremented.
    **/

    /** picks which rewrite rule to apply
     *  @precondition: column does not contain alternatives (ensured by initRep)
     */
    def MixtureRule(scrut: Scrutinee, column: List[Tree], rest: Rep): RuleApplication =
      Patterns(scrut, column map Pattern) mkRule rest

    sealed abstract class RuleApplication {
      def pats: Patterns
      def rest: Rep
      lazy val Patterns(scrut, patterns) = pats
      lazy val head = pats.head
      private lazy val sym = scrut.sym

      def mkFail(xs: List[Row]) =
        make(sym :: rest.temp, xs)

      /** Splices scrutinee's symbol in between the given lists */
      def mkNewRep(pre: List[Symbol], post: List[Symbol], rows: List[Row]) =
        make(pre ::: sym :: post, rows)

      /** translate outcome of the rule application into code (possible involving recursive application of rewriting) */
      def tree(): Tree
    }

    case class ErrorRule() extends RuleApplication {
      def pats: Patterns = impossible
      def rest: Rep = impossible
      final def tree() = failTree
    }

    /** {case ... if guard => bx} else {guardedRest} */
    /** VariableRule: The top-most row has only variable (non-constructor) patterns. */
    case class VariableRule(subst: Bindings, guard: Guard, guardedRest: Rep, bx: Int) extends RuleApplication {
      def pats: Patterns = impossible
      def rest: Rep = guardedRest

      final def tree(): Tree = {
        def body      = requestBody(bx, subst)
        def guardTest = IF (guard.duplicate.tree) THEN body ELSE guardedRest.toTree

        typer typed(
          if (guard.isEmpty) body
          else squeezedBlock(subst.targetParams(typer), guardTest)
        )
      }
    }

    /** mixture rule for literals
     */
    class MixLiterals(val pats: Patterns, val rest: Rep) extends RuleApplication {
      // e.g. (1,1) (1,3) (42,2) for column { case ..1.. => ;; case ..42..=> ;; case ..1.. => }
      var defaultV: Set[Symbol] = emptySymbolSet
      var defaultIndexSet = new BitSet(pats.size)
      protected var tagIndices = IntMap.empty[List[Int]]

      def insertDefault(tag: Int, vs: Traversable[Symbol]): Unit = {
        defaultIndexSet += tag
        defaultV = defaultV ++ vs
      }

      def haveDefault: Boolean    = !defaultIndexSet.isEmpty
      def defaultRows: List[Row]  = defaultIndexSet.toList reverseMap grabRow

      protected def grabRow(index: Int): Row = {
        val r = rest.row(index)
        if (defaultV.isEmpty) r
        else r.rebind2(pats(index).boundVariables, scrut.sym)  // get vars
      }

      /** inserts row indices using in to list of tagIndices */
      protected def tagIndicesToReps() : List[(Int, Rep)] =
        tagIndices map { case (k, v) => (k, make(rest.temp, (v reverseMap grabRow) ::: defaultRows)) } toList

      protected def defaultsToRep() = make(rest.temp, defaultRows)

      protected def insertTagIndexPair(tag: Int, index: Int) =
        tagIndices = tagIndices.update(tag, index :: tagIndices.getOrElse(tag, Nil))

      /** returns
       *  @return list of continuations,
       *  @return variables bound to default continuation,
       *  @return optionally, a default continuation
       **/
      def getTransition(): (List[(Int,Rep)], Set[Symbol], Option[Rep]) =
        (tagIndicesToReps, defaultV, if (haveDefault) Some(defaultsToRep) else None)

      val varMap: List[(Int, List[Symbol])] =
        (for ((x, i) <- pats.zip) yield unbind(x) match {
          case p @ LIT(c: Int)          => insertTagIndexPair(c, i)           ; Some(c,       definedVars(x))
          case p @ LIT(c: Char)         => insertTagIndexPair(c.toInt, i)     ; Some(c.toInt, definedVars(x))
          case p if isDefaultPattern(p) => insertDefault(i, x.boundVariables) ; None
        }) flatMap (x => x) reverse

      // lazy
      private def bindVars(Tag: Int, orig: Bindings): Bindings = {
        def myBindVars(rest:List[(Int,List[Symbol])], bnd: Bindings): Bindings = rest match {
          case Nil => bnd
          case (Tag,vs)::xs => myBindVars(xs, bnd.add(vs, scrut.sym))
          case (_,  vs)::xs => myBindVars(xs, bnd)
        }
        myBindVars(varMap, orig)
      }

      final def tree(): Tree = {
        val (branches, defaultV, defaultRep) = this.getTransition // tag body pairs
        val cases = for ((tag, r) <- branches) yield {
          val r2 = make(r.temp, r.row map (x => x rebind bindVars(tag, x.subst)))

          CASE(Literal(tag)) ==> r2.toTree
        }
        lazy val ndefault         = defaultRep map (_.toTree) getOrElse (failTree)
        lazy val casesWithDefault = cases ::: List(CASE(WILD(IntClass.tpe)) ==> ndefault)

        cases match {
          case CaseDef(lit,_,body) :: Nil   => IF (scrut.id ANY_== lit) THEN body ELSE ndefault
          case _                            =>
            val target: Tree = if (scrut.tpe.isChar) scrut.id DOT nme.toInt else scrut.id // chars to ints
            target MATCH (casesWithDefault: _*)
        }
      }
      override def toString = {
        "MixLiterals {\n  pats: %s\n  varMap: %s\n}".format(
          pats, varMap
        )
      }
    }

    /** mixture rule for unapply pattern
     */
    class MixUnapply(val pats: Patterns, val rest: Rep) extends RuleApplication {
      lazy val Strip(vs, ua @ UnApply(app @ Apply(fxn, _ :: applyTail), args)) = head.tree

      object sameUnapplyCall {
        def sameFunction(fn1: Tree) = fxn.symbol == fn1.symbol && (fxn equalsStructure fn1)
        def unapply(t: Tree) = t match {
          case UnApply(Apply(fn1, _), args) if sameFunction(fn1)  => Some(args)
          case _                                                  => None
        }
      }

      def newVarCapture(pos: Position, tpe: Type) =
        newVar(pos, tpe, flags = scrut.flags)

      /** returns (unapply-call, success-rep, optional fail-rep*/
      final def getTransition(): Branch[UnapplyCall] = {
        val unapplyRes  = newVarCapture(ua.pos, app.tpe)
        val rhs         = Apply(fxn, scrut.id :: applyTail) setType unapplyRes.tpe
        val zipped      = pats zip rest.row
        val nrowsOther  = zipped.tail flatMap {
          case (Stripped(sameUnapplyCall(_)), _)  => Nil
          case (pat, r)                           => List(r insert pat)
        }

        def mkTransition(vdefs: List[Tree], ntemps: List[Symbol], nrows: List[Row]) =
          Branch(
            UnapplyCall(typedValDef(unapplyRes, rhs), vdefs),
            mkNewRep(ntemps, rest.temp, nrows),
            if (nrowsOther.isEmpty) None else Some(mkFail(nrowsOther))
          )

        // Second argument is number of dummies to prepend in the default case
        def mkNewRows(sameFilter: (List[Tree]) => List[Tree], dum: Int) =
          for ((pat @ Strip(vs, p), r) <- zipped) yield p match {
            case sameUnapplyCall(args)  => r.insert2(sameFilter(args) ::: List(EmptyTree), vs, scrut.sym)
            case _                      => r insert (getDummies(dum) ::: List(pat))
          }
        def mkGet(s: Symbol) = typedValDef(s, fn(ID(unapplyRes), nme.get))
        def mkVar(tpe: Type) = newVarCapture(ua.pos, tpe)

        // 0 args => Boolean, 1 => Option[T], >1 => Option[? <: ProductN[T1,...,Tn]]
        args.length match {
          case 0  =>
            mkTransition(Nil, Nil, mkNewRows((xs) => Nil, 0))

          case 1 =>
            val vsym  = mkVar(app.tpe typeArgs 0)
            val nrows = mkNewRows(xs => List(xs.head), 1)
            val vdef  = mkGet(vsym)

            mkTransition(List(vdef), List(vsym), nrows)

          case _ =>
            val uresGet   = mkVar(app.tpe typeArgs 0)
            val vdefHead  = mkGet(uresGet)
            val ts        = getProductArgs(uresGet.tpe).get
            val nrows     = mkNewRows(identity, ts.size)

            val (vdefs: List[Tree], vsyms: List[Symbol]) = List.unzip(
              for ((vtpe, i) <- ts.zip((1 to ts.size).toList)) yield {
                val vchild  = mkVar(vtpe)
                val accSym  = productProj(uresGet, i)
                val rhs     = typer typed fn(ID(uresGet), accSym)

                (typedValDef(vchild, rhs), vchild)
              })

            mkTransition(vdefHead :: vdefs, vsyms, nrows)
        }
      } /* def getTransition(...) */

      final def tree() = {
        val Branch(UnapplyCall(uacall, vdefs), srep, frep) = this.getTransition
        val succ = srep.toTree
        val fail = frep map (_.toTree) getOrElse (failTree)
        val cond =
          if (uacall.symbol.tpe.isBoolean) ID(uacall.symbol)
          else uacall.symbol IS_DEFINED

        typer typed squeezedBlock(
          List(handleOuter(uacall)),
          IF (cond) THEN squeezedBlock(vdefs, succ) ELSE fail
        )
      }
      override def toString = {
        "MixUnapply {\n  pats: %s\n  ua: %s\n}".format(
          pats, ua
        )
      }
    }

    /** handle sequence pattern and ArrayValue (but not star patterns)
     */
    sealed class MixSequence(val pats: Patterns, val rest: Rep) extends RuleApplication {
      /** array elements except for star (if present) */
      protected def nonStarElems(x: ArrayValue) =
        if (isRightIgnoring(x)) x.elems.init else x.elems

      protected def elemLength(x: ArrayValue)     = nonStarElems(x).length
      protected def isAllDefaults(x: ArrayValue)  = nonStarElems(x) forall isDefaultPattern

      final def removeStar(xs: List[Tree]): List[Tree] =
        xs.init ::: List(makeBind(xs.last.boundVariables, WILD(scrut.seqType)))

      protected def getSubPatterns(len: Int, x: Tree): Option[List[Tree]] = condOpt(x) {
        case av @ ArrayValue(_,xs) if !isRightIgnoring(av) && xs.length == len   => xs ::: List(EmptyTree)
        case av @ ArrayValue(_,xs) if  isRightIgnoring(av) && xs.length == len+1 => removeStar(xs) // (*)
        case EmptyTree | WILD()                                                  => getDummies(len + 1)
      }

      protected def makeSuccRep(vs: List[Symbol], tail: Symbol, nrows: List[Row]) =
        make(vs ::: tail :: rest.temp, nrows)

      /** True if 'next' must be checked even if 'first' failed to match after passing its length test
        * (the conditional supplied by getPrecondition.) This is an optimization to avoid checking sequences
        * which cannot match due to a length incompatibility.
        */
      protected def mustCheck(first: Tree, next: Tree): Boolean =
        (first ne next) && (isDefaultPattern(next) || cond((first, next)) {
          case (av: ArrayValue, bv: ArrayValue) =>
            // number of non-star elements in each sequence
            val (firstLen, nextLen) = (elemLength(av), elemLength(bv))

            // !isAllDefaults(av) ||
            ((isRightIgnoring(av), isRightIgnoring(bv)) match {
              case (true, true)   => nextLen < firstLen   // Seq(a,b,c,_*) followed by Seq(a,b,_*) because of (a,b)
              case (true, false)  =>
                isAllDefaults(av) && (nextLen < firstLen)
                // nextLen < firstLen   // Seq(a,b,c,_*) followed by Seq(a,b) because of (a,b)
              case (false, true)  => true
              // case (false, true)  => nextLen <= firstLen  // Seq(a,b) followed by Seq(a,b,c,_*) cannot match since len == 2
              // however that conditional causes the following to fail with 2nd case unreachable:
              // def not_unreachable2(xs:Seq[Char]) = xs match {
              //   case Seq(x, y) => x::y::Nil
              //   case Seq(x, y, z, _*) => List(x,y)
              // }
              // ...which means this logic must be applied more broadly than I had inferred from the comment
              // "...even if [x] failed to match *after* passing its length test." So one would think that means
              // the second case would only not be tried if scrut.length == 2, and reachable the rest of the time.
              case (false, false) => nextLen == firstLen   // same length (self compare ruled out up top)
            })
        })

      case class TransitionContext(f: TreeFunction2)

      // context (to be used in IF), success and failure Rep
      def getTransition(): Branch[TransitionContext] = {
        scrut assertIsSubtype head.tpe   // scrut.tpe <:< column.head.tpe confirmed by assertion
        val av @ ArrayValue(_, elems)   = head.tree
        val ys                          = if (isRightIgnoring(av)) elems.init else elems
        val vs                          = ys map (y => newVar(unbind(y).pos, scrut.elemType))
        def scrutineeCopy               = scrut.id.duplicate

        lazy val tail                   = newVar(scrut.pos, scrut.seqType)
        lazy val lastBinding            = typedValDef(tail, scrutineeCopy DROP ys.size)
        def elemAt(i: Int)              = typer typed ((scrutineeCopy DOT (scrutineeCopy.tpe member nme.apply))(LIT(i)))

        val bindings =
          (vs.zipWithIndex map { case (v,i) => typedValDef(v, elemAt(i)) }) ::: List(lastBinding)

        val (nrows, frows) = List.unzip(
          for ((c, row) <- pats zip rest.row) yield getSubPatterns(ys.size, c) match {
            case Some(ps) => (Some(row insert ps), if (mustCheck(av, c)) Some(row insert c) else None)
            case None     => (None, Some(row insert c))
          })

        val succ = makeSuccRep(vs, tail, nrows flatMap (x => x))
        val fail = mkFail(frows flatMap (x => x))
        def transition = (thenp: Tree, elsep: Tree) =>
          IF (getPrecondition(scrutineeCopy, elemLength(av))) THEN squeezedBlock(bindings, thenp) ELSE elsep

        Branch(TransitionContext(transition), succ, Some(fail))
      }

      protected def lengthCheck(tree: Tree, len: Int, op: TreeFunction2) = {
        def compareOp = head.tpe member nme.lengthCompare  // symbol for "lengthCompare" method
        typer typed op((tree.duplicate DOT compareOp)(LIT(len)), ZERO)
      }

      // precondition for matching: sequence is exactly length of arg
      protected def getPrecondition(tree: Tree, lengthArg: Int) =
        lengthCheck(tree, lengthArg, _ ANY_== _)

      final def tree() = {
        val Branch(TransitionContext(transition), succ, Some(fail)) = this.getTransition
        transition(succ.toTree, fail.toTree)
      }
    }

    /** handle sequence pattern and ArrayValue with star patterns
     */
    final class MixSequenceStar(pats: Patterns, rest: Rep) extends MixSequence(pats, rest) {
      // in principle, we could optimize more, but variable binding gets complicated (@todo use finite state methods instead)
      override def getSubPatterns(minlen: Int, x: Tree) = condOpt(x) {
        case av @ ArrayValue(_,xs) if (!isRightIgnoring(av) && xs.length   == minlen) =>  // Seq(p1,...,pN)
          xs ::: List(gen.mkNil, EmptyTree)
        case av @ ArrayValue(_,xs) if ( isRightIgnoring(av) && xs.length-1 == minlen) =>  // Seq(p1,...,pN,_*)
          removeStar(xs) ::: List(EmptyTree)
        case av @ ArrayValue(_,xs) if ( isRightIgnoring(av) && xs.length-1  < minlen) =>  // Seq(p1..,pJ,_*)   J < N
          getDummies(minlen + 1) ::: List(x)
        case EmptyTree | WILD()                   =>
          getDummies(minlen + 1          + 1)
      }

      override protected def makeSuccRep(vs: List[Symbol], tail: Symbol, nrows: List[Row]) =
        mkNewRep(vs ::: List(tail), rest.temp, nrows)

      // precondition for matching
      override protected def getPrecondition(tree: Tree, lengthArg: Int) =
        lengthCheck(tree, lengthArg, _ ANY_>= _)
    }

    // @todo: equals test for same constant
    class MixEquals(val pats: Patterns, val rest: Rep) extends RuleApplication {
      /** condition (to be used in IF), success and failure Rep */
      final def getTransition(): (Branch[Tree], Symbol) = {
        val vlue = (head.tpe: @unchecked) match {
          case TypeRef(_,_,List(SingleType(pre,sym))) => REF(pre, sym)
          case TypeRef(_,_,List(PseudoType(o)))       => o.duplicate
        }
        assert(vlue.tpe ne null, "value tpe is null")
        val vs        = head.boundVariables
        val nsuccFst  = rest.row.head.insert2(List(EmptyTree), vs, scrut.sym)
        val failLabel = owner.newLabel(scrut.pos, newName(scrut.pos, "failCont%")) // warning, untyped
        val sx        = shortCut(failLabel)     // register shortcut
        val nsuccRow  = nsuccFst :: Row(getDummies( 1 /* scrutinee */ + rest.temp.length), NoBinding, NoGuard, sx) :: Nil

        // todo: optimize if no guard, and no further tests
        val nsucc = mkNewRep(Nil, rest.temp, nsuccRow)
        val nfail = mkFail(List.map2(rest.row.tail, pats.tail.ps)(_ insert _))

        (Branch(typer typed (scrut.id ANY_== vlue), nsucc, Some(nfail)), failLabel)
      }

      final def tree() = {
        val (Branch(cond, srep, Some(frep)), failLabel) = this.getTransition
        val fail  = typer typed frep.toTree
        failLabel setInfo MethodType(Nil, fail.tpe)

        typer typed(
          IF (handleOuter(cond)) THEN srep.toTree ELSE LabelDef(failLabel, Nil, fail)
        )
      }
    }

    /** mixture rule for type tests
    **/
    class MixTypes(val pats: Patterns, val rest: Rep) extends RuleApplication {
      private def subpatterns(p: Tree): List[Tree] = p match {
        case Bind(_, p)                                                 => subpatterns(p)
        case app @ Apply(fn, ps) if isCaseClass(app.tpe) && fn.isType   => if (pats.isCaseHead) ps else pats.dummies
        case Apply(fn, xs) if !xs.isEmpty || fn.isType                  => abort("strange Apply")
        case _                                                          => pats.dummies
      }

      // moreSpecific: more specific patterns
      //     subsumed: more general patterns (subsuming current), row index and subpatterns
      //    remaining: remaining, row index and pattern
      def join[T](xs: List[Option[T]]): List[T] = xs.flatMap(x => x)
      val (moreSpecific, subsumed, remaining) : (List[Tree], List[(Int, List[Tree])], List[(Int, Tree)]) = unzip3(
        for ((pat @ Stripped(spat), j) <- pats.zip) yield {
          def eqHead(tpe: Type)         = pats.headType =:= tpe
          def alts(yes: Tree, no: Tree) = if (eqHead(pat.tpe)) yes else no

          lazy val isDef                = isDefaultPattern(pat)
          lazy val cmp: TypeComparison  = spat.tpe cmp pats.headType  // contains type info about pattern's type vs. head pattern
          lazy val dummy                = (j, pats.dummies)
          lazy val pass                 = (j, pat)
          lazy val subs                 = (j, subpatterns(pat))

          import cmp._  // imports xIsaY, etc.

          // each pattern will yield a triple of options corresponding to the three lists,
          // which will be flattened down to the values
          implicit def mkOpt[T](x: T): Option[T] = Some(x)    // limits noise from Some(value)
          (spat match {
            case LIT(null) if !eqHead(spat.tpe)               => (None, None, pass)           // special case for constant null
            case _ if pats.isObjectTest(pat)                  => (EmptyTree, dummy, None)     // matching an object
            case Typed(p @ Stripped(_: UnApply), _) if xIsaY  => (p, dummy, None)             // <:< is never <equals>
            case q @ Typed(pp, _) if xIsaY                    => (alts(pp, q), dummy, None)   // never =:= for <equals>
            // this next line inflicted great suffering upon innocents
            // case z: UnApply                                   => (None, None, pass)
            // XXX note - while removing the above line fixed the abhorrent "wrong answer" behavior
            // illustrated in bug #1697, it then led to "consistency problem in target generation"
            // failures with extractors in the first position (see classifyPat.)
            case z: UnApply                                   => (EmptyTree, dummy, pass)
            case _ if erased.xIsaY || xIsaY && !isDef         => (alts(EmptyTree, pat), subs, None) // never =:= for <equals>
            case _ if erased.yIsaX || yIsaX || isDef          => (EmptyTree, dummy, pass)     // subsuming (matched *and* remaining pattern)
            case _                                            => (None, None, pass)
          }) : (Option[Tree], Option[(Int, List[Tree])], Option[(Int, Tree)])
        }
      ) match { case (x,y,z) => (join(x), join(y), join(z)) }

      override def toString = {
        "MixTypes(%s: %s) {\n  moreSpecific: %s\n  subsumed: %s\n  remaining: %s\n}".format(
          scrut, scrut.tpe, moreSpecific, subsumed, remaining
        )
      }

      /** returns casted symbol, success matrix and optionally fail matrix for type test on the top of this column */
      final def getTransition(): Branch[Scrutinee] = {
        val casted = scrut casted pats.headType

        // succeeding => transition to translate(subsumed) (taking into account more specific)
        val nmatrix = {
          val ms = moreSpecific exists (_ != EmptyTree)
          val accessorTemps =
            if (!pats.isCaseHead) Nil
            else casted.accessors map (meth => newVar(scrut.pos, (casted.tpe memberType meth).resultType, scrut.flags))
          val subtestTemps = if (!ms) Nil else List(casted.sym)
          val subtests =
            if (!ms) subsumed
            else moreSpecific.zip(subsumed) map { case (mspat, (j, pats)) => (j, mspat::pats) }
          val ntriples =
            for ((j, ps) <- subtests ; val Strip(vs, thePat) = pats(j)) yield
              (rest row j).insert2(ps, vs, casted.sym)

          make(subtestTemps ::: accessorTemps ::: rest.temp, ntriples)
        }

        // fails      => transition to translate(remaining)
        val nmatrixFail: Option[Rep] = {
          val ntriples = for ((j, pat) <- remaining) yield (rest row j) insert pat
          if (ntriples.isEmpty) None else Some(mkFail(ntriples))
        }

        Branch(casted, nmatrix, nmatrixFail)
      }

      // temporary checks so we're less crashy while we determine what to implement.
      def checkErroneous(scrut: Scrutinee): Type = {
        scrut.tpe match {
          case tpe @ ThisType(_) if tpe.termSymbol == NoSymbol        =>
            cunit.error(scrut.pos, "self type test in anonymous class forbidden by implementation.")
            ErrorType
          case x => x
        }
      }

      final def tree(): Tree = {
        val Branch(casted, srep, frep) = this.getTransition
        val castedTpe = checkErroneous(casted)
        val cond = condition(castedTpe, scrut)
        val succ = srep.toTree
        val fail = frep map (_.toTree) getOrElse (failTree)

        // dig out case field accessors that were buried in (***)
        val cfa       = if (!pats.isCaseHead) Nil else casted.accessors
        val caseTemps = srep.temp match { case x :: xs if x == casted.sym => xs ; case x => x }
        def needCast  = if (casted.sym ne scrut.sym) List(VAL(casted.sym) === (scrut.id AS_ANY castedTpe)) else Nil
        val vdefs     = needCast ::: (
          for ((tmp, accessor) <- caseTemps zip cfa) yield
            typedValDef(tmp, typer typed fn(casted.id, accessor))
        )

        typer typed (IF (cond) THEN squeezedBlock(vdefs, succ) ELSE fail)
      }
    }

    case class Row(pat: List[Tree], subst: Bindings, guard: Guard, bx: Int) {
      def insert(h: Tree)                               = copy(pat = h :: pat)
      def insert(hs: List[Tree])                        = copy(pat = hs ::: pat)    // prepends supplied tree
      def replace(hs: List[Tree])                       = copy(pat = hs)            // substitutes for patterns
      def rebind(b: Bindings)                           = copy(subst = b)           // substitutes for bindings

      def rebind2(vs: Iterable[Symbol], temp: Symbol) =
        copy(subst = subst.add(vs, temp))
      def insert2(hs: List[Tree], vs: Iterable[Symbol], temp: Symbol) =             // prepends and prepends
        copy(pat = hs ::: pat, subst = subst.add(vs, temp))

      def insert(p: Pattern)                            = copy(pat = p.tree :: pat) // transitioning to patterns

      /** returns true if the patterns in this row cover a type symbols "combination" and there is no guard
       *  @param comb pairs of (column index, type symbol)
       */
      def covers(combos: List[Combo]) =
        guard.isEmpty && (combos forall (c => c isCovered pat(c.index)))

      // returns this row with alternatives expanded
      def expand(classifyPat: (Tree, Int) => Tree): List[Row] = {
        def getAlternativeBranches(p: Tree): List[Tree] = {
          def get_BIND(pctx: TreeFunction1, p:Tree): List[Tree] = p match {
            case b @ Bind(n,p)   => get_BIND((x: Tree) => pctx(treeCopy.Bind(b, n, x) setType x.tpe), p)
            case Alternative(ps) => ps map pctx
          }
          logAndReturn("get_BIND: ", get_BIND(x => x, p))
        }

        val indexOfAlternative            = pat findIndexOf isAlternative
        val pats: List[Tree]              = List.map2(pat, pat.indices.toList)(classifyPat)
        lazy val (prefix, alts :: suffix) = pats splitAt indexOfAlternative
        lazy val alternativeBranches      = getAlternativeBranches(alts) map (x => replace(prefix ::: x :: suffix))

        if (indexOfAlternative == -1) List(replace(pats)) else alternativeBranches
      }
      override def toString() = {
        val patStr = pat.mkString
        val others = List(subst, guard) map (_.toString) filter (_ != "")
        val otherStr = if (others.isEmpty) "" else " // " + others.mkString(" ")

        "Row(%d) %s%s".format(bx, patStr, otherStr)
      }
    }
    case class Combo(index: Int, sym: Symbol) {
      // is this combination covered by the given pattern?
      def isCovered(p: Tree) = cond(unbind(p)) {
        case _: UnApply | _: ArrayValue => true
        case _                          => isDefaultPattern(p) || (p.tpe coversSym sym)
      }
    }
    case class SetCombo(index: Int, syms: Set[Symbol]) {}
    case class Branch[T](action: T, succ: Rep, fail: Option[Rep])
    case class UnapplyCall(ua: Tree, args: List[Tree])

    case class Rep(val temp: List[Symbol], val row: List[Row]) {
      import Flags._

      /** Converts this to a tree - recursively acquires subreps. */
      final def toTree(): Tree = this.applyRule.tree

      private def toUse(s: Symbol) =
         (s hasFlag MUTABLE) &&                 // indicates that have not yet checked exhaustivity
        !(s hasFlag TRANS_FLAG) &&              // indicates @unchecked
         (s.tpe.typeSymbol hasFlag SEALED)

      // the superclass is taken if it is not abstract
      private def countSuper(s: Symbol): Set[Symbol]  = if (s hasFlag ABSTRACT) emptySymbolSet else Set(s)
      private def countSymbol(s: Symbol): Set[Symbol] = candidates(s) ++ countSuper(s)
      private def candidates(s: Symbol): Set[Symbol]  =
        if (s hasFlag SEALED) s.children flatMap countSymbol
        else emptySymbolSet

      private def setsToCombine: List[(Int, Set[Symbol])] =
        for ((sym, i) <- temp.zipWithIndex ; if toUse(sym)) yield {
          sym resetFlag MUTABLE
          (i, candidates(sym.tpe.typeSymbol))
        }

      // computes cartesian product, keeps indices available
      private def combine(colcom: List[(Int, Set[Symbol])]): List[List[Combo]] = colcom match {
        case Nil              => Nil
        case (i, syms) :: Nil => syms.toList map (s => List(Combo(i, s)))
        case (i, syms) :: cs  => for (s <- syms.toList; rest <- combine(cs)) yield Combo(i, s) :: rest
      }

      /*   internal representation is (temp:List[Symbol], row:List[Row])
       *
       *         tmp1       tmp_m
       */
      final def applyRule(): RuleApplication = {
        def dropIndex[T](xs: List[T], n: Int) = (xs take n) ::: (xs drop (n + 1))
        if (row.isEmpty)
          return ErrorRule()

        val Row(pats, subst, guard, index) = row.head
        def guardedRest =
          if (guard.isEmpty) null else make(temp, row.tail)

        val (defaults, others) = pats span (p => isDefaultPattern(unbind(p)))

        /** top-most row contains only variables/wildcards */
        if (others.isEmpty) {
          val binding = (defaults map { case Strip(vs, _) => vs } zip temp) .
            foldLeft(subst)((b, pair) => b.add(pair._1, pair._2))

          return VariableRule(binding, guard, guardedRest, index)
        }

        val rpat @ Strip(vs, p) = others.head
        val px = defaults.size
        // Row( _  ... _ p_1i  ...  p_1n   g_m  b_m ) :: rows
        // cut out column px that contains the non-default pattern
        val column    = rpat :: (row.tail map (_ pat px))
        val restTemp  = dropIndex(temp, px)
        val restRows  = row map (r => r replace dropIndex(r.pat, px))

        MixtureRule(new Scrutinee(temp(px)), column, make(restTemp, restRows))
      }

      def checkExhaustive: this.type = {
        val allcomb = combine(setsToCombine)
        if (allcomb forall (combo => row exists (_ covers combo)))
          return this

        // if we reach here, patterns were not exhaustive
        def mkPad(xs: List[Combo], i: Int): String = xs match {
          case Nil                    => pad("*")
          case Combo(j, sym) :: rest  => if (j == i) pad(sym.name.toString) else mkPad(rest, i)
        }
        def mkMissingStr(open: List[Combo]) =
          "missing combination " + temp.indices.map(mkPad(open, _)).mkString + "\n"

        val missingCombos = allcomb filter (open => row.forall(!_.covers(open))) map mkMissingStr
        cunit.warning(temp.head.pos, "match is not exhaustive!\n" + missingCombos.mkString)
        this
      }

      // a fancy toString method for debugging
      override def toString() = {
        val tempStr = (temp map (t => pad(t.name))).mkString
        val underlines = tempStr.replaceAll("""\S""", "-")
        val rowStr = (
          for (Row(pat, subst, guard, bx) <- row) yield {
            val extraStr: String = guard.toString + subst
            "%s %s\n".format(pat map pad mkString, extraStr)
          }
        ) mkString

        if (temp.size == 0) "Rep(%dx%d)".format(temp.size, row.size)
        else "Rep(%dx%d)\n%s\n%s\n%s".format(temp.size, row.size, tempStr, underlines, rowStr)
      }

      private val NPAD = 15

      private def typeToString(t: Type): String = t match {
        case NoType => "x"
        case x      => x.toString
      }
      private def symbolToString(s: Symbol): String = s match {
        case x  => x.toString
      }
      private def treeToString(t: Tree): String = unbind(t) match {
        case EmptyTree            => "?"
        case WILD()               => "_"
        case Literal(Constant(x)) => "LIT(%s)".format(x)
        case Apply(fn, args)      => "%s(%s)".format(treeToString(fn), args map treeToString mkString ",")
        case x: TypeTree          => "TT(%s)".format(symbolToString(x.symbol))
        case Typed(expr, tpt)     => "%s: %s".format(treeToString(expr), treeToString(tpt))
        case x                    =>  x.toString + " (" + x.getClass + ")"
      }

      private def pad(s: Any): String = pad(s match {
        case x: Tree    => treeToString(x)
        // case x: AnyRef  => x.toString + " (" + x.getClass + ")"
        case x                  => x.toString
      })
      private def pad(s: String): String = "%%%ds" format (NPAD - 1) format s
    }

    /** Expands the patterns recursively. */
    final def expand(roots: List[Symbol], cases: List[Tree]): (List[Row], List[Tree], List[List[Symbol]]) = {
      val res = unzip3(
        for ((CaseDef(pat, guard, body), index) <- cases.zipWithIndex) yield {
          def mkRow(ps: List[Tree]) = Some(Row(ps, NoBinding, Guard(guard), index))
          def rowForPat: Option[Row] = pat match {
            case _ if roots.length <= 1 => mkRow(List(pat))
            case Apply(fn, args)        => mkRow(args)
            case WILD()                 => mkRow(getDummies(roots.length))
            case _                      => None
          }

          (rowForPat, body, definedVars(pat))
        }
      )

      res match { case (rows, finals, vars) => (rows flatMap (x => x), finals, vars) }
    }

    /** returns the condition in "if (cond) k1 else k2"
     */
    final def condition(tpe: Type, scrut: Scrutinee): Tree = {
      assert(scrut.isDefined)
      val cond = handleOuter(condition(tpe, scrut.id))

      if (!needsOuterTest(tpe, scrut.tpe, owner)) cond
      else addOuterCondition(cond, tpe, scrut.id, handleOuter)
    }

    final def condition(tpe: Type, scrutTree: Tree): Tree = {
      assert((tpe ne NoType) && (scrutTree.tpe ne NoType))
      def useEqTest         = tpe.termSymbol.isModule || (tpe.prefix eq NoPrefix)

      typer typed (tpe match {
        case ct: ConstantType => ct.value match {
            case v @ Constant(null) if isAnyRef(scrutTree.tpe)  => scrutTree ANY_EQ NULL
            case v                                              => scrutTree ANY_== Literal(v)
          }
        case _: SingletonType if useEqTest                      =>
          // See ticket #1503 for why both these checks are necessary.
          (REF(tpe.termSymbol) ANY_== scrutTree) AND (scrutTree IS tpe.widen)

        case _ if scrutTree.tpe <:< tpe && isAnyRef(tpe)        => scrutTree OBJ_!= NULL
        case _                                                  => scrutTree IS tpe
      })
    }

    /** adds a test comparing the dynamic outer to the static outer */
    final def addOuterCondition(cond: Tree, tpe2test: Type, scrut: Tree, handleOuter: TreeFunction1) = {
      val theRef = handleOuter(tpe2test match {
        case TypeRef(NoPrefix, _, _)          => abort("assertion failed: NoPrefix")
        case TypeRef(ThisType(clazz), _, _)   => THIS(clazz)
        case TypeRef(prefix, _, _)            => REF(prefix.prefix, prefix.termSymbol)
      })

      outerAccessor(tpe2test.typeSymbol) match {
        case NoSymbol => ifDebug(cunit.warning(scrut.pos, "no outer acc for " + tpe2test.typeSymbol)) ; cond
        case outerAcc => cond AND (((scrut AS_ANY tpe2test) DOT outerAcc)() ANY_EQ theRef)
      }
    }
  }
}
