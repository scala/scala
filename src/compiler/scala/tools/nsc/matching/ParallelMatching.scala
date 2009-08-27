/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * Copyright 2007 Google Inc. All Rights Reserved.
 * Author: bqe@google.com (Burak Emir)
 */
// $Id$

package scala.tools.nsc
package matching

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
 *   internal representation is (tvars:List[Symbol], rows:List[Row])
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
  import symtab.Flags
  import Types._
  import CODE._
  import scala.Function.tupled

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
  def isSwitchableConst(t: Tree)  = cond(unbind(t)) { case Literal(x: Constant) => isSwitchableTag(x.tag) }

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

    final def isDefault       = isDefaultPattern(tree)

    lazy val Strip(boundVariables, stripped) = tree

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

    private lazy val expandResult       = expand(roots, cases)
    lazy val targets: List[FinalState]  = expandResult._2
    lazy val vss: List[List[Symbol]]    = expandResult._3
    lazy val expansion: Rep             = make(roots, expandResult._1)

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

        val body    = targets(bx).body
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
      val body  = targets(bx).body
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
        println(debugConsistencyFailure())
        // TRACE(debugConsistencyFailure())
        cunit.error(body.pos, msg)
      }

      def vds = for (v <- vss(bx) ; substv <- subst(v)) yield typedValDef(v, substv)

      if (isLabellable(body)) ID(label) APPLY (args)
      else                    squeezedBlock(vds, body.duplicate setType resultType)
    }

    /** the injection here handles alternatives and unapply type tests */
    final def make(tvars: List[Symbol], row1: List[Row]): Rep = {
      // Martin: I am not really sure what stype is doing, but it caused aliases.scala to fail
      // The problem is if a val has a singleType of some other module. Then isModule is true and
      // sType is called. But it ends up mixing the prefix of the other module with the val symbol
      // which then causes erasure to be confused.
      def mkSingletonType(x: Tree) = x.tpe match {
        case st: SingleType => st
        case _              => singleType(x.tpe.prefix, x.symbol)
      }
      def equalsCheck(x: Tree) =
        if (x.symbol.isValue) singleType(NoPrefix, x.symbol)
        else mkSingletonType(x)

      def classifyPat(opat: Tree, j: Int): Tree = {
        def vars                    = opat.boundVariables
        def rebind(t: Tree)         = makeBind(vars, t)
        def rebindEmpty(tpe: Type)  = mkEmptyTreeBind(vars, tpe)
        def rebindTyped()           = mkTypedBind(vars, equalsCheck(unbind(opat)))

        // @pre for doUnapplySeq: is not right-ignoring (no star pattern) ; no exhaustivity check
        def doUnapplySeq(tptArg: Tree, xs: List[Tree]) = {
          tvars(j) setFlag Flags.TRANS_FLAG
          rebind(normalizedListPattern(xs, tptArg.tpe))
        }

        def doUnapplyApply(ua: UnApply, fn: Tree) = {
          val MethodType(List(arg, _*), _) = fn.tpe
          val npat =
            if (tvars(j).tpe <:< arg.tpe) ua
            else Typed(ua, TypeTree(arg.tpe)) setType arg.tpe

          // TRACE("doUnapplyApply: %s <:< %s == %s", tvars(j).tpe, argtpe, (tvars(j).tpe <:< argtpe))
          logAndReturn("doUnapplyApply: ", rebind(npat) setType arg.tpe)
        }
        def doValMatch(x: Tree, fn: Tree) = {
          def examinePrefix(path: Tree) = (path, path.tpe) match {
            case (_, t: ThisType)     => singleType(t, x.symbol)            // cases 2/3 are e.g. `case Some(p._2)' in s.c.jcl.Map
            case (_: Apply, _)        => PseudoType(x)                      // outer-matching: test/files/pos/t154.scala
            case _                    => singleType(mkSingletonType(path), x.symbol)  // old
          }
          val singletonType =
            if (isModule(x)) mkSingletonType(x) else fn match {
              case Select(path, _)  => examinePrefix(path)
              case x: Ident         => equalsCheck(x)
            }
          val typeToTest = mkEqualsRef(List(singletonType))

          rebind(Typed(WILD(typeToTest), TypeTree(singletonType)) setType typeToTest)
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
          { case Typed(p @ Stripped(_: UnApply), tpt) => if (tvars(j).tpe <:< tpt.tpe) rebind(p) else opat } ,
          { case x if doReturnOriginal(x)             => opat } ,
          { case x if doRebindTyped(x)                => rebindTyped() } ,  // Ident(_) != nme.WILDCARD
          { case _: This                              => opat } ,
          { case UnapplySeq(tptArg, xs)               => doUnapplySeq(tptArg, xs) } ,
          { case ua @ UnApply(Apply(fn, _), _)        => doUnapplyApply(ua, fn) } ,
          { case x @ Apply_Function(fn)               => doValMatch(x, fn) } ,
          { case Apply_Value(pre, sym)                => rebindEmpty(mkEqualsRef(List(singleType(pre, sym)))) } ,
          { case Apply_CaseClass(tpe, args)           => if (args.isEmpty) rebindEmpty(tpe) else opat } ,
          { case x                                    => abort("Unexpected pattern: " + x.getClass + " => " + x) }
        ) reduceLeft (_ orElse _)

        f(unbind(opat))
      }

      val rows = row1 flatMap (_ expandAlternatives classifyPat)
      if (rows.length != row1.length) make(tvars, rows)  // recursive call if any change
      else Rep(tvars, rows).checkExhaustive
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
      private lazy val trees = ps map (_.tree)
      lazy val head = ps.head
      lazy val tail = Patterns(scrut, ps.tail)
      lazy val last = ps.last.tree
      lazy val headType = head.tpeIfHead
      lazy val isCaseHead = isCaseClass(headType)
      lazy val dummies = if (isCaseHead) getDummies(headType.typeSymbol.caseFieldAccessors.length) else Nil
      lazy val size = ps.length

      def apply(i: Int): Tree = ps(i).tree
      def zip() = trees.zipWithIndex
      def zip[T](others: List[T]) = trees zip others

      def isObjectTest(pat: Pattern)  = pat isObjectTest headType
      def isObjectTest(pat: Tree)     = Pattern(pat) isObjectTest headType

      def extractSimpleSwitch(): Option[(List[Tree], Option[Tree])] = {
        def isSwitchableDefault(x: Tree) = isSwitchableConst(x) || isDefaultPattern(x)
        val (lits, others) = trees span isSwitchableConst
        others match {
          case Nil                                => Some(lits, None)
          // TODO: This needs to also allow the case that the last is a compatible type pattern.
          case List(x) if isSwitchableDefault(x)  => Some(lits, Some(x))
          case _                                  => None
        }
      }

      // an unapply for which we don't need a type test (the scrutinee's static type conforms
      // to the unapply's argument type.)
      object SafeUnapply {
        def unapply(x: Tree): Boolean = cond(x) { case __UnApply(_,tpe,_) => scrut.tpe <:< tpe }
      }

      object SimpleSwitch {
        // TODO - scala> (5: Any) match { case 5 => 5 ; case 6 => 7 }
        // ... should compile to a switch.  It doesn't because the scrut isn't Int/Char, but
        // that could be handle in an if/else since every pattern requires an Int.
        // More immediately, Byte and Short scruts should also work.
        def unapply(x: Patterns) = if (x.scrut.isSimple) x.extractSimpleSwitch else None
      }

      def mkRule(rest: Rep): RuleApplication =
        logAndReturn("mkRule: ", head.tree match {
            case x if isEquals(x.tpe)                 => new MixEquals(this, rest)
            case x: ArrayValue if isRightIgnoring(x)  => new MixSequenceStar(this, rest)
            case x: ArrayValue                        => new MixSequence(this, rest)
            case SafeUnapply()                        => new MixUnapply(this, rest)
            case _ => this match {
              case SimpleSwitch(lits, d)              => new MixLiteralInts(this, rest, lits, d)
              case _                                  => new MixTypes(this, rest)
            }
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

    Let {i1,...,ij} be the rows-indices of the patterns in the column that match c. Since the pat-
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
     *  @precondition: column does not contain alternatives
     */
    def MixtureRule(scrut: Scrutinee, column: List[Tree], rest: Rep): RuleApplication =
      Patterns(scrut, column map Pattern) mkRule rest

    sealed abstract class RuleApplication {
      def pats: Patterns
      def rest: Rep
      lazy val Patterns(scrut, patterns) = pats
      lazy val head = pats.head
      private lazy val sym = scrut.sym

      /** Creates Some(fail rule) even if xs == Nil. */
      def mkFail(xs: List[Row]): Option[Rep] = Some(make(sym :: rest.tvars, xs))

      /** Returns None if xs == Nil, Some(fail rule) otherwise. */
      def mkFailOpt(xs: List[Row]): Option[Rep] = if (xs.isEmpty) None else mkFail(xs)

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
    /** VariableRule: The top-most rows has only variable (non-constructor) patterns. */
    case class VariableRule(subst: Bindings, guard: Guard, guardedRest: Rep, bx: Int) extends RuleApplication {
      def pats: Patterns = impossible
      def rest: Rep = guardedRest

      final def tree(): Tree = {
        def body      = requestBody(bx, subst)
        def guardTest = IF (guard.duplicate.tree) THEN body ELSE guardedRest.toTree

        typer typed(
          if (guard.isEmpty) body
          else squeezedBlock(subst targetParams typer, guardTest)
        )
      }
    }

    /** Mixture rule for all literal ints (and chars) i.e. hopefully a switch
     *  will be emitted on the JVM.
     */
    class MixLiteralInts(
      val pats: Patterns,
      val rest: Rep,
      literals: List[Tree],
      defaultPattern: Option[Tree])
    extends RuleApplication
    {
      private object NUM {
        def unapply(x: Tree): Option[Int] = condOpt(unbind(x)) { case Literal(c) => c.intValue }
      }
      // bound vars and rows for default pattern (only one row, but a list is easier to use later)
      val (defaultVars, defaultRows) = defaultPattern match {
        case None               => (Nil, Nil)
        case Some(Strip(vs, p)) => (vs, List((rest rows literals.size).rebind2(vs, scrut.sym)))
      }
      // literalMap is a map from each literal to a list of row indices.
      // varMap is a list from each literal to a list of the defined vars.
      val (literalMap, varMap) = {
        val tags    = literals map { case NUM(tag) => tag }
        val varMap  = tags zip (literals map definedVars)
        val litMap  =
          tags.zipWithIndex.reverse.foldLeft(IntMap.empty[List[Int]]) {
            // we reverse before the fold so the list can be built with ::
            case (map, (tag, index)) => map.updated(tag, index :: map.getOrElse(tag, Nil))
          }

        (litMap, varMap)
      }

      final def tree(): Tree = {
        // Just a demo of breakIf
        // Interpreter.breakIf(true, "lits" -> literals, "mixlit" -> this, literalMap)

        def bindVars(Tag: Int, orig: Bindings): Bindings = {
          def myBindVars(rest: List[(Int, List[Symbol])], bnd: Bindings): Bindings = rest match {
            case Nil => bnd
            case (Tag,vs)::xs => myBindVars(xs, bnd.add(vs, scrut.sym))
            case (_,  vs)::xs => myBindVars(xs, bnd)
          }
          myBindVars(varMap, orig)
        }

        // creates a row transformer for injecting the default case bindings at a given index
        def addDefaultVars(index: Int): Row => Row =
          if (defaultVars.isEmpty) identity
          else (r: Row) => r.rebind2(pats(index).boundVariables, scrut.sym)

        val cases =
          for ((tag, indices) <- literalMap.toList) yield {
            val newRows = indices map (i => addDefaultVars(i)(rest rows i))
            val r       = make(rest.tvars, newRows ::: defaultRows)
            val r2      = make(r.tvars, r.rows map (x => x rebind bindVars(tag, x.subst)))

            CASE(Literal(tag)) ==> r2.toTree
          }

        val defaultTree       = make(rest.tvars, defaultRows).toTree
        def casesWithDefault  = cases ::: List(CASE(WILD(IntClass.tpe)) ==> defaultTree)

        cases match {
          case List(CaseDef(lit, _, body))  =>
            // only one case becomes if/else
            IF (scrut.id ANY_== lit) THEN body ELSE defaultTree
          case _                            =>
            // otherwise cast to an Int if necessary and run match
            val target: Tree = if (!scrut.tpe.isInt) scrut.id DOT nme.toInt else scrut.id
            target MATCH (casesWithDefault: _*)
        }
      }
      override def toString = {
        "MixLiteralInts {\n  pats: %s\n  varMap: %s\n}".format(
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
        def unapply(t: Tree) = condOpt(t) { case UnApply(Apply(fn1, _), args) if sameFunction(fn1)  => args }
      }

      def newVarCapture(pos: Position, tpe: Type) =
        newVar(pos, tpe, flags = scrut.flags)

      /** returns (unapply-call, success-rep, optional fail-rep*/
      final def getTransition(): Branch[UnapplyCall] = {
        val unapplyRes  = newVarCapture(ua.pos, app.tpe)
        val rhs         = Apply(fxn, scrut.id :: applyTail) setType unapplyRes.tpe
        val zipped      = pats zip rest.rows
        val nrowsOther  = zipped.tail flatMap {
          case (Stripped(sameUnapplyCall(_)), _)  => Nil
          case (pat, r)                           => List(r insert pat)
        }

        def mkTransition(vdefs: List[Tree], ntemps: List[Symbol], nrows: List[Row]) =
          Branch(
            UnapplyCall(typedValDef(unapplyRes, rhs), vdefs),
            mkNewRep(ntemps, rest.tvars, nrows),
            mkFailOpt(nrowsOther)
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
              for ((vtpe, i) <- ts.zipWithIndex) yield {
                val vchild  = mkVar(vtpe)
                val accSym  = productProj(uresGet, i+1)
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
        make(vs ::: tail :: rest.tvars, nrows)

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
              // XXX note this used to say "nextLen == firstLen" and this caused #2187.  Rewrite this.
              case (false, false) => nextLen >= firstLen   // same length (self compare ruled out up top)
            })
        })

      case class TransitionContext(f: TreeFunction2)

      // context (to be used in IF), success and failure Rep
      def getTransition(): Branch[TransitionContext] = {
        scrut assertIsSubtype head.tpe   // scrut.tpe <:< column.head.tpe confirmed by assertion
        val av @ ArrayValue(_, elems)   = head.tree
        val ys                          = if (isRightIgnoring(av)) elems.init else elems
        val vs                          = ys map (y => newVar(unbind(y).pos, scrut.elemType))
        def scrutCopy                   = scrut.id.duplicate

        lazy val tail                   = newVar(scrut.pos, scrut.seqType)
        lazy val lastBinding            = typedValDef(tail, scrutCopy DROP ys.size)
        def elemAt(i: Int)              = typer typed ((scrutCopy DOT (scrutCopy.tpe member nme.apply))(LIT(i)))

        val bindings =
          (vs.zipWithIndex map tupled((v, i) => typedValDef(v, elemAt(i)))) ::: List(lastBinding)

        val (nrows, frows) = List.unzip(
          for ((c, rows) <- pats zip rest.rows) yield getSubPatterns(ys.size, c) match {
            case Some(ps) => (Some(rows insert ps), if (mustCheck(av, c)) Some(rows insert c) else None)
            case None     => (None, Some(rows insert c))
          })

        val succ = makeSuccRep(vs, tail, nrows flatMap (x => x))
        val fail = mkFail(frows flatMap (x => x))
        def transition = (thenp: Tree, elsep: Tree) =>
          IF (getPrecondition(scrutCopy, elemLength(av))) THEN squeezedBlock(bindings, thenp) ELSE elsep

        Branch(TransitionContext(transition), succ, fail)
      }

      protected def lengthCheck(tree: Tree, len: Int, op: TreeFunction2) = {
        def compareOp = head.tpe member nme.lengthCompare  // symbol for "lengthCompare" method
        def cmpFunction(t1: Tree) = op((t1.duplicate DOT compareOp)(LIT(len)), ZERO)
        // first ascertain lhs is not null: bug #2241
        typer typed nullSafe(cmpFunction _)(tree)
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
        mkNewRep(vs ::: List(tail), rest.tvars, nrows)

      // precondition for matching
      override protected def getPrecondition(tree: Tree, lengthArg: Int) =
        lengthCheck(tree, lengthArg, _ ANY_>= _)
    }

    // @todo: equals test for same constant
    class MixEquals(val pats: Patterns, val rest: Rep) extends RuleApplication {
      /** condition (to be used in IF), success and failure Rep */
      final def getTransition(): (Branch[Tree], Symbol) = {
        val value = {
          // how is it known these are the only possible typerefs here?
          val TypeRef(_,_,List(arg)) = head.tpe
          arg match {
            case SingleType(pre, sym) => REF(pre, sym)
            case PseudoType(o)        => o.duplicate
          }
        }

        val label       = owner.newLabel(scrut.pos, newName(scrut.pos, "failCont%")) // warning, untyped
        val succ        = List(
          rest.rows.head.insert2(List(EmptyTree), head.boundVariables, scrut.sym),
          Row(getDummies(1 + rest.tvars.length), NoBinding, NoGuard, shortCut(label))
        )

        // todo: optimize if no guard, and no further tests
        val fail    = mkFail(List.map2(rest.rows.tail, pats.tail.ps)(_ insert _))
        val action  = typer typed (scrut.id ANY_== value)

        (Branch(action, mkNewRep(Nil, rest.tvars, succ), fail), label)
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

    case class PatPair(moreSpecific: Tree, moreGeneral: Tree, index: Int)

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
      //     subsumed: more general patterns (subsuming current), rows index and subpatterns
      //    remaining: remaining, rows index and pattern
      def join[T](xs: List[Option[T]]): List[T] = xs.flatMap(x => x)
      val (moreSpecific, subsumed, remaining) : (List[Tree], List[(Int, List[Tree])], List[(Int, Tree)]) = unzip3(
        for ((pat @ Stripped(spat), j) <- pats.zip) yield {
          def eqHead(tpe: Type)         = pats.headType =:= tpe
          def alts(yes: Tree, no: Tree) = if (eqHead(pat.tpe)) yes else no

          lazy val isDef                = isDefaultPattern(pat)
          lazy val dummy                = (j, pats.dummies)
          lazy val pass                 = (j, pat)
          lazy val subs                 = (j, subpatterns(pat))

          lazy val cmpOld: TypeComp  = spat.tpe cmp pats.headType  // contains type info about pattern's type vs. head pattern
          import cmpOld.{ erased }

          def erased_xIsaY = erased.xIsaY
          def erased_yIsaX = erased.yIsaX

          // scrutinee, pattern
          val (s, p)  = (decodedEqualsType(spat.tpe), decodedEqualsType(pats.headType))
          def xIsaY   = s <:< p
          def yIsaX   = p <:< s

          // XXX exploring what breaks things and what doesn't
          // def dummyIsOk = {
          //   val old = erased.yIsaX || yIsaX || isDef
          //   println("Old logic: %s || %s || %s == %s".format(erased.yIsaX, yIsaX, isDef, erased.yIsaX || yIsaX || isDef))
          //   println("isCaseClass(spat.tpe) = %s, isCaseClass(pats.headType) = %s".format(
          //     isCaseClass(spat.tpe), isCaseClass(pats.headType)))
          //   println("spat.tpe = %s, pats.head = %s, pats.headType = %s".format(
          //     spat.tpe, pats.head, pats.headType))
          //
          //   (erased.yIsaX || yIsaX || isDef)
          //   // (!isCaseClass(spat.tpe) || !isCaseClass(pats.headType))
          // }

          // each pattern will yield a triple of options corresponding to the three lists,
          // which will be flattened down to the values
          implicit def mkOpt[T](x: T): Option[T] = Some(x)    // limits noise from Some(value)
          (spat match {
            case LIT(null) if !eqHead(spat.tpe)               => (None, None, pass)           // special case for constant null
            case _ if pats.isObjectTest(pat)                  => (EmptyTree, dummy, None)     // matching an object
            case Typed(p @ Stripped(_: UnApply), _) if xIsaY  => (p, dummy, None)             // <:< is never <equals>
            case q @ Typed(pp, _) if xIsaY                    => (alts(pp, q), dummy, None)   // never =:= for <equals>
            // case z: UnApply                                   => (None, None, pass)
            case z: UnApply                                   => (EmptyTree, dummy, pass)
            case _ if erased.xIsaY || xIsaY && !isDef         => (alts(EmptyTree, pat), subs, None) // never =:= for <e
            case _ if erased.yIsaX || yIsaX || isDef          => (EmptyTree, dummy, pass)     // subsuming (matched *an
            case _                                            => (None, None, pass)
            // The below fixes bugs 425 and 816 with only the small downside
            // of causing 60 other tests to fail.
            // case _ =>
            //   if (erased_xIsaY || xIsaY && !isDef)  (alts(EmptyTree, pat), subs, None) // never =:= for <equals>
            //   else if (isDef)                       (EmptyTree,           dummy, pass)
            //   else                                  (None,                 None, pass)

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
        val isAnyMoreSpecific = moreSpecific exists (_ != EmptyTree)

        def mkZipped    = moreSpecific zip subsumed map { case (mspat, (j, pats)) => (j, mspat :: pats) }
        def mkAccessors = casted.accessors map (m => newVar(scrut.pos, (casted.tpe memberType m).resultType, scrut.flags))

        val (subtests, subtestVars) =
          if (isAnyMoreSpecific)  (mkZipped, List(casted.sym))
          else                    (subsumed, Nil)

        val accessorVars = if (pats.isCaseHead) mkAccessors else Nil
        val newRows =
          for ((j, ps) <- subtests) yield {
            val Strip(vs, thePat) = pats(j)
            (rest rows j).insert2(ps, vs, casted.sym)
          }

        Branch(
          casted,
          // succeeding => transition to translate(subsumed) (taking into account more specific)
          make(subtestVars ::: accessorVars ::: rest.tvars, newRows),
          // fails      => transition to translate(remaining)
          mkFailOpt(remaining map tupled(rest rows _ insert _))
        )
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
        val cfa         = if (pats.isCaseHead) casted.accessors else Nil
        val caseTemps   = srep.tvars match { case x :: xs if x == casted.sym => xs ; case x => x }
        def castedScrut = typedValDef(casted.sym, scrut.id AS_ANY castedTpe)
        def needCast    = if (casted.sym ne scrut.sym) List(castedScrut) else Nil


        val vdefs       = needCast ::: (
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

      def rebind2(vs: Iterable[Symbol], tvars: Symbol) =
        copy(subst = subst.add(vs, tvars))
      def insert2(hs: List[Tree], vs: Iterable[Symbol], tvars: Symbol) =             // prepends and prepends
        copy(pat = hs ::: pat, subst = subst.add(vs, tvars))

      def insert(p: Pattern)                            = copy(pat = p.tree :: pat) // transitioning to patterns

      /** returns true if the patterns in this rows cover a type symbols "combination" and there is no guard
       *  @param comb pairs of (column index, type symbol)
       */
      def covers(combos: List[Combo]) =
        guard.isEmpty && (combos forall (c => c isCovered pat(c.index)))

      // returns this rows with alternatives expanded
      def expandAlternatives(classifyPat: (Tree, Int) => Tree): List[Row] = {
        // If the given pattern contains alternatives, return it as a list of patterns.
        // Makes typed copies of any bindings found so all alternatives point to final state.
        def newPrev(b: Bind): TreeFunction1 = (x: Tree) => treeCopy.Bind(b, b.name, x) setType x.tpe
        def extractBindings(p: Tree, prevBindings: TreeFunction1 = identity[Tree] _): List[Tree] = p match {
          case b @ Bind(_, body)    => extractBindings(body, newPrev(b))
          case Alternative(ps)      => ps map prevBindings
          case x                    => List(x)  // this shouldn't happen
        }

        // classify all the top level patterns - alternatives come back unaltered
        val newPats: List[Tree] = List.map2(pat, pat.indices.toList)(classifyPat)

        // expand alternatives if any are present
        (newPats indexWhere isAlternative) match {
          case -1     => List(replace(newPats))
          case index  =>
            val (prefix, alts :: suffix) = newPats splitAt index
            // make a new row for each alternative, with it spliced into the original position
            extractBindings(alts) map (x => replace(prefix ::: x :: suffix))
        }
      }
      override def toString() = {
        val patStr = pat.mkString
        val others = List(subst, guard) map (_.toString) filter (_ != "")
        val otherStr = if (others.isEmpty) "" else " // " + others.mkString(" ")

        "Row(%d) %s%s".format(bx, patStr, otherStr)
      }
    }

    case class FinalState(subst: Bindings, body: Tree)

    case class Combo(index: Int, sym: Symbol) {
      // is this combination covered by the given pattern?
      def isCovered(p: Tree) = cond(unbind(p)) {
        case _: UnApply | _: ArrayValue => true
        case _                          => isDefaultPattern(p) || (p.tpe coversSym sym) // typeCoversSym(p.tpe, sym)
      }
    }
    case class SetCombo(index: Int, syms: Set[Symbol]) {}
    case class Branch[T](action: T, succ: Rep, fail: Option[Rep])
    case class UnapplyCall(ua: Tree, args: List[Tree])

    // sealed abstract class Pat {
    //   def isSimple: Boolean
    // }
    // case class SimplePat(pat: Tree) extends Pat {
    //   val isSimple = true
    // }
    // case class ComplexPat(pat: Tree) extends Pat {
    //   val isSimple = false
    // }

    case class Rep(val tvars: List[Symbol], val rows: List[Row]) {
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
        for ((sym, i) <- tvars.zipWithIndex ; if toUse(sym)) yield {
          sym resetFlag MUTABLE
          (i, candidates(sym.tpe.typeSymbol))
        }

      // computes cartesian product, keeps indices available
      private def combine(colcom: List[(Int, Set[Symbol])]): List[List[Combo]] = colcom match {
        case Nil              => Nil
        case (i, syms) :: Nil => syms.toList map (s => List(Combo(i, s)))
        case (i, syms) :: cs  => for (s <- syms.toList; rest <- combine(cs)) yield Combo(i, s) :: rest
      }

      /*   internal representation is (tvars:List[Symbol], rows:List[Row])
       *
       *         tmp1       tmp_m
       */
      final def applyRule(): RuleApplication = {
        def dropIndex[T](xs: List[T], n: Int) = (xs take n) ::: (xs drop (n + 1))

        lazy val Row(pats, subst, guard, index) = rows.head
        lazy val guardedRest        = if (guard.isEmpty) null else make(tvars, rows.tail)
        lazy val (defaults, others) = pats span (p => isDefaultPattern(unbind(p)))

        if (rows.isEmpty) ErrorRule()
        else others match {
          /** top-most rows contains only variables/wildcards */
          case Nil          =>
            val binding = (defaults map (_.boundVariables) zip tvars) .
              foldLeft(subst)((b, pair) => b.add(pair._1, pair._2))

            VariableRule(binding, guard, guardedRest, index)

          /** cut out the column (px) containing the non-default pattern. */
          case (rpat @ Strip(vs, _)) :: _ =>
            val px        = defaults.size
            val column    = rpat :: (rows.tail map (_ pat px))
            val restTemp  = dropIndex(tvars, px)
            val restRows  = rows map (r => r replace dropIndex(r.pat, px))

            MixtureRule(new Scrutinee(tvars(px)), column, make(restTemp, restRows))
        }
      }

      def checkExhaustive: this.type = {
        val allcomb = combine(setsToCombine)
        if (allcomb forall (combo => rows exists (_ covers combo)))
          return this

        // if we reach here, patterns were not exhaustive
        def mkPad(xs: List[Combo], i: Int): String = xs match {
          case Nil                    => pad("*")
          case Combo(j, sym) :: rest  => if (j == i) pad(sym.name.toString) else mkPad(rest, i)
        }
        def mkMissingStr(open: List[Combo]) =
          "missing combination " + tvars.indices.map(mkPad(open, _)).mkString + "\n"

        val missingCombos = allcomb filter (open => rows.forall(!_.covers(open))) map mkMissingStr
        cunit.warning(tvars.head.pos, "match is not exhaustive!\n" + missingCombos.mkString)
        this
      }

      // a fancy toString method for debugging
      override def toString() = {
        val tempStr = (tvars map (t => pad(t.name))).mkString
        val underlines = tempStr.replaceAll("""\S""", "-")
        val rowStr = (
          for (Row(pat, subst, guard, bx) <- rows) yield {
            val extraStr: String = guard.toString + subst
            "%s %s\n".format(pat map pad mkString, extraStr)
          }
        ) mkString

        if (tvars.size == 0) "Rep(%dx%d)".format(tvars.size, rows.size)
        else "Rep(%dx%d)\n%s\n%s\n%s".format(tvars.size, rows.size, tempStr, underlines, rowStr)
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
    final def expand(roots: List[Symbol], cases: List[Tree]): (List[Row], List[FinalState], List[List[Symbol]]) = {
      val res = unzip3(
        for ((CaseDef(pat, guard, body), index) <- cases.zipWithIndex) yield {
          def mkRow(ps: List[Tree]) = Row(ps, NoBinding, Guard(guard), index)

          def rowForPat: Option[Row] = condOpt(pat) {
            case _ if roots.length <= 1 => mkRow(List(pat))
            case Apply(fn, args)        => mkRow(args)
            case WILD()                 => mkRow(getDummies(roots.length))
          }

          (rowForPat, FinalState(NoBinding, body), definedVars(pat))
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
