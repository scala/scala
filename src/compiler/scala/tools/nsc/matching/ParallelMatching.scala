/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * Copyright 2007 Google Inc. All Rights Reserved.
 * Author: bqe@google.com (Burak Emir)
 */
// $Id$

package scala.tools.nsc
package matching

import util.Position
import transform.ExplicitOuter
import symtab.Flags
import collection._
import mutable.{ BitSet, HashMap, ListBuffer }
import immutable.IntMap
import MatchUtil._
import annotation.elidable
import Function.tupled

trait ParallelMatching extends ast.TreeDSL
      with Matrix
      with Patterns
      with PatternBindings
      with PatternOptimizer
      with PatternNodes
{
  self: ExplicitOuter =>

  import global.{ typer => _, _ }
  import definitions.{ AnyRefClass, IntClass, getProductArgs, productProj }
  import Types._
  import CODE._

  /** Debugging support: enable with -Ypmat-debug **/
  private final def trace = settings.Ypmatdebug.value

  def ifDebug(body: => Unit): Unit          = { if (settings.debug.value) body }
  def DBG(msg: => String): Unit             = { ifDebug(println(msg)) }

  @elidable(elidable.FINE)
  def TRACE(f: String, xs: Any*): Unit      = { if (trace) println(if (xs.isEmpty) f else f.format(xs : _*)) }

  def logAndReturn[T](s: String, x: T): T   = { log(s + x.toString) ; x }
  def traceAndReturn[T](s: String, x: T): T = { TRACE(s + x.toString) ; x }

  /** Functions in transition - doomed upon completion of patternization. **/
  def isDefaultPattern(t: Tree)   = cond(unbind(t)) { case EmptyTree | WILD() => true }
  def isStar(t: Tree)             = cond(unbind(t)) { case Star(q) => isDefaultPattern(q) }
  def isRightIgnoring(t: Tree)    = cond(unbind(t)) { case ArrayValue(_, xs) if !xs.isEmpty => isStar(xs.last) }

  def getDummies(i: Int): List[Tree] = List.fill(i)(EmptyTree)
  def toPats(xs: List[Tree]): List[Pattern] = xs map Pattern.apply

  /** Back to the regular schedule. **/

  class MatchMatrix(val context: MatrixContext, data: MatrixInit) extends MatchMatrixOptimizer {
    import context._

    val MatrixInit(roots, cases, failTree)  = data
    val ExpandedMatrix(rows, targets)       = expand(roots, cases)
    val expansion: Rep                      = make(roots, rows)

    val shortCuts   = new ListBuffer[Symbol]()

    final def shortCut(theLabel: Symbol): Int = {
      shortCuts += theLabel
      -shortCuts.length
    }

    /** first time bx is requested, a LabelDef is returned. next time, a jump.
     *  the function takes care of binding
     */
    final def requestBody(bx: Int, subst: Bindings): Tree = {
      implicit val ctx = context
      lazy val target @ FinalState(bindings, body, freeVars) = targets(bx)
      lazy val substInfo = subst infoFor freeVars
      import substInfo._

      // shortcut
      if (bx < 0) Apply(ID(shortCuts(-bx-1)), Nil)
      // first time this bx is requested - might be bound elsewhere
      else if (target.isNotReached) target.createLabelBody("body%"+bx, vsyms, vdefs)
      // call label "method" if possible
      else target.getLabelBody(idents, vdefs)
    }

    /** the injection here handles alternatives and unapply type tests */
    final def make(tvars: List[Symbol], row1: List[Row]): Rep = {
      def classifyPat(opat: Pattern, j: Int): Pattern = {
        def testVar = tvars(j)

        // @pre for doUnapplySeq: is not right-ignoring (no star pattern) ; no exhaustivity check
        def doUnapplySeq(tptArg: Tree, xs: List[Tree]) = {
          testVar setFlag Flags.TRANS_FLAG

          opat rebindTo normalizedListPattern(xs, tptArg.tpe)
        }
        def doValMatch(x: Tree, fn: Tree) = {
          val p = Pattern(x)

          def examinePrefix(path: Tree) = (path, path.tpe) match {
            case (_, t: ThisType)     => singleType(t, x.symbol)            // cases 2/3 are e.g. `case Some(p._2)' in s.c.jcl.Map
            case (_: Apply, _)        => PseudoType(x)                      // outer-matching: test/files/pos/t154.scala
            case _                    => singleType(Pattern(path).mkSingleton, x.symbol)  // old
          }
          val singletonType =
            if (p.isModule) p.mkSingleton else fn match {
              case Select(path, _)  => examinePrefix(path)
              case x: Ident         => Pattern(fn).equalsCheck
            }
          val typeToTest = mkEqualsRef(singletonType)

          val t = Typed(WILD(typeToTest), TypeTree(singletonType)) setType typeToTest
          opat rebindTo t
        }

        def doReturnOriginal(t: Tree) = cond(t) {
          case EmptyTree | WILD() | _: Literal | _: Typed | _: ArrayValue   => true
        }

        // NOTE - this seemingly pointless representation has a point.  Until extractors
        // can be trusted, I only feel safe using them by using one to a match, because it is
        // in the transitions they are broken.  This will return to a more traditional
        // pattern match before the final curtain falls.
        val f = List[PartialFunction[Tree, Pattern]](
          { case _: Alternative                       => opat } ,
          { case _: Typed                             => opat simplify testVar } ,
          { case x if doReturnOriginal(x)             => opat } ,
          // This busts things for now
          // { case _: Ident                             => opat.simplify() } ,
          { case _: Ident | _: Select                 => opat.rebindToEqualsCheck() } ,
          { case _: This                              => opat } ,
          { case UnapplySeq(tptArg, xs)               => doUnapplySeq(tptArg, xs) } ,
          { case UnApply(Apply(fn, _), _)             => opat simplify testVar } ,
          { case x @ Apply_Function(fn)               => doValMatch(x, fn) } ,
          { case Apply_Value(_, _)                    => opat simplify testVar } ,
          { case Apply_CaseClass(_, _)                => opat.simplify() } ,
          { case x                                    => abort("Unexpected pattern: " + x.getClass + " => " + x) }
        ) reduceLeft (_ orElse _)

        f(opat.tree)
      }

      val rows = row1 flatMap (_ expandAlternatives classifyPat)
      if (rows.length != row1.length) make(tvars, rows)  // recursive call if any change
      else Rep(tvars, rows).checkExhaustive
    }

    override def toString() = "MatchMatrix(%s)".format(targets)

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
      def isSimple  = tpe.isByte || tpe.isShort || tpe.isChar || tpe.isInt

      // sequences
      def seqType   = tpe.widen baseType SeqClass
      def elemType  = tpe typeArgs 0  // can this happen? if (seqType == NoType) error("...")

      // for propagating "unchecked" to synthetic vars
      def flags: List[Long] = List(Flags.TRANS_FLAG) filter (sym hasFlag _)

      def castedTo(headType: Type) =
        if (tpe =:= headType) this
        else new Scrutinee(newVar(pos, headType, flags = flags))

      override def toString() = "Scrutinee(sym = %s, tpe = %s, id = %s)".format(sym, tpe, id)
    }

    case class Patterns(scrut: Scrutinee, ps: List[Pattern]) {
      private lazy val trees = ps map (_.boundTree)
      lazy val head = ps.head
      lazy val tail = ps.tail
      lazy val size = ps.length

      lazy val headType = head.tree match {
        case p @ (_:Ident | _:Select) => head.mkSingleton // should be singleton object
        case __UnApply(_,argtpe,_)    => argtpe                   // ?? why argtpe?
        case _                        => head.tpe
      }
      def isCaseHead = isCaseClass(headType)
      def dummies = if (isCaseHead) getDummies(headType.typeSymbol.caseFieldAccessors.length) else Nil
      def dummyPatterns = dummies map (x => Pattern(x))

      def apply(i: Int): Pattern = ps(i)
      // XXX temp
      def zip() = trees.zipWithIndex
      def pzip() = ps.zipWithIndex
      def zip[T](others: List[T]) = trees zip others
      def pzip[T](others: List[T]) = ps zip others

      def isObjectTest(pat: Pattern)  = pat isObjectTest headType
      def isObjectTest(pat: Tree)     = Pattern(pat) isObjectTest headType

      def extractSimpleSwitch(): Option[(List[Tree], Option[Tree])] = {
        def isSwitchableTag(tag: Int)     = cond(tag)       { case ByteTag | ShortTag | IntTag | CharTag => true }
        def isSwitchableConst(t: Tree)    = cond(unbind(t)) { case Literal(x: Constant) => isSwitchableTag(x.tag) }
        def isSwitchableDefault(x: Tree)  = isSwitchableConst(x) || isDefaultPattern(x)

        val (lits, others) = trees span isSwitchableConst
        others match {
          case Nil                                => Some(lits, None)
          // TODO: This needs to also allow the case that the last is a compatible type pattern.
          case List(x) if isSwitchableDefault(x)  => Some(lits, Some(x))
          case _                                  => None
        }
      }

      // Any unapply - returns Some(true) if a type test is needed before the unapply can
      // be called (e.g. def unapply(x: Foo) = { ... } but our scrutinee is type Any.)
      object AnyUnapply {
        def unapply(x: Tree): Option[Boolean] = condOpt(x) { case __UnApply(_,tpe,_) => !(scrut.tpe <:< tpe) }
      }

      object SimpleSwitch {
        // TODO - scala> (5: Any) match { case 5 => 5 ; case 6 => 7 }
        // ... should compile to a switch.  It doesn't because the scrut isn't Int/Char, but
        // that could be handle in an if/else since every pattern requires an Int.
        // More immediately, Byte and Short scruts should also work.
        def unapply(x: Patterns) = if (x.scrut.isSimple) x.extractSimpleSwitch else None
      }

      def mkRule(rest: Rep): RuleApplication = {
        logAndReturn("mkRule: ", head.boundTree match {
            case x if isEquals(x.tpe)                 => new MixEquals(this, rest)
            case x: ArrayValue if isRightIgnoring(x)  => new MixSequenceStar(this, rest)
            case x: ArrayValue                        => new MixSequence(this, rest)
            case AnyUnapply(false)                    => new MixUnapply(this, rest, false)
            case _ => this match {
              case SimpleSwitch(lits, d)              => new MixLiteralInts(this, rest, lits, d)
              case _                                  => new MixTypes(this, rest)
            }
          }
        )
      }
    } // Patterns

    /** picks which rewrite rule to apply
     *  @precondition: column does not contain alternatives
     */
    def MixtureRule(scrut: Scrutinee, column: List[Pattern], rest: Rep): RuleApplication =
      Patterns(scrut, column) mkRule rest

    /**
     * Class encapsulating a guard expression in a pattern match:
     *   case ... if(tree) => ...
     */
    case class Guard(tree: Tree) {
      def isEmpty   = tree.isEmpty
      def duplicate = Guard(tree.duplicate)
      override def toString() = if (isEmpty) "" else " // if %s" format tree
    }
    val NoGuard = Guard(EmptyTree)

    /***** Rule Applications *****/

    sealed abstract class RuleApplication {
      def pats: Patterns
      def rest: Rep
      lazy val Patterns(scrut, patterns) = pats
      lazy val head = pats.head
      private def sym = scrut.sym

      /** Creates Some(fail rule) even if xs == Nil. */
      def mkFail(xs: List[Row]): Option[Rep] = Some(make(sym :: rest.tvars, xs))

      /** Returns None if xs == Nil, Some(fail rule) otherwise. */
      def mkFailOpt(xs: List[Row]): Option[Rep] = if (xs.isEmpty) None else mkFail(xs)

      /** Splices scrutinee's symbol in between the given lists */
      def mkNewRep(pre: List[Symbol], post: List[Symbol], rows: List[Row]) =
        make(pre ::: sym :: post, rows)

      /** translate outcome of the rule application into code (possible involving recursive application of rewriting) */
      def tree(): Tree

      override def toString = {
        "RuleApplication/%s (%s: %s) { %s ... }".format(
          getClass(), scrut, scrut.tpe, head
        )
      }
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
            IF (scrut.id MEMBER_== lit) THEN body ELSE defaultTree
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
    class MixUnapply(val pats: Patterns, val rest: Rep, typeTest: Boolean) extends RuleApplication {
      // Note: trailingArgs is not necessarily Nil, because unapply can take implicit parameters.
      lazy val ua @ UnApply(app, args) = head.tree
      lazy val Apply(fxn, _ :: trailingArgs) = app

      object sameUnapplyCall {
        private def sameFunction(fn1: Tree) = fxn.symbol == fn1.symbol && (fxn equalsStructure fn1)
        def unapply(t: Tree) = condOpt(t) { case UnApply(Apply(fn1, _), args) if sameFunction(fn1)  => args }
      }

      def newVarCapture(pos: Position, tpe: Type) =
        newVar(pos, tpe, flags = scrut.flags)

    /** returns (un)apply-call, success-rep, optional fail-rep */
      final def getTransition(): Branch[UnapplyCall] = {
        val unapplyRes  = newVarCapture(ua.pos, app.tpe)
        val rhs         = Apply(fxn, scrut.id :: trailingArgs) setType unapplyRes.tpe
        val zipped      = pats zip rest.rows
        val nrowsOther  = zipped.tail flatMap {
          case (Stripped(sameUnapplyCall(_)), _)  => Nil
          case (pat, r)                           => List(r insert Pattern(pat))
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
            case sameUnapplyCall(args)  => r.insert2(toPats(sameFilter(args)) ::: List(NoPattern), vs, scrut.sym)
            case _                      => r insert (emptyPatterns(dum) ::: List(Pattern(pat)))
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

      final def removeStar(xs: List[Pattern]): List[Pattern] =
        xs.init ::: List(Pattern(makeBind(xs.last.boundVariables, WILD(scrut.seqType))))

      protected def getSubPatterns(len: Int, x: Tree): Option[List[Pattern]] = condOpt(x) {
        case av @ ArrayValue(_,xs) if !isRightIgnoring(av) && xs.length == len   => toPats(xs) ::: List(NoPattern)
        case av @ ArrayValue(_,xs) if  isRightIgnoring(av) && xs.length == len+1 => removeStar(toPats(xs)) // (*)
        case EmptyTree | WILD()                                                  => emptyPatterns(len + 1)
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
            val (len1, len2)    = (elemLength(av), elemLength(bv))
            val (star1, star2)  = (isRightIgnoring(av), isRightIgnoring(bv))

            // this still needs rewriting.
            ( star1 &&  star2 && len2  < len1                     ) ||  // Seq(a,b,c,_*) followed by Seq(a,b,_*) because of (a,b)
            ( star1 && !star2 && len2  < len1 && isAllDefaults(av)) ||  // Seq(a,b,c,_*) followed by Seq(a,b) because of (a,b)
            (!star1 &&  star2                                     ) ||
            (!star1 && !star2 && len2 >= len1                     )
        })

      case class TransitionContext(f: TreeFunction2)

      // context (to be used in IF), success and failure Rep
      def getTransition(): Branch[TransitionContext] = {
        assert(scrut.tpe <:< head.tpe, "fatal: %s is not <:< %s".format(scrut, head.tpe))

        val av @ ArrayValue(_, elems)   = head.boundTree
        val ys                          = if (isRightIgnoring(av)) elems.init else elems
        val vs                          = ys map (y => newVar(unbind(y).pos, scrut.elemType))
        def scrutCopy                   = scrut.id.duplicate

        lazy val tail                   = newVar(scrut.pos, scrut.seqType)
        lazy val lastBinding            = typedValDef(tail, scrutCopy DROP ys.size)
        def elemAt(i: Int)              = typer typed ((scrutCopy DOT (scrutCopy.tpe member nme.apply))(LIT(i)))

        val bindings =
          (vs.zipWithIndex map tupled((v, i) => typedValDef(v, elemAt(i)))) ::: List(lastBinding)

        val (nrows, frows): (List[Option[Row]], List[Option[Row]]) = List.unzip(
          for ((c, rows) <- pats zip rest.rows) yield getSubPatterns(ys.size, c) match {
            case Some(ps) => (Some(rows insert ps), if (mustCheck(av, c)) Some(rows insert Pattern(c)) else None)
            case None     => (None, Some(rows insert Pattern(c)))
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
        typer typed nullSafe(cmpFunction _, FALSE)(tree)
      }

      // precondition for matching: sequence is exactly length of arg
      protected def getPrecondition(tree: Tree, lengthArg: Int) =
        lengthCheck(tree, lengthArg, _ MEMBER_== _)

      final def tree() = {
        val Branch(TransitionContext(transition), succ, Some(fail)) = this.getTransition
        transition(succ.toTree, fail.toTree)
      }
    }

    /** handle sequence pattern and ArrayValue with star patterns
     */
    final class MixSequenceStar(pats: Patterns, rest: Rep) extends MixSequence(pats, rest) {
      // in principle, we could optimize more, but variable binding gets complicated (@todo use finite state methods instead)
      override def getSubPatterns(minlen: Int, x: Tree): Option[List[Pattern]] = condOpt(x) {
        case av @ ArrayValue(_,xs) if (!isRightIgnoring(av) && xs.length   == minlen) =>  // Seq(p1,...,pN)
          toPats(xs ::: List(gen.mkNil, EmptyTree))
        case av @ ArrayValue(_,xs) if ( isRightIgnoring(av) && xs.length-1 == minlen) =>  // Seq(p1,...,pN,_*)
          removeStar(toPats(xs)) ::: List(NoPattern)
        case av @ ArrayValue(_,xs) if ( isRightIgnoring(av) && xs.length-1  < minlen) =>  // Seq(p1..,pJ,_*)   J < N
          emptyPatterns(minlen + 1) ::: List(Pattern(x))
        case EmptyTree | WILD()                   =>
          emptyPatterns(minlen + 1          + 1)
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
          rest.rows.head.insert2(List(NoPattern), head.boundVariables, scrut.sym),
          Row(emptyPatterns(1 + rest.tvars.length), NoBinding, NoGuard, shortCut(label))
        )

        // todo: optimize if no guard, and no further tests
        val fail    = mkFail(List.map2(rest.rows.tail, pats.tail)(_ insert _))
        val action  = typer typed (scrut.id MEMBER_== value)

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

    /** mixture rule for type tests
    **/
    class MixTypes(val pats: Patterns, val rest: Rep) extends RuleApplication {
      private def subpatterns(p: Pattern): List[Pattern] =
        p.tree match {
          case app @ Apply(fn, ps) if isCaseClass(app.tpe) && fn.isType   => if (pats.isCaseHead) toPats(ps) else pats.dummyPatterns
          case Apply(fn, xs) if !xs.isEmpty || fn.isType                  => abort("strange Apply")
          case _                                                          => pats.dummyPatterns
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
          lazy val subs                 = (j, subpatterns(Pattern(pat)) map (_.boundTree))

          lazy val cmpOld: TypeComp  = spat.tpe cmp pats.headType  // contains type info about pattern's type vs. head pattern
          import cmpOld.{ erased }

          def erased_xIsaY = erased.xIsaY
          def erased_yIsaX = erased.yIsaX

          // scrutinee, pattern
          val (s, p)  = (decodedEqualsType(spat.tpe), decodedEqualsType(pats.headType))
          def xIsaY   = s <:< p
          def yIsaX   = p <:< s

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
            case _ if erased.xIsaY || xIsaY && !isDef         => (alts(EmptyTree, pat), subs, None) // never =:= for <equals>
            case _ if erased.yIsaX || yIsaX || isDef          => (EmptyTree, dummy, pass)     // subsuming (matched *and* remaining pattern)
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
        val casted = scrut castedTo pats.headType
        val isAnyMoreSpecific = moreSpecific exists (x => !x.isEmpty)

        def mkZipped    = moreSpecific zip subsumed map { case (mspat, (j, pats)) => (j, mspat :: pats) }
        def mkAccessors = casted.accessors map (m => newVar(scrut.pos, (casted.tpe memberType m).resultType, scrut.flags))

        val (subtests, subtestVars) =
          if (isAnyMoreSpecific)  (mkZipped, List(casted.sym))
          else                    (subsumed, Nil)

        val accessorVars = if (pats.isCaseHead) mkAccessors else Nil
        val newRows =
          for ((j, ps) <- subtests) yield
            (rest rows j).insert2(toPats(ps), pats(j).boundVariables, casted.sym)

        Branch(
          casted,
          // succeeding => transition to translate(subsumed) (taking into account more specific)
          make(subtestVars ::: accessorVars ::: rest.tvars, newRows),
          // fails      => transition to translate(remaining)
          mkFailOpt(remaining map tupled((p1, p2) => rest rows p1 insert Pattern(p2)))
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

  /*** States, Rows, Etc. ***/

    case class Row(pat: List[Pattern], subst: Bindings, guard: Guard, bx: Int) {
      def insert(h: Pattern)              = copy(pat = h :: pat)
      def insert(hs: List[Pattern])       = copy(pat = hs ::: pat)    // prepends supplied tree
      def replace(hs: List[Pattern])      = copy(pat = hs)            // substitutes for patterns
      def rebind(b: Bindings)             = copy(subst = b)           // substitutes for bindings

      def rebind2(vs: Iterable[Symbol], tvars: Symbol) =
        copy(subst = subst.add(vs, tvars))

      def insert2(hs: List[Pattern], vs: Iterable[Symbol], tvars: Symbol) =             // prepends and prepends
        copy(pat = hs ::: pat, subst = subst.add(vs, tvars))

      /** returns true if the patterns in this rows cover a type symbols "combination" and there is no guard
       *  @param comb pairs of (column index, type symbol)
       */
      def covers(combos: List[Combo]) =
        guard.isEmpty && (combos forall (c => c isCovered pat(c.index)))

      // returns this rows with alternatives expanded
      def expandAlternatives(classifyPat: (Pattern, Int) => Pattern): List[Row] = {
        // classify all the top level patterns - alternatives come back unaltered
        val newPats: List[Pattern] = List.map2(pat, pat.indices.toList)(classifyPat)
        // expand alternatives if any are present
        (newPats indexWhere (_.isAlternative)) match {
          case -1     => List(replace(newPats))
          case index  =>
            val (prefix, alts :: suffix) = newPats splitAt index
            // make a new row for each alternative, with it spliced into the original position
            extractBindings(alts.boundTree) map (x => replace(prefix ::: Pattern(x) :: suffix))
        }
      }
      override def toString() = {
        val patStr = pat.mkString
        val others = List(subst, guard) map (_.toString) filter (_ != "")
        val otherStr = if (others.isEmpty) "" else " // " + others.mkString(" ")

        "Row(%d) %s%s".format(bx, patStr, otherStr)
      }
    }

    object ExpandedMatrix {
      def unapply(x: ExpandedMatrix) = Some(x.rows, x.targets)
      def apply(rows: List[Row], targets: List[FinalState]) = new ExpandedMatrix(rows, targets)
    }
    class ExpandedMatrix(val rows: List[Row], val targets: List[FinalState])

    abstract class State {
      def bindings: Bindings
      def body: Tree
      def freeVars: List[Symbol]
      def isFinal: Boolean
    }

    case class FinalState(bindings: Bindings, body: Tree, freeVars: List[Symbol]) extends State {
      private var referenceCount = 0
      private var _label: Symbol = null
      def setLabel(s: Symbol): Unit = { _label = s }
      def label = _label

      // arguments to pass to this body%xx
      def labelParamTypes = label.tpe.paramTypes

      def createLabelBody(name: String, args: List[Symbol], vdefs: List[Tree]) = {
        require(_label == null)
        _label = owner.newLabel(body.pos, name) setInfo MethodType(args, tpe)
        referenceCount += 1

        squeezedBlock(vdefs,
          if (isLabellable) LabelDef(label, args, body setType tpe)
          else duplicate
        )
      }

      def getLabelBody(idents: List[Tree], vdefs: List[Tree]): Tree = {
        referenceCount += 1
        if (isLabellable) ID(label) APPLY (idents)
        else squeezedBlock(vdefs, duplicate)
      }

      // @bug: typer is not able to digest a body of type Nothing being assigned result type Unit
      def tpe = if (body.tpe.isNothing) body.tpe else matchResultType
      def duplicate = body.duplicate setType tpe

      def isFinal = true
      def isLabellable = !cond(body)  { case _: Throw | _: Literal => true }
      def isNotReached = referenceCount == 0
      def isReachedOnce = referenceCount == 1
      def isReachedTwice = referenceCount > 1
    }

    case class Combo(index: Int, sym: Symbol) {
      // is this combination covered by the given pattern?
      def isCovered(p: Pattern) = cond(p.tree) {
        case _: UnApply | _: ArrayValue => true
        case x                          => isDefaultPattern(x) || (p.tpe coversSym sym)
      }
    }
    case class SetCombo(index: Int, syms: Set[Symbol]) {}
    case class Branch[T](action: T, succ: Rep, fail: Option[Rep])
    case class UnapplyCall(ua: Tree, args: List[Tree])

    case class Rep(val tvars: List[Symbol], val rows: List[Row]) {
      import Flags._

      /** Converts this to a tree - recursively acquires subreps. */
      final def toTree(): Tree = this.applyRule.tree()

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

      /** Applying the rule will result in one of:
        *
        *   VariableRule - if all patterns are default patterns
        *    MixtureRule - if one or more patterns are not default patterns
        *      ErrorRule - if there are no rows remaining
        */
      final def applyRule(): RuleApplication = {
        def dropIndex[T](xs: List[T], n: Int) = (xs take n) ::: (xs drop (n + 1))

        lazy val Row(pats, subst, guard, index) = rows.head
        lazy val guardedRest        = if (guard.isEmpty) NoRep else make(tvars, rows.tail)
        lazy val (defaults, others) = pats span (_.isDefault)
        lazy val ndIndex            = defaults.size   // index of non-default pattern in pats

        if (rows.isEmpty) ErrorRule()
        else if (others.isEmpty) {
          /** top-most rows contains only variables/wildcards */
          val binding = (defaults map (_.boundVariables) zip tvars) .
            foldLeft(subst)((b, pair) => b.add(pair._1, pair._2))

          VariableRule(binding, guard, guardedRest, index)
        }
        else {
          /** cut out the column (px) containing the non-default pattern. */
          val rpat      = others.head
          val vs        = rpat.boundVariables
          val column    = rpat :: (rows.tail map (_ pat ndIndex))
          val restTemp  = dropIndex(tvars, ndIndex)
          val restRows  = rows map (r => r replace dropIndex(r.pat, ndIndex))

          MixtureRule(new Scrutinee(tvars(ndIndex)), column, make(restTemp, restRows))
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

    val NoRep = Rep(Nil, Nil)
    /** Expands the patterns recursively. */
    final def expand(roots: List[Symbol], cases: List[Tree]): ExpandedMatrix = {
      val (rows, finals) = List.unzip(
        for ((CaseDef(pat, guard, body), index) <- cases.zipWithIndex) yield {
          def mkRow(ps: List[Tree]) = Row(toPats(ps), NoBinding, Guard(guard), index)

          def rowForPat: Option[Row] = condOpt(pat) {
            case _ if roots.length <= 1 => mkRow(List(pat))
            case Apply(fn, args)        => mkRow(args)
            case WILD()                 => mkRow(getDummies(roots.length))
          }
          (rowForPat, FinalState(NoBinding, body, definedVars(pat)))
        }
      )

      new ExpandedMatrix(rows flatMap (x => x), finals)
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
            case v                                              => scrutTree MEMBER_== Literal(v)
          }
        case _: SingletonType if useEqTest                      => REF(tpe.termSymbol) MEMBER_== scrutTree
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
