/* NSC -- new Scala compiler
 *
 * Copyright 2011-2013 LAMP/EPFL
 * @author Adriaan Moors
 */

package scala.tools.nsc.transform.patmat

import scala.tools.nsc.symtab._
import scala.language.postfixOps
import scala.collection.mutable
import scala.reflect.internal.util.Statistics
import scala.reflect.internal.util.Position
import scala.reflect.internal.util.HashSet


trait Logic extends Debugging  {
  import PatternMatchingStats._

  private def max(xs: Seq[Int]) = if (xs isEmpty) 0 else xs max
  private def alignedColumns(cols: Seq[AnyRef]): Seq[String] = {
    def toString(x: AnyRef) = if (x eq null) "" else x.toString
    if (cols.isEmpty || cols.tails.isEmpty) cols map toString
    else {
      val colLens = cols map (c => toString(c).length)
      val maxLen = max(colLens)
      val avgLen = colLens.sum/colLens.length
      val goalLen = maxLen min avgLen*2
      def pad(s: String) = {
        val toAdd = ((goalLen - s.length) max 0) + 2
        (" " * (toAdd/2)) + s + (" " * (toAdd/2 + (toAdd%2)))
      }
      cols map (x => pad(toString(x)))
    }
  }

  def alignAcrossRows(xss: List[List[AnyRef]], sep: String, lineSep: String = "\n"): String = {
    val maxLen = max(xss map (_.length))
    val padded = xss map (xs => xs ++ List.fill(maxLen - xs.length)(null))
    padded.transpose.map(alignedColumns).transpose map (_.mkString(sep)) mkString(lineSep)
  }

  // http://www.cis.upenn.edu/~cis510/tcl/chap3.pdf
  // http://users.encs.concordia.ca/~ta_ahmed/ms_thesis.pdf
  // propositional logic with constants and equality
  trait PropositionalLogic {
    type Type
    type Tree

    class Prop
    case class Eq(p: Var, q: Const) extends Prop

    type Const

    type TypeConst <: Const
    def TypeConst: TypeConstExtractor
    trait TypeConstExtractor {
      def apply(tp: Type): Const
    }

    type ValueConst <: Const
    def ValueConst: ValueConstExtractor
    trait ValueConstExtractor {
      def apply(p: Tree): Const
    }

    val NullConst: Const

    type Var <: AbsVar
    val Var: VarExtractor
    trait VarExtractor {
      def apply(x: Tree): Var
      def unapply(v: Var): Some[Tree]
    }

    // resets hash consing -- only supposed to be called by TreeMakersToProps
    def prepareNewAnalysis(): Unit

    trait AbsVar {
      // indicate we may later require a prop for V = C
      def registerEquality(c: Const): Unit

      // call this to indicate null is part of the domain
      def registerNull(): Unit

      // can this variable be null?
      def mayBeNull: Boolean

      // compute the domain and return it (call registerNull first!)
      def domainSyms: Option[Set[Sym]]

      // the symbol for this variable being equal to its statically known type
      // (only available if registerEquality has been called for that type before)
      def symForStaticTp: Option[Sym]

      // for this var, call it V, turn V = C into the equivalent proposition in boolean logic
      // registerEquality(c) must have been called prior to this call
      // in fact, all equalities relevant to this variable must have been registered
      def propForEqualsTo(c: Const): Prop

      // populated by registerEquality
      // once implications has been called, must not call registerEquality anymore
      def implications: List[(Sym, List[Sym], List[Sym])]
    }

    // would be nice to statically check whether a prop is equational or pure,
    // but that requires typing relations like And(x: Tx, y: Ty) : (if(Tx == PureProp && Ty == PureProp) PureProp else Prop)
    case class And(a: Prop, b: Prop) extends Prop
    case class Or(a: Prop, b: Prop) extends Prop
    case class Not(a: Prop) extends Prop

    case object True extends Prop
    case object False extends Prop

    // symbols are propositions
    abstract case class Sym(variable: Var, const: Const) extends Prop {
      private[this] val id = Sym.nextSymId

      override def toString = variable +"="+ const +"#"+ id
    }
    class UniqueSym(variable: Var, const: Const) extends Sym(variable, const)
    object Sym {
      private val uniques: HashSet[Sym] = new HashSet("uniques", 512)
      def apply(variable: Var, const: Const): Sym = {
        val newSym = new UniqueSym(variable, const)
        (uniques findEntryOrUpdate newSym)
      }
      private def nextSymId = {_symId += 1; _symId}; private var _symId = 0
    }

    def /\(props: Iterable[Prop]) = if (props.isEmpty) True else props.reduceLeft(And(_, _))
    def \/(props: Iterable[Prop]) = if (props.isEmpty) False else props.reduceLeft(Or(_, _))

    trait PropTraverser {
      def apply(x: Prop): Unit = x match {
        case And(a, b) => apply(a); apply(b)
        case Or(a, b) => apply(a); apply(b)
        case Not(a) => apply(a)
        case Eq(a, b) => applyVar(a); applyConst(b)
        case _ =>
      }
      def applyVar(x: Var): Unit = {}
      def applyConst(x: Const): Unit = {}
    }

    def gatherVariables(p: Prop): Set[Var] = {
      val vars = new mutable.HashSet[Var]()
      (new PropTraverser {
        override def applyVar(v: Var) = vars += v
      })(p)
      vars.toSet
    }

    trait PropMap {
      def apply(x: Prop): Prop = x match { // TODO: mapConserve
        case And(a, b) => And(apply(a), apply(b))
        case Or(a, b) => Or(apply(a), apply(b))
        case Not(a) => Not(apply(a))
        case p => p
      }
    }

    // to govern how much time we spend analyzing matches for unreachability/exhaustivity
    object AnalysisBudget {
      import scala.tools.cmd.FromString.IntFromString
      val max = sys.props.get("scalac.patmat.analysisBudget").collect(IntFromString.orElse{case "off" => Integer.MAX_VALUE}).getOrElse(256)

      abstract class Exception(val advice: String) extends RuntimeException("CNF budget exceeded")

      object exceeded extends Exception(
          s"(The analysis required more space than allowed. Please try with scalac -Dscalac.patmat.analysisBudget=${AnalysisBudget.max*2} or -Dscalac.patmat.analysisBudget=off.)")

    }

    // convert finite domain propositional logic with subtyping to pure boolean propositional logic
    // a type test or a value equality test are modelled as a variable being equal to some constant
    // a variable V may be assigned multiple constants, as long as they do not contradict each other
    // according to subtyping, e.g., V = ConstantType(1) and V = Int are valid assignments
    // we rewrite V = C to a fresh boolean symbol, and model what we know about the variable's domain
    // in a prelude (the equality axioms)
    //   1. a variable with a closed domain (of a sealed type) must be assigned one of the instantiatable types in its domain
    //   2. for each variable V in props, and each constant C it is compared to,
    //      compute which assignments imply each other (as in the example above: V = 1 implies V = Int)
    //      and which assignments are mutually exclusive (V = String implies -(V = Int))
    //
    // note that this is a conservative approximation: V = Constant(A) and V = Constant(B)
    // are considered mutually exclusive (and thus both cases are considered reachable in {case A => case B =>}),
    // even though A may be equal to B   (and thus the second case is not "dynamically reachable")
    //
    // TODO: for V1 representing x1 and V2 standing for x1.head, encode that
    //       V1 = Nil implies -(V2 = Ci) for all Ci in V2's domain (i.e., it is unassignable)
    // may throw an AnalysisBudget.Exception
    def removeVarEq(props: List[Prop], modelNull: Boolean = false): (Formula, List[Formula]) = {
      val start = if (Statistics.canEnable) Statistics.startTimer(patmatAnaVarEq) else null

      val vars = new scala.collection.mutable.HashSet[Var]

      object gatherEqualities extends PropTraverser {
        override def apply(p: Prop) = p match {
          case Eq(v, c) =>
            vars += v
            v.registerEquality(c)
          case _ => super.apply(p)
        }
      }

      object rewriteEqualsToProp extends PropMap {
        override def apply(p: Prop) = p match {
          case Eq(v, c) => v.propForEqualsTo(c)
          case _ => super.apply(p)
        }
      }

      props foreach gatherEqualities.apply
      if (modelNull) vars foreach (_.registerNull())

      val pure = props map (p => eqFreePropToSolvable(rewriteEqualsToProp(p)))

      val eqAxioms = formulaBuilder
      @inline def addAxiom(p: Prop) = addFormula(eqAxioms, eqFreePropToSolvable(p))

      debug.patmat("removeVarEq vars: "+ vars)
      vars.foreach { v =>
        // if v.domainSyms.isEmpty, we must consider the domain to be infinite
        // otherwise, since the domain fully partitions the type of the value,
        // exactly one of the types (and whatever it implies, imposed separately) must be chosen
        // consider X ::= A | B | C, and A => B
        // coverage is formulated as: A \/ B \/ C and the implications are
        v.domainSyms foreach { dsyms => addAxiom(\/(dsyms)) }

        // when this variable cannot be null the equality corresponding to the type test `(x: T)`, where T is x's static type,
        // is always true; when the variable may be null we use the implication `(x != null) => (x: T)` for the axiom
        v.symForStaticTp foreach { symForStaticTp =>
          if (v.mayBeNull) addAxiom(Or(v.propForEqualsTo(NullConst), symForStaticTp))
          else addAxiom(symForStaticTp)
        }

        v.implications foreach { case (sym, implied, excluded) =>
          // when sym is true, what must hold...
          implied  foreach (impliedSym  => addAxiom(Or(Not(sym), impliedSym)))
          // ... and what must not?
          excluded foreach (excludedSym => addAxiom(Or(Not(sym), Not(excludedSym))))
        }
      }

      debug.patmat("eqAxioms:\n"+ cnfString(toFormula(eqAxioms)))
      debug.patmat("pure:"+ pure.map(p => cnfString(p)).mkString("\n"))

      if (Statistics.canEnable) Statistics.stopTimer(patmatAnaVarEq, start)

      (toFormula(eqAxioms), pure)
    }


    // an interface that should be suitable for feeding a SAT solver when the time comes
    type Formula
    type FormulaBuilder

    // creates an empty formula builder to which more formulae can be added
    def formulaBuilder: FormulaBuilder

    // val f = formulaBuilder; addFormula(f, f1); ... addFormula(f, fN)
    // toFormula(f) == andFormula(f1, andFormula(..., fN))
    def addFormula(buff: FormulaBuilder, f: Formula): Unit
    def toFormula(buff: FormulaBuilder): Formula

    // the conjunction of formulae `a` and `b`
    def andFormula(a: Formula, b: Formula): Formula

    // equivalent formula to `a`, but simplified in a lightweight way (drop duplicate clauses)
    def simplifyFormula(a: Formula): Formula

    // may throw an AnalysisBudget.Exception
    def propToSolvable(p: Prop): Formula = {
      val (eqAxioms, pure :: Nil) = removeVarEq(List(p), modelNull = false)
      andFormula(eqAxioms, pure)
    }

    // may throw an AnalysisBudget.Exception
    def eqFreePropToSolvable(p: Prop): Formula
    def cnfString(f: Formula): String

    type Model = Map[Sym, Boolean]
    val EmptyModel: Model
    val NoModel: Model

    def findModelFor(f: Formula): Model
    def findAllModelsFor(f: Formula): List[Model]
  }
}

trait ScalaLogic extends Interface with Logic with TreeAndTypeAnalysis {
  trait TreesAndTypesDomain extends PropositionalLogic with CheckableTreeAndTypeAnalysis {
    type Type = global.Type
    type Tree = global.Tree

    // resets hash consing -- only supposed to be called by TreeMakersToProps
    def prepareNewAnalysis(): Unit = { Var.resetUniques(); Const.resetUniques() }

    object Var extends VarExtractor {
      private var _nextId = 0
      def nextId = {_nextId += 1; _nextId}

      def resetUniques() = {_nextId = 0; uniques.clear()}
      private val uniques = new mutable.HashMap[Tree, Var]
      def apply(x: Tree): Var = uniques getOrElseUpdate(x, new Var(x, x.tpe))
      def unapply(v: Var) = Some(v.path)
    }
    class Var(val path: Tree, staticTp: Type) extends AbsVar {
      private[this] val id: Int = Var.nextId

      // private[this] var canModify: Option[Array[StackTraceElement]] = None
      private[this] def ensureCanModify() = {} //if (canModify.nonEmpty) debug.patmat("BUG!"+ this +" modified after having been observed: "+ canModify.get.mkString("\n"))

      private[this] def observed() = {} //canModify = Some(Thread.currentThread.getStackTrace)

      // don't access until all potential equalities have been registered using registerEquality
      private[this] val symForEqualsTo = new mutable.HashMap[Const, Sym]

      // when looking at the domain, we only care about types we can check at run time
      val staticTpCheckable: Type = checkableType(staticTp)

      private[this] var _mayBeNull = false
      def registerNull(): Unit = { ensureCanModify(); if (NullTp <:< staticTpCheckable) _mayBeNull = true }
      def mayBeNull: Boolean = _mayBeNull

      // case None => domain is unknown,
      // case Some(List(tps: _*)) => domain is exactly tps
      // we enumerate the subtypes of the full type, as that allows us to filter out more types statically,
      // once we go to run-time checks (on Const's), convert them to checkable types
      // TODO: there seems to be bug for singleton domains (variable does not show up in model)
      lazy val domain: Option[Set[Const]] = {
        val subConsts = enumerateSubtypes(staticTp).map{ tps =>
          tps.toSet[Type].map{ tp =>
            val domainC = TypeConst(tp)
            registerEquality(domainC)
            domainC
          }
        }

        val allConsts =
          if (mayBeNull) {
            registerEquality(NullConst)
            subConsts map (_ + NullConst)
          } else
            subConsts

        observed(); allConsts
      }

      // populate equalitySyms
      // don't care about the result, but want only one fresh symbol per distinct constant c
      def registerEquality(c: Const): Unit = {ensureCanModify(); symForEqualsTo getOrElseUpdate(c, Sym(this, c))}

      // return the symbol that represents this variable being equal to the constant `c`, if it exists, otherwise False (for robustness)
      // (registerEquality(c) must have been called prior, either when constructing the domain or from outside)
      def propForEqualsTo(c: Const): Prop = {observed(); symForEqualsTo.getOrElse(c, False)}

      // [implementation NOTE: don't access until all potential equalities have been registered using registerEquality]p
      /** the information needed to construct the boolean proposition that encods the equality proposition (V = C)
       *
       * that models a type test pattern `_: C` or constant pattern `C`, where the type test gives rise to a TypeConst C,
       * and the constant pattern yields a ValueConst C
       *
       * for exhaustivity, we really only need implication (e.g., V = 1 implies that V = 1 /\ V = Int, if both tests occur in the match,
       * and thus in this variable's equality symbols), but reachability also requires us to model things like V = 1 precluding V = "1"
       */
      lazy val implications = {
        /** when we know V = C, which other equalities must hold
         *
         * in general, equality to some type implies equality to its supertypes
         * (this multi-valued kind of equality is necessary for unreachability)
         * note that we use subtyping as a model for implication between instanceof tests
         * i.e., when S <:< T we assume x.isInstanceOf[S] implies x.isInstanceOf[T]
         * unfortunately this is not true in general (see e.g. SI-6022)
         */
        def implies(lower: Const, upper: Const): Boolean =
          // values and null
            lower == upper ||
          // type implication
            (lower != NullConst && !upper.isValue &&
             instanceOfTpImplies(if (lower.isValue) lower.wideTp else lower.tp, upper.tp))

          // if(r) debug.patmat("implies    : "+(lower, lower.tp, upper, upper.tp))
          // else  debug.patmat("NOT implies: "+(lower, upper))


        /** does V = C preclude V having value `other`?
         (1) V = null is an exclusive assignment,
         (2) V = A and V = B, for A and B value constants, are mutually exclusive unless A == B
             we err on the safe side, for example:
               - assume `val X = 1; val Y = 1`, then
                 (2: Int) match { case X => case Y =>  <falsely considered reachable>  }
               - V = 1 does not preclude V = Int, or V = Any, it could be said to preclude V = String, but we don't model that

         (3) for types we could try to do something fancy, but be conservative and just say no
         */
        def excludes(a: Const, b: Const): Boolean =
          a != b && ((a == NullConst || b == NullConst) || (a.isValue && b.isValue))

          // if(r) debug.patmat("excludes    : "+(a, a.tp, b, b.tp))
          // else  debug.patmat("NOT excludes: "+(a, b))

/*
[ HALF BAKED FANCINESS: //!equalitySyms.exists(common => implies(common.const, a) && implies(common.const, b)))
 when type tests are involved, we reason (conservatively) under a closed world assumption,
 since we are really only trying to counter the effects of the symbols that we introduce to model type tests
 we don't aim to model the whole subtyping hierarchy, simply to encode enough about subtyping to do unreachability properly

 consider the following hierarchy:

    trait A
    trait B
    trait C
    trait AB extends B with A

  // two types are mutually exclusive if there is no equality symbol whose constant implies both
  object Test extends App {
    def foo(x: Any) = x match {
      case _ : C  => println("C")
      case _ : AB => println("AB")
      case _ : (A with B) => println("AB'")
      case _ : B  => println("B")
      case _ : A  => println("A")
    }

 of course this kind of reasoning is not true in general,
 but we can safely pretend types are mutually exclusive as long as there are no counter-examples in the match we're analyzing}
*/

        val excludedPair = new mutable.HashSet[ExcludedPair]

        case class ExcludedPair(a: Const, b: Const) {
          override def equals(o: Any) = o match {
            case ExcludedPair(aa, bb) => (a == aa && b == bb) || (a == bb && b == aa)
            case _ => false
          }
          // make ExcludedPair(a, b).hashCode == ExcludedPair(b, a).hashCode
          override def hashCode = a.hashCode ^ b.hashCode
        }

        equalitySyms map { sym =>
          // if we've already excluded the pair at some point (-A \/ -B), then don't exclude the symmetric one (-B \/ -A)
          // (nor the positive implications -B \/ A, or -A \/ B, which would entail the equality axioms falsifying the whole formula)
          val todo = equalitySyms filterNot (b => (b.const == sym.const) || excludedPair(ExcludedPair(b.const, sym.const)))
          val (excluded, notExcluded) = todo partition (b => excludes(sym.const, b.const))
          val implied = notExcluded filter (b => implies(sym.const, b.const))

          debug.patmat("eq axioms for: "+ sym.const)
          debug.patmat("excluded: "+ excluded)
          debug.patmat("implied: "+ implied)

          excluded foreach { excludedSym => excludedPair += ExcludedPair(sym.const, excludedSym.const)}

          (sym, implied, excluded)
        }
      }

      // accessing after calling registerNull will result in inconsistencies
      lazy val domainSyms: Option[Set[Sym]] = domain map { _ map symForEqualsTo }

      lazy val symForStaticTp: Option[Sym]  = symForEqualsTo.get(TypeConst(staticTpCheckable))

      // don't access until all potential equalities have been registered using registerEquality
      private lazy val equalitySyms = {observed(); symForEqualsTo.values.toList}

      // don't call until all equalities have been registered and registerNull has been called (if needed)
      def describe = {
        def domain_s = domain match {
          case Some(d) => d mkString (" ::= ", " | ", "// "+ symForEqualsTo.keys)
          case _       => symForEqualsTo.keys mkString (" ::= ", " | ", " | ...")
        }
        s"$this: ${staticTp}${domain_s} // = $path"
      }
      override def toString = "V"+ id
    }


    import global.{ConstantType, Constant, SingletonType, Literal, Ident, singleType}
    import global.definitions.{AnyClass, UnitClass}


    // all our variables range over types
    // a literal constant becomes ConstantType(Constant(v)) when the type allows it (roughly, anyval + string + null)
    // equality between variables: SingleType(x) (note that pattern variables cannot relate to each other -- it's always patternVar == nonPatternVar)
    object Const {
      def resetUniques() = {_nextTypeId = 0; _nextValueId = 0; uniques.clear() ; trees.clear()}

      private var _nextTypeId = 0
      def nextTypeId = {_nextTypeId += 1; _nextTypeId}

      private var _nextValueId = 0
      def nextValueId = {_nextValueId += 1; _nextValueId}

      private val uniques = new mutable.HashMap[Type, Const]
      private[TreesAndTypesDomain] def unique(tp: Type, mkFresh: => Const): Const =
        uniques.get(tp).getOrElse(
          uniques.find {case (oldTp, oldC) => oldTp =:= tp} match {
            case Some((_, c)) =>
              debug.patmat("unique const: "+ (tp, c))
              c
            case _ =>
              val fresh = mkFresh
              debug.patmat("uniqued const: "+ (tp, fresh))
              uniques(tp) = fresh
              fresh
          })

      private val trees = mutable.HashSet.empty[Tree]

      // hashconsing trees (modulo value-equality)
      private[TreesAndTypesDomain] def uniqueTpForTree(t: Tree): Type =
        // a new type for every unstable symbol -- only stable value are uniqued
        // technically, an unreachable value may change between cases
        // thus, the failure of a case that matches on a mutable value does not exclude the next case succeeding
        // (and thuuuuus, the latter case must be considered reachable)
        if (!t.symbol.isStable) t.tpe.narrow
        else trees find (a => a.correspondsStructure(t)(sameValue)) match {
          case Some(orig) =>
            debug.patmat("unique tp for tree: "+ (orig, orig.tpe))
            orig.tpe
          case _ =>
            // duplicate, don't mutate old tree (TODO: use a map tree -> type instead?)
            val treeWithNarrowedType = t.duplicate setType t.tpe.narrow
            debug.patmat("uniqued: "+ (t, t.tpe, treeWithNarrowedType.tpe))
            trees += treeWithNarrowedType
            treeWithNarrowedType.tpe
        }
    }

    sealed abstract class Const {
      def tp: Type
      def wideTp: Type

      def isAny = wideTp.typeSymbol == AnyClass
      def isValue: Boolean //= tp.isStable

      // note: use reference equality on Const since they're hash-consed (doing type equality all the time is too expensive)
      // the equals inherited from AnyRef does just this
    }

    // find most precise super-type of tp that is a class
    // we skip non-class types (singleton types, abstract types) so that we can
    // correctly compute how types relate in terms of the values they rule out
    // e.g., when we know some value must be of type T, can it still be of type S? (this is the positive formulation of what `excludes` on Const computes)
    // since we're talking values, there must have been a class involved in creating it, so rephrase our types in terms of classes
    // (At least conceptually: `true` is an instance of class `Boolean`)
    private def widenToClass(tp: Type): Type =
      if (tp.typeSymbol.isClass) tp
      else tp.baseType(tp.baseClasses.head)

    object TypeConst extends TypeConstExtractor {
      def apply(tp: Type) = {
        if (tp =:= NullTp) NullConst
        else if (tp.isInstanceOf[SingletonType]) ValueConst.fromType(tp)
        else Const.unique(tp, new TypeConst(tp))
      }
      def unapply(c: TypeConst): Some[Type] = Some(c.tp)
    }

    // corresponds to a type test that does not imply any value-equality (well, except for outer checks, which we don't model yet)
    sealed class TypeConst(val tp: Type) extends Const {
      assert(!(tp =:= NullTp))
      /*private[this] val id: Int = */ Const.nextTypeId

      val wideTp = widenToClass(tp)
      def isValue = false
      override def toString = tp.toString //+"#"+ id
    }

    // p is a unique type or a constant value
    object ValueConst extends ValueConstExtractor {
      def fromType(tp: Type) = {
        assert(tp.isInstanceOf[SingletonType])
        val toString = tp match {
          case ConstantType(c) => c.escapedStringValue
          case _ => tp.toString
        }
        Const.unique(tp, new ValueConst(tp, tp.widen, toString))
      }
      def apply(p: Tree) = {
        val tp = p.tpe.normalize
        if (tp =:= NullTp) NullConst
        else {
          val wideTp = widenToClass(tp)

          val narrowTp =
            if (tp.isInstanceOf[SingletonType]) tp
            else p match {
              case Literal(c) =>
                if (c.tpe.typeSymbol == UnitClass) c.tpe
                else ConstantType(c)
              case Ident(_) if p.symbol.isStable =>
                // for Idents, can encode uniqueness of symbol as uniqueness of the corresponding singleton type
                // for Selects, which are handled by the next case, the prefix of the select varies independently of the symbol (see pos/virtpatmat_unreach_select.scala)
                singleType(tp.prefix, p.symbol)
              case _ =>
                Const.uniqueTpForTree(p)
            }

          val toString =
            if (hasStableSymbol(p)) p.symbol.name.toString // tp.toString
            else p.toString //+"#"+ id

          Const.unique(narrowTp, new ValueConst(narrowTp, checkableType(wideTp), toString)) // must make wide type checkable so that it is comparable to types from TypeConst
        }
      }
    }
    sealed class ValueConst(val tp: Type, val wideTp: Type, override val toString: String) extends Const {
      // debug.patmat("VC"+(tp, wideTp, toString))
      assert(!(tp =:= NullTp)) // TODO: assert(!tp.isStable)
      /*private[this] val id: Int = */Const.nextValueId
      def isValue = true
    }


    lazy val NullTp = ConstantType(Constant(null))
    case object NullConst extends Const {
      def tp     = NullTp
      def wideTp = NullTp

      def isValue = true
      override def toString = "null"
    }
  }
}