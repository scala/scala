/* NSC -- new Scala compiler
 *
 * Copyright 2011-2013 LAMP/EPFL
 * @author Adriaan Moors
 */

package scala.tools.nsc.transform.patmat

import scala.collection.mutable.ArrayBuffer
import scala.reflect.internal.util.Statistics
import scala.language.postfixOps
import scala.collection.mutable
import scala.reflect.internal.util.Collections._

// a literal is a (possibly negated) variable
class Lit(val v: Int) extends AnyVal {
  def unary_- : Lit = Lit(-v)

  def variable: Int = Math.abs(v)

  def positive = v >= 0

  override def toString(): String = s"Lit#$v"
}

object Lit {
  def apply(v: Int): Lit = new Lit(v)

  implicit val LitOrdering: Ordering[Lit] = Ordering.by(_.v)
}

/** Solve pattern matcher exhaustivity problem via DPLL.
 */
trait Solving extends Logic {

  import PatternMatchingStats._

  trait CNF extends PropositionalLogic {

    type Clause  = Set[Lit]

    // a clause is a disjunction of distinct literals
    def clause(l: Lit*): Clause = l.toSet

    /** Conjunctive normal form (of a Boolean formula).
     *  A formula in this form is amenable to a SAT solver
     *  (i.e., solver that decides satisfiability of a formula).
     */
    type Cnf = Array[Clause]

    class SymbolMapping(symbols: Set[Sym]) {
      val variableForSymbol: Map[Sym, Int] = {
        symbols.zipWithIndex.map {
          case (sym, i) => sym -> (i + 1)
        }.toMap
      }

      val symForVar: Map[Int, Sym] = variableForSymbol.map(_.swap)

      val relevantVars: Set[Int] = symForVar.keySet.map(math.abs)

      def lit(sym: Sym): Lit = Lit(variableForSymbol(sym))

      def size = symbols.size
    }

    case class Solvable(cnf: Cnf, symbolMapping: SymbolMapping)

    trait CnfBuilder {
      private[this] val buff = ArrayBuffer[Clause]()

      var literalCount: Int

      /**
       * @return new Tseitin variable
       */
      def newLiteral(): Lit = {
        literalCount += 1
        Lit(literalCount)
      }

      lazy val constTrue: Lit = {
        val constTrue = newLiteral()
        addClauseProcessed(clause(constTrue))
        constTrue
      }

      def constFalse: Lit = -constTrue

      def isConst(l: Lit): Boolean = l == constTrue || l == constFalse

      def addClauseProcessed(clause: Clause) {
        if (clause.nonEmpty) {
          buff += clause
        }
      }

      def buildCnf: Array[Clause] = buff.toArray

    }

    /** Plaisted transformation: used for conversion of a
      * propositional formula into conjunctive normal form (CNF)
      * (input format for SAT solver).
      * A simple conversion into CNF via Shannon expansion would
      * also be possible but it's worst-case complexity is exponential
      * (in the number of variables) and thus even simple problems
      * could become untractable.
      * The Plaisted transformation results in an _equisatisfiable_
      * CNF-formula (it generates auxiliary variables)
      * but runs with linear complexity.
      * The common known Tseitin transformation uses bi-implication,
      * whereas the Plaisted transformation uses implication only, thus
      * the resulting CNF formula has (on average) only half of the clauses
      * of a Tseitin transformation.
      * The Plaisted transformation uses the polarities of sub-expressions
      * to figure out which part of the bi-implication can be omitted.
      * However, if all sub-expressions have positive polarity
      * (e.g., after transformation into negation normal form)
      * then the conversion is rather simple and the pseudo-normalization
      * via NNF increases chances only one side of the bi-implication
      * is needed.
      */
    class TransformToCnf(symbolMapping: SymbolMapping) extends CnfBuilder {

      // new literals start after formula symbols
      var literalCount: Int = symbolMapping.size

      def convertSym(sym: Sym): Lit = symbolMapping.lit(sym)

      def apply(p: Prop): Solvable = {

        def convert(p: Prop): Lit = {
          p match {
            case And(fv)  =>
              and(fv.map(convert))
            case Or(fv)   =>
              or(fv.map(convert))
            case Not(a)   =>
              not(convert(a))
            case sym: Sym =>
              convertSym(sym)
            case True     =>
              constTrue
            case False    =>
              constFalse
            case _: Eq    =>
              throw new MatchError(p)
          }
        }

        def and(bv: Set[Lit]): Lit = {
          if (bv.isEmpty) {
            // this case can actually happen because `removeVarEq` could add no constraints
            constTrue
          } else if (bv.size == 1) {
            bv.head
          } else if (bv.contains(constFalse)) {
            constFalse
          } else {
            // op1 /\ op2 /\ ... /\ opx <==>
            // (o -> op1) /\ (o -> op2) ... (o -> opx) /\ (!op1 \/ !op2 \/... \/ !opx \/ o)
            // (!o \/ op1) /\ (!o \/ op2) ... (!o \/ opx) /\ (!op1 \/ !op2 \/... \/ !opx \/ o)
            val new_bv = bv - constTrue // ignore `True`
            val o = newLiteral() // auxiliary Tseitin variable
            new_bv.map(op => addClauseProcessed(clause(op, -o)))
            o
          }
        }

        def or(bv: Set[Lit]): Lit = {
          if (bv.isEmpty) {
            constFalse
          } else if (bv.size == 1) {
            bv.head
          } else if (bv.contains(constTrue)) {
            constTrue
          } else {
            // op1 \/ op2 \/ ... \/ opx <==>
            // (op1 -> o) /\ (op2 -> o) ... (opx -> o) /\ (op1 \/ op2 \/... \/ opx \/ !o)
            // (!op1 \/ o) /\ (!op2 \/ o) ... (!opx \/ o) /\ (op1 \/ op2 \/... \/ opx \/ !o)
            val new_bv = bv - constFalse // ignore `False`
            val o = newLiteral() // auxiliary Tseitin variable
            addClauseProcessed(new_bv + (-o))
            o
          }
        }

        // no need for auxiliary variable
        def not(a: Lit): Lit = -a

        // add intermediate variable since we want the formula to be SAT!
        addClauseProcessed(clause(convert(p)))

        Solvable(buildCnf, symbolMapping)
      }
    }

    class AlreadyInCNF(symbolMapping: SymbolMapping) {

      object ToLiteral {
        def unapply(f: Prop): Option[Lit] = f match {
          case Not(ToLiteral(lit)) => Some(-lit)
          case sym: Sym            => Some(symbolMapping.lit(sym))
          case _                   => None
        }
      }

      object ToDisjunction {
        def unapply(f: Prop): Option[Array[Clause]] = f match {
          case Or(fv)         =>
            val cl = fv.foldLeft(Option(clause())) {
              case (Some(clause), ToLiteral(lit)) =>
                Some(clause + lit)
              case (_, _)                         =>
                None
            }
            cl.map(Array(_))
          case True           => Some(Array()) // empty, no clauses needed
          case False          => Some(Array(clause())) // empty clause can't be satisfied
          case ToLiteral(lit) => Some(Array(clause(lit)))
          case _              => None
        }
      }

      /**
       * Checks if propositional formula is already in CNF
       */
      object ToCnf {
        def unapply(f: Prop): Option[Solvable] = f match {
          case ToDisjunction(clauses) => Some(Solvable(clauses, symbolMapping) )
          case And(fv)                =>
            val clauses = fv.foldLeft(Option(mutable.ArrayBuffer[Clause]())) {
              case (Some(cnf), ToDisjunction(clauses)) =>
                Some(cnf ++= clauses)
              case (_, _)                              =>
                None
            }
            clauses.map(c => Solvable(c.toArray, symbolMapping))
          case _                      => None
        }
      }
    }

    def eqFreePropToSolvable(p: Prop): Solvable = {

      // collect all variables since after simplification / CNF conversion
      // they could have been removed from the formula
      val symbolMapping = new SymbolMapping(gatherSymbols(p))

      val simplified = simplify(p)
      val cnfExtractor = new AlreadyInCNF(symbolMapping)
      simplified match {
        case cnfExtractor.ToCnf(solvable) =>
          // this is needed because t6942 would generate too many clauses with Tseitin
          // already in CNF, just add clauses
          solvable
        case p                           =>
          new TransformToCnf(symbolMapping).apply(p)
      }
    }
  }

  // simple solver using DPLL
  trait Solver extends CNF {
    import scala.collection.mutable.ArrayBuffer

    def cnfString(f: Array[Clause]): String = {
      val lits: Array[List[String]] = f map (_.map(_.toString).toList)
      val xss: List[List[String]] = lits toList
      val aligned: String = alignAcrossRows(xss, "\\/", " /\\\n")
      aligned
    }

    // adapted from http://lara.epfl.ch/w/sav10:simple_sat_solver (original by Hossein Hojjat)

    // empty set of clauses is trivially satisfied
    val EmptyModel = Map.empty[Sym, Boolean]

    // no model: originates from the encounter of an empty clause, i.e.,
    // happens if all variables have been assigned in a way that makes the corresponding literals false
    // thus there is no possibility to satisfy that clause, so the whole formula is UNSAT
    val NoModel: Model = null

    // this model contains the auxiliary variables as well
    type TseitinModel = Set[Lit]
    val EmptyTseitinModel = Set.empty[Lit]
    val NoTseitinModel: TseitinModel = null

    // returns all solutions, if any (TODO: better infinite recursion backstop -- detect fixpoint??)
    def findAllModelsFor(solvable: Solvable): List[Solution] = {
      debug.patmat("find all models for\n"+ cnfString(solvable.cnf))

      // we must take all vars from non simplified formula
      // otherwise if we get `T` as formula, we don't expand the variables
      // that are not in the formula...
      val relevantVars: Set[Int] = solvable.symbolMapping.relevantVars

      // debug.patmat("vars "+ vars)
      // the negation of a model -(S1=True/False /\ ... /\ SN=True/False) = clause(S1=False/True, ...., SN=False/True)
      // (i.e. the blocking clause - used for ALL-SAT)
      def negateModel(m: TseitinModel) = {
        // filter out auxiliary Tseitin variables
        val relevantLits = m.filter(l => relevantVars.contains(l.variable))
        relevantLits.map(lit => -lit)
      }

      final case class TseitinSolution(model: TseitinModel, unassigned: List[Int]) {
        def projectToSolution(symForVar: Map[Int, Sym]) = Solution(projectToModel(model, symForVar), unassigned map symForVar)
      }
      def findAllModels(clauses: Array[Clause],
                        models: List[TseitinSolution],
                        recursionDepthAllowed: Int = global.settings.YpatmatExhaustdepth.value): List[TseitinSolution]=
        if (recursionDepthAllowed == 0) {
          val maxDPLLdepth = global.settings.YpatmatExhaustdepth.value
          reportWarning("(Exhaustivity analysis reached max recursion depth, not all missing cases are reported. " +
              s"Please try with scalac -Ypatmat-exhaust-depth ${maxDPLLdepth * 2} or -Ypatmat-exhaust-depth off.)")
          models
        } else {
          debug.patmat("find all models for\n" + cnfString(clauses))
          val model = findTseitinModelFor(clauses)
          // if we found a solution, conjunct the formula with the model's negation and recurse
          if (model ne NoTseitinModel) {
            // note that we should not expand the auxiliary variables (from Tseitin transformation)
            // since they are existentially quantified in the final solution
            val unassigned: List[Int] = (relevantVars -- model.map(lit => lit.variable)).toList
            debug.patmat("unassigned "+ unassigned +" in "+ model)

            val solution = TseitinSolution(model, unassigned)
            val negated = negateModel(model)
            findAllModels(clauses :+ negated, solution :: models, recursionDepthAllowed - 1)
          }
          else models
        }

      val tseitinSolutions = findAllModels(solvable.cnf, Nil)
      tseitinSolutions.map(_.projectToSolution(solvable.symbolMapping.symForVar))
    }

    private def withLit(res: TseitinModel, l: Lit): TseitinModel = {
      if (res eq NoTseitinModel) NoTseitinModel else res + l
    }

    /** Drop trivially true clauses, simplify others by dropping negation of `unitLit`.
     *
     *  Disjunctions that contain the literal we're making true in the returned model are trivially true.
     *  Clauses can be simplified by dropping the negation of the literal we're making true
     *  (since False \/ X == X)
     */
    private def dropUnit(clauses: Array[Clause], unitLit: Lit): Array[Clause] = {
      val negated = -unitLit
      val simplified = new ArrayBuffer[Clause](clauses.size)
      clauses foreach {
        case trivial if trivial contains unitLit => // drop
        case clause                              => simplified += clause - negated
      }
      simplified.toArray
    }

    def findModelFor(solvable: Solvable): Model = {
      projectToModel(findTseitinModelFor(solvable.cnf), solvable.symbolMapping.symForVar)
    }

    def findTseitinModelFor(clauses: Array[Clause]): TseitinModel = {
      @inline def orElse(a: TseitinModel, b: => TseitinModel) = if (a ne NoTseitinModel) a else b

      debug.patmat(s"DPLL\n${cnfString(clauses)}")

      val start = if (Statistics.canEnable) Statistics.startTimer(patmatAnaDPLL) else null

      val satisfiableWithModel: TseitinModel =
        if (clauses isEmpty) EmptyTseitinModel
        else if (clauses exists (_.isEmpty)) NoTseitinModel
        else clauses.find(_.size == 1) match {
          case Some(unitClause) =>
            val unitLit = unitClause.head
            withLit(findTseitinModelFor(dropUnit(clauses, unitLit)), unitLit)
          case _ =>
            // partition symbols according to whether they appear in positive and/or negative literals
            val pos = new mutable.HashSet[Int]()
            val neg = new mutable.HashSet[Int]()
            mforeach(clauses)(lit => if (lit.positive) pos += lit.variable else neg += lit.variable)

            // appearing in both positive and negative
            val impures = pos intersect neg
            // appearing only in either positive/negative positions
            val pures = (pos ++ neg) -- impures

            if (pures nonEmpty) {
              val pureVar = pures.head
              // turn it back into a literal
              // (since equality on literals is in terms of equality
              //  of the underlying symbol and its positivity, simply construct a new Lit)
              val pureLit = Lit(if (neg(pureVar)) -pureVar else pureVar)
              // debug.patmat("pure: "+ pureLit +" pures: "+ pures +" impures: "+ impures)
              val simplified = clauses.filterNot(_.contains(pureLit))
              withLit(findTseitinModelFor(simplified), pureLit)
            } else {
              val split = clauses.head.head
              // debug.patmat("split: "+ split)
              orElse(findTseitinModelFor(clauses :+ clause(split)), findTseitinModelFor(clauses :+ clause(-split)))
            }
        }

      if (Statistics.canEnable) Statistics.stopTimer(patmatAnaDPLL, start)
      satisfiableWithModel
    }

    private def projectToModel(model: TseitinModel, symForVar: Map[Int, Sym]): Model =
      if (model == NoTseitinModel) NoModel
      else if (model == EmptyTseitinModel) EmptyModel
      else {
        val mappedModels = model.toList collect {
          case lit if symForVar isDefinedAt lit.variable => (symForVar(lit.variable), lit.positive)
        }
        if (mappedModels.isEmpty) {
          // could get an empty model if mappedModels is a constant like `True`
          EmptyModel
        } else {
          mappedModels.toMap
        }
      }
  }
}
