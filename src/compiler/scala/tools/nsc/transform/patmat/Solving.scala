/* NSC -- new Scala compiler
 *
 * Copyright 2011-2013 LAMP/EPFL
 * @author Adriaan Moors
 */

package scala.tools.nsc.transform.patmat

import scala.collection.mutable
import scala.reflect.internal.util.Statistics
import scala.language.postfixOps
import scala.reflect.internal.util.Collections._

// naive CNF translation and simple DPLL solver
trait Solving extends Logic {
  import PatternMatchingStats._
  trait CNF extends PropositionalLogic {
    import scala.collection.mutable.ArrayBuffer
    type FormulaBuilder = ArrayBuffer[Clause]
    def formulaBuilder  = ArrayBuffer[Clause]()
    def formulaBuilderSized(init: Int)  = new ArrayBuffer[Clause](init)
    def addFormula(buff: FormulaBuilder, f: Formula): Unit = buff ++= f
    def toFormula(buff: FormulaBuilder): Formula = buff

    // CNF: a formula is a conjunction of clauses
    type Formula = FormulaBuilder
    def formula(c: Clause*): Formula = ArrayBuffer(c: _*)

    type Clause  = Set[Lit]
    // a clause is a disjunction of distinct literals
    def clause(l: Lit*): Clause = l.toSet

    type Lit
    def Lit(sym: Sym, pos: Boolean = true): Lit

    def andFormula(a: Formula, b: Formula): Formula = a ++ b
    def simplifyFormula(a: Formula): Formula = a.distinct

    private def merge(a: Clause, b: Clause) = a ++ b

    // throws an AnalysisBudget.Exception when the prop results in a CNF that's too big
    // TODO: be smarter/more efficient about this (http://lara.epfl.ch/w/sav09:tseitin_s_encoding)
    def eqFreePropToSolvable(p: Prop): Formula = {
      def negationNormalFormNot(p: Prop, budget: Int): Prop =
        if (budget <= 0) throw AnalysisBudget.exceeded
        else p match {
          case And(a, b) =>  Or(negationNormalFormNot(a, budget - 1), negationNormalFormNot(b, budget - 1))
          case Or(a, b)  => And(negationNormalFormNot(a, budget - 1), negationNormalFormNot(b, budget - 1))
          case Not(p)    => negationNormalForm(p, budget - 1)
          case True      => False
          case False     => True
          case s: Sym    => Not(s)
        }

      def negationNormalForm(p: Prop, budget: Int = AnalysisBudget.max): Prop =
        if (budget <= 0) throw AnalysisBudget.exceeded
        else p match {
          case And(a, b)      => And(negationNormalForm(a, budget - 1), negationNormalForm(b, budget - 1))
          case Or(a, b)       =>  Or(negationNormalForm(a, budget - 1), negationNormalForm(b, budget - 1))
          case Not(negated)   => negationNormalFormNot(negated, budget - 1)
          case True
             | False
             | (_ : Sym)      => p
        }

      val TrueF          = formula()
      val FalseF         = formula(clause())
      def lit(s: Sym)    = formula(clause(Lit(s)))
      def negLit(s: Sym) = formula(clause(Lit(s, pos = false)))

      def conjunctiveNormalForm(p: Prop, budget: Int = AnalysisBudget.max): Formula = {
        def distribute(a: Formula, b: Formula, budget: Int): Formula =
          if (budget <= 0) throw AnalysisBudget.exceeded
          else
            (a, b) match {
              // true \/ _ = true
              // _ \/ true = true
              case (trueA, trueB) if trueA.size == 0 || trueB.size == 0 => TrueF
              // lit \/ lit
              case (a, b) if a.size == 1 && b.size == 1 => formula(merge(a(0), b(0)))
              // (c1 /\ ... /\ cn) \/ d = ((c1 \/ d) /\ ... /\ (cn \/ d))
              // d \/ (c1 /\ ... /\ cn) = ((d \/ c1) /\ ... /\ (d \/ cn))
              case (cs, ds) =>
                val (big, small) = if (cs.size > ds.size) (cs, ds) else (ds, cs)
                big flatMap (c => distribute(formula(c), small, budget - (big.size*small.size)))
            }

        if (budget <= 0) throw AnalysisBudget.exceeded

        p match {
          case True        => TrueF
          case False       => FalseF
          case s: Sym      => lit(s)
          case Not(s: Sym) => negLit(s)
          case And(a, b)   =>
            val cnfA = conjunctiveNormalForm(a, budget - 1)
            val cnfB = conjunctiveNormalForm(b, budget - cnfA.size)
            cnfA ++ cnfB
          case Or(a, b)    =>
            val cnfA = conjunctiveNormalForm(a)
            val cnfB = conjunctiveNormalForm(b)
            distribute(cnfA, cnfB, budget - (cnfA.size + cnfB.size))
        }
      }

      val start = if (Statistics.canEnable) Statistics.startTimer(patmatCNF) else null
      val res   = conjunctiveNormalForm(negationNormalForm(p))

      if (Statistics.canEnable) Statistics.stopTimer(patmatCNF, start)

      if (Statistics.canEnable) patmatCNFSizes(res.size).value += 1

//      debug.patmat("cnf for\n"+ p +"\nis:\n"+cnfString(res))
      res
    }
  }

  // simple solver using DPLL
  trait Solver extends CNF {
    // a literal is a (possibly negated) variable
    def Lit(sym: Sym, pos: Boolean = true) = new Lit(sym, pos)
    class Lit(val sym: Sym, val pos: Boolean) {
      override def toString = if (!pos) "-"+ sym.toString else sym.toString
      override def equals(o: Any) = o match {
        case o: Lit => (o.sym eq sym) && (o.pos == pos)
        case _ => false
      }
      override def hashCode = sym.hashCode + pos.hashCode

      def unary_- = Lit(sym, !pos)
    }

    def cnfString(f: Formula) = alignAcrossRows(f map (_.toList) toList, "\\/", " /\\\n")

    // adapted from http://lara.epfl.ch/w/sav10:simple_sat_solver (original by Hossein Hojjat)
    val EmptyModel = Map.empty[Sym, Boolean]
    val NoModel: Model = null

    // returns all solutions, if any (TODO: better infinite recursion backstop -- detect fixpoint??)
    def findAllModelsFor(f: Formula): List[Model] = {

      debug.patmat("find all models for\n"+ cnfString(f))

      val vars: Set[Sym] = f.flatMap(_ collect {case l: Lit => l.sym}).toSet
      // debug.patmat("vars "+ vars)
      // the negation of a model -(S1=True/False /\ ... /\ SN=True/False) = clause(S1=False/True, ...., SN=False/True)
      def negateModel(m: Model) = clause(m.toSeq.map{ case (sym, pos) => Lit(sym, !pos) } : _*)

      /**
       * The DPLL procedure only returns a minimal mapping from literal to value
       * such that the CNF formula is satisfied.
       * E.g. for:
       * `(a \/ b)`
       * The DPLL procedure will find either {a = true} or {b = true}
       * as solution.
       *
       * The expansion step will amend both solutions with the unassigned variable
       * i.e., {a = true} will be expanded to {a = true, b = true} and {a = true, b = false}.
       */
      def expandUnassigned(unassigned: List[Sym], model: Model): List[Model] = {
        // the number of solutions is doubled for every unassigned variable
        val expandedModels = 1 << unassigned.size
        var current = mutable.ArrayBuffer[Model]()
        var next = mutable.ArrayBuffer[Model]()
        current.sizeHint(expandedModels)
        next.sizeHint(expandedModels)

        current += model

        // we use double buffering:
        // read from `current` and create a two models for each model in `next`
        for {
          s <- unassigned
        } {
          for {
            model <- current
          } {
            def force(l: Lit) = model + (l.sym -> l.pos)

            next += force(Lit(s, pos = true))
            next += force(Lit(s, pos = false))
          }

          val tmp = current
          current = next
          next = tmp

          next.clear()
        }

        current.toList
      }

      def findAllModels(f: Formula,
                        models: List[Model],
                        recursionDepthAllowed: Int = global.settings.YpatmatExhaustdepth.value): List[Model]=
        if (recursionDepthAllowed == 0) {
          val maxDPLLdepth = global.settings.YpatmatExhaustdepth.value
          reportWarning("(Exhaustivity analysis reached max recursion depth, not all missing cases are reported. " +
              s"Please try with scalac -Ypatmat-exhaust-depth ${maxDPLLdepth * 2} or -Ypatmat-exhaust-depth off.)")
          models
        } else {
          val model = findModelFor(f)
          // if we found a solution, conjunct the formula with the model's negation and recurse
          if (model ne NoModel) {
            val unassigned = (vars -- model.keySet).toList
            debug.patmat("unassigned "+ unassigned +" in "+ model)

            val forced = expandUnassigned(unassigned, model)
            debug.patmat("forced "+ forced)
            val negated = negateModel(model)
            findAllModels(f :+ negated, forced ++ models, recursionDepthAllowed - 1)
          }
          else models
        }

      findAllModels(f, Nil)
    }

    private def withLit(res: Model, l: Lit): Model = if (res eq NoModel) NoModel else res + (l.sym -> l.pos)
    private def dropUnit(f: Formula, unitLit: Lit): Formula = {
      val negated = -unitLit
      // drop entire clauses that are trivially true
      // (i.e., disjunctions that contain the literal we're making true in the returned model),
      // and simplify clauses by dropping the negation of the literal we're making true
      // (since False \/ X == X)
      val dropped = formulaBuilderSized(f.size)
      for {
        clause <- f
        if !(clause contains unitLit)
      } dropped += (clause - negated)
      dropped
    }

    def findModelFor(f: Formula): Model = {
      @inline def orElse(a: Model, b: => Model) = if (a ne NoModel) a else b

      debug.patmat("DPLL\n"+ cnfString(f))

      val start = if (Statistics.canEnable) Statistics.startTimer(patmatAnaDPLL) else null

      val satisfiableWithModel: Model =
        if (f isEmpty) EmptyModel
        else if(f exists (_.isEmpty)) NoModel
        else f.find(_.size == 1) match {
          case Some(unitClause) =>
            val unitLit = unitClause.head
            // debug.patmat("unit: "+ unitLit)
            withLit(findModelFor(dropUnit(f, unitLit)), unitLit)
          case _ =>
            // partition symbols according to whether they appear in positive and/or negative literals
            val pos = new mutable.HashSet[Sym]()
            val neg = new mutable.HashSet[Sym]()
            mforeach(f)(lit => if (lit.pos) pos += lit.sym else neg += lit.sym)

            // appearing in both positive and negative
            val impures = pos intersect neg
            // appearing only in either positive/negative positions
            val pures = (pos ++ neg) -- impures

            if (pures nonEmpty) {
              val pureSym = pures.head
              // turn it back into a literal
              // (since equality on literals is in terms of equality
              //  of the underlying symbol and its positivity, simply construct a new Lit)
              val pureLit = Lit(pureSym, pos(pureSym))
              // debug.patmat("pure: "+ pureLit +" pures: "+ pures +" impures: "+ impures)
              val simplified = f.filterNot(_.contains(pureLit))
              withLit(findModelFor(simplified), pureLit)
            } else {
              val split = f.head.head
              // debug.patmat("split: "+ split)
              orElse(findModelFor(f :+ clause(split)), findModelFor(f :+ clause(-split)))
            }
        }

      if (Statistics.canEnable) Statistics.stopTimer(patmatAnaDPLL, start)
      satisfiableWithModel
    }
  }
}
