/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * Author: Paul Phillips
 */

package scala.tools.nsc
package matching

import transform.ExplicitOuter
import symtab.Flags
import scala.collection.mutable
import language.implicitConversions

trait Matrix extends MatrixAdditions {
  self: ExplicitOuter with ParallelMatching =>

  import global.{ typer => _, _ }
  import analyzer.Typer
  import CODE._
  import Debug._
  import Flags.{ SYNTHETIC, MUTABLE }

  private[matching] val NO_EXHAUSTIVE = Flags.TRANS_FLAG

  /** Translation of match expressions.
   *
   *  `p`:  pattern
   *  `g`:  guard
   *  `bx`: body index
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

  /** "The Mixture Rule"

        {v=pat1, pats1 .. } {q1}
  match {..               } {..}
        {v=patn, patsn .. } {qn}

  The is the real work-horse of the algorithm. There is some column whose top-most pattern is a
  constructor. (Forsimplicity, itisdepicted above asthe left-most column, but anycolumn will do.)
  The goal is to build a test state with the variablevand some outgoing arcs (one for each construc-
  tor and possibly a default arc). Foreach constructor in the selected column, its arc is deﬁned as
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

  /** Handles all translation of pattern matching.
   */
  def handlePattern(
    selector: Tree,         // tree being matched upon (called scrutinee after this)
    cases: List[CaseDef],   // list of cases in the match
    isChecked: Boolean,     // whether exhaustiveness checking is enabled (disabled with @unchecked)
    context: MatrixContext): Tree =
  {
    import context._
    TRACE("handlePattern", "(%s: %s) match { %s cases }", selector, selector.tpe, cases.size)

    val matrixInit: MatrixInit = {
      val v = copyVar(selector, isChecked, selector.tpe, "temp")
      MatrixInit(List(v), cases, atPos(selector.pos)(MATCHERROR(v.ident)))
    }
    val matrix = new MatchMatrix(context) { lazy val data = matrixInit }
    val mch     = typer typed matrix.expansion.toTree
    val dfatree = typer typed Block(matrix.data.valDefs, mch)

    // redundancy check
    matrix.targets filter (_.unreached) foreach (cs => cunit.error(cs.body.pos, "unreachable code"))
    // optimize performs squeezing and resets any remaining NO_EXHAUSTIVE
    tracing("handlePattern")(matrix optimize dfatree)
  }

  case class MatrixContext(
    cunit: CompilationUnit,       // current unit
    handleOuter: Tree => Tree,    // for outer pointer
    typer: Typer,                 // a local typer
    owner: Symbol,                // the current owner
    matchResultType: Type)        // the expected result type of the whole match
      extends Squeezer
  {
    private def ifNull[T](x: T, alt: T) = if (x == null) alt else x

    // NO_EXHAUSTIVE communicates there should be no exhaustiveness checking
    private def flags(checked: Boolean) = if (checked) Nil else List(NO_EXHAUSTIVE)

    // Recording the symbols of the synthetics we create so we don't go clearing
    // anyone else's mutable flags.
    private val _syntheticSyms = mutable.HashSet[Symbol]()
    def clearSyntheticSyms() = {
      _syntheticSyms foreach (_ resetFlag (NO_EXHAUSTIVE|MUTABLE))
      debuglog("Cleared NO_EXHAUSTIVE/MUTABLE on " + _syntheticSyms.size + " synthetic symbols.")
      _syntheticSyms.clear()
    }
    def recordSyntheticSym(sym: Symbol): Symbol = {
      _syntheticSyms += sym
      if (_syntheticSyms.size > 25000) {
        cunit.error(owner.pos, "Sanity check failed: over 25000 symbols created for pattern match.")
        abort("This is a bug in the pattern matcher.")
      }
      sym
    }

    case class MatrixInit(
      roots: List[PatternVar],
      cases: List[CaseDef],
      default: Tree
    ) {
      def tvars = roots map (_.lhs)
      def valDefs = roots map (_.valDef)
      override def toString() = "MatrixInit(roots = %s, %d cases)".format(pp(roots), cases.size)
    }

    implicit def pvlist2pvgroup(xs: List[PatternVar]): PatternVarGroup =
      PatternVarGroup(xs)

    object PatternVarGroup {
      def apply(xs: PatternVar*) = new PatternVarGroup(xs.toList)
      def apply(xs: List[PatternVar]) = new PatternVarGroup(xs)

      // XXX - transitional
      def fromBindings(vlist: List[Binding], freeVars: List[Symbol] = Nil) = {
        def vmap(v: Symbol): Option[Binding] = vlist find (_.pvar eq v)
        val info =
          if (freeVars.isEmpty) vlist
          else (freeVars map vmap).flatten

        val xs =
          for (Binding(lhs, rhs) <- info) yield
            new PatternVar(lhs, Ident(rhs) setType lhs.tpe, !(rhs hasFlag NO_EXHAUSTIVE))

        new PatternVarGroup(xs)
      }
    }

    val emptyPatternVarGroup = PatternVarGroup()
    class PatternVarGroup(val pvs: List[PatternVar]) {
      def syms    = pvs map (_.sym)
      def valDefs = pvs map (_.valDef)
      def idents  = pvs map (_.ident)

      def extractIndex(index: Int): (PatternVar, PatternVarGroup) = {
        val (t, ts) = self.extractIndex(pvs, index)
        (t, PatternVarGroup(ts))
      }

      def isEmpty = pvs.isEmpty
      def size = pvs.size
      def head = pvs.head
      def ::(t: PatternVar) = PatternVarGroup(t :: pvs)
      def :::(ts: List[PatternVar]) = PatternVarGroup(ts ::: pvs)
      def ++(other: PatternVarGroup) = PatternVarGroup(pvs ::: other.pvs)

      def apply(i: Int) = pvs(i)
      def zipWithIndex = pvs.zipWithIndex
      def indices = pvs.indices
      def map[T](f: PatternVar => T) = pvs map f
      def filter(p: PatternVar => Boolean) = PatternVarGroup(pvs filter p)

      override def toString() = pp(pvs)
    }

    /** Every temporary variable allocated is put in a PatternVar.
     */
    class PatternVar(val lhs: Symbol, val rhs: Tree, val checked: Boolean) {
      def sym = lhs
      def tpe = lhs.tpe
      if (checked)
        lhs resetFlag NO_EXHAUSTIVE
      else
        lhs setFlag NO_EXHAUSTIVE

      // See #1427 for an example of a crash which occurs unless we retype:
      // in that instance there is an existential in the pattern.
      lazy val ident  = typer typed Ident(lhs)
      lazy val valDef = typer typedValDef ValDef(lhs, rhs)

      override def toString() = "%s: %s = %s".format(lhs, tpe, rhs)
    }

    /** Given a tree, creates a new synthetic variable of the same type
     *  and assigns the tree to it.
     */
    def copyVar(
      root: Tree,
      checked: Boolean,
      _tpe: Type = null,
      label: String = "temp"): PatternVar =
    {
      val tpe   = ifNull(_tpe, root.tpe)
      val name  = cunit.freshTermName(label)
      val sym   = newVar(root.pos, tpe, flags(checked), name)

      tracing("copy")(new PatternVar(sym, root, checked))
    }

    /** Creates a new synthetic variable of the specified type and
     *  assigns the result of f(symbol) to it.
     */
    def createVar(tpe: Type, f: Symbol => Tree, checked: Boolean) = {
      val lhs = newVar(owner.pos, tpe, flags(checked))
      val rhs = f(lhs)

      tracing("create")(new PatternVar(lhs, rhs, checked))
    }
    def createLazy(tpe: Type, f: Symbol => Tree, checked: Boolean) = {
      val lhs = newVar(owner.pos, tpe, Flags.LAZY :: flags(checked))
      val rhs = f(lhs)

      tracing("createLazy")(new PatternVar(lhs, rhs, checked))
    }

    private def newVar(
      pos: Position,
      tpe: Type,
      flags: List[Long] = Nil,
      name: TermName = null): Symbol =
    {
      val n = if (name == null) cunit.freshTermName("temp") else name
      // careful: pos has special meaning
      val flagsLong = (SYNTHETIC.toLong /: flags)(_|_)
      recordSyntheticSym(owner.newVariable(n, pos, flagsLong) setInfo tpe)
    }
  }
}
