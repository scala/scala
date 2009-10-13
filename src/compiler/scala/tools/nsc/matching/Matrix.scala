/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * Author: Paul Phillips
 */

package scala.tools.nsc
package matching

import transform.ExplicitOuter
import util.Position
import symtab.Flags

trait Matrix extends MatrixAdditions {
  self: ExplicitOuter with ParallelMatching =>

  import global.{ typer => _, _ }
  import analyzer.Typer
  import CODE._
  import Debug._
  import Flags.{ TRANS_FLAG }

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

  case class MatrixContext(
    handleOuter: Tree => Tree,    // for outer pointer
    typer: Typer,                 // a local typer
    owner: Symbol,                // the current owner
    matchResultType: Type)        // the expected result type of the whole match
      extends Squeezer
  {
    private def ifNull[T](x: T, alt: T) = if (x == null) alt else x

    // TRANS_FLAG communicates there should be no exhaustiveness checking
    private def flags(checked: Boolean) = if (checked) Nil else List(TRANS_FLAG)

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
            new PatternVar(lhs, Ident(rhs) setType lhs.tpe, !(rhs hasFlag TRANS_FLAG))

        new PatternVarGroup(xs)
      }
    }

    val emptyPatternVarGroup = PatternVarGroup()
    class PatternVarGroup(val pvs: List[PatternVar]) {
      def syms = pvs map (_.sym)
      def valDefs = pvs map (_.valDef)
      def idents = pvs map (_.ident)

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
      def valsym = valDef.symbol
      // XXX how will valsym.tpe differ from sym.tpe ?
      def tpe = valsym.tpe

      lazy val ident  = ID(lhs)
      lazy val valDef = tracing("typedVal", typer typedValDef (VAL(lhs) === rhs))

      override def toString() = "%s: %s = %s".format(lhs, lhs.info, rhs)
    }
    // val NoPatternVar = PatternVar(NoSymbol, EmptyTree, false)

    /** Sets the rhs to EmptyTree, which makes the valDef ignored in Scrutinee.
     */
    def specialVar(lhs: Symbol, checked: Boolean) =
      new PatternVar(lhs, EmptyTree, checked)

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
      val name  = newName(root.pos, label)
      val sym   = newVar(root.pos, tpe, flags(checked), name)

      tracing("copy", new PatternVar(sym, root, checked))
    }

    /** Creates a new synthetic variable of the specified type and
     *  assigns the result of f(symbol) to it.
     */
    def createVar(tpe: Type, f: Symbol => Tree, checked: Boolean) = {
      val lhs = newVar(owner.pos, tpe, flags(checked))
      val rhs = f(lhs)

      tracing("create", new PatternVar(lhs, rhs, checked))
    }

    private def newVar(
      pos: Position,
      tpe: Type,
      flags: List[Long] = Nil,
      name: Name = null): Symbol =
    {
      val n: Name = if (name == null) newName(pos, "temp") else name
      // careful: pos has special meaning
      owner.newVariable(pos, n) setInfo tpe setFlag (0L /: flags)(_|_)
    }

    def typedValDef(x: Symbol, rhs: Tree) =
      tracing("typedVal", typer typedValDef (VAL(x) === rhs))
  }
}