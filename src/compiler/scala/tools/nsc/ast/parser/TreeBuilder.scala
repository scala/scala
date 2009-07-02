/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc.ast.parser

import symtab.Flags._
import scala.collection.mutable.ListBuffer
import scala.tools.nsc.util.Position

/** Methods for building trees, used in the parser.  All the trees
 *  returned by this class must be untyped.
 */
abstract class TreeBuilder {

  val global: Global
  import global._

  def freshName(prefix: String): Name
  def freshName(): Name = freshName("x$")

  /* These methods should probably be factored into a shared trait.
   * Be careful what you share between TreeGen and TreeBuilder!
   */
  def rootId                  = gen.rootId _
  def rootScalaDot            = gen.rootScalaDot _
  def scalaDot                = gen.scalaDot _
  def scalaAnyRefConstr       = gen.scalaAnyRefConstr
  def scalaUnitConstr         = gen.scalaUnitConstr
  def scalaScalaObjectConstr  = gen.scalaScalaObjectConstr
  def productConstr           = gen.productConstr

  /** Create a tree representing the function type (argtpes) => restpe */
  def makeFunctionTypeTree    = gen.scalaFunctionConstr _

  /** Convert all occurrences of (lower-case) variables in a pattern as follows:
   *    x                  becomes      x @ _
   *    x: T               becomes      x @ (_: T)
   */
  private object patvarTransformer extends Transformer {
    override def transform(tree: Tree): Tree = tree match {
      case Ident(name) if (treeInfo.isVarPattern(tree) && name != nme.WILDCARD) =>
        atPos(tree.pos)(Bind(name, atPos(tree) (Ident(nme.WILDCARD))))
      case Typed(id @ Ident(name), tpt) if (treeInfo.isVarPattern(id) && name != nme.WILDCARD) =>
        atPos(tree.pos.withPoint(id.pos.point)) {
          Bind(name, atPos(tree.pos.withStart(tree.pos.point)) {
            Typed(Ident(nme.WILDCARD), tpt)
          })
        }
      case Apply(fn @ Apply(_, _), args) =>
        treeCopy.Apply(tree, transform(fn), transformTrees(args))
      case Apply(fn, args) =>
        treeCopy.Apply(tree, fn, transformTrees(args))
      case Typed(expr, tpt) =>
        treeCopy.Typed(tree, transform(expr), tpt)
      case Bind(name, body) =>
        treeCopy.Bind(tree, name, transform(body))
      case Sequence(_) | Alternative(_) | Star(_) =>
        super.transform(tree)
      case _ =>
        tree
    }
  }

  /** Traverse pattern and collect all variable names with their types in buffer */
  private object getvarTraverser extends Traverser {
    val buf = new ListBuffer[(Name, Tree, Position)]
    def init: Traverser = { buf.clear; this }
    override def traverse(tree: Tree): Unit = tree match {
      case Bind(name, Typed(tree1, tpt)) =>
        if ((name != nme.WILDCARD) && (buf.iterator forall (name !=)))
          buf += ((name, if (treeInfo.mayBeTypePat(tpt)) TypeTree() else tpt, tree.pos))
        traverse(tree1)
      case Bind(name, tree1) =>
        if ((name != nme.WILDCARD) && (buf.iterator forall (name !=)))
          buf += ((name, TypeTree(), tree.pos))
        traverse(tree1)
      case _ =>
        super.traverse(tree)
    }
  }

  /** Returns list of all pattern variables, possibly with their types,
   *  without duplicates
   */
  private def getVariables(tree: Tree): List[(Name, Tree,Position)] = {
    getvarTraverser.init.traverse(tree)
    getvarTraverser.buf.toList
  }

  private def makeTuple(trees: List[Tree], isType: Boolean): Tree = {
    val tupString = "Tuple" + trees.length
    Apply(scalaDot(if (isType) newTypeName(tupString) else newTermName(tupString)), trees)
  }

  def makeTupleTerm(trees: List[Tree], flattenUnary: Boolean): Tree = trees match {
    case Nil => Literal(())
    case List(tree) if flattenUnary => tree
    case _ => makeTuple(trees, false)
  }

  def makeTupleType(trees: List[Tree], flattenUnary: Boolean): Tree = trees match {
    case Nil => scalaUnitConstr
    case List(tree) if flattenUnary => tree
    case _ => AppliedTypeTree(scalaDot(newTypeName("Tuple" + trees.length)), trees)
  }

  def stripParens(t: Tree) = t match {
    case Parens(ts) => atPos(t.pos) { makeTupleTerm(ts, true) }
    case _ => t
  }

  def makeAnnotated(t: Tree, annot: Tree): Tree = atPos(annot.pos union t.pos)(Annotated(annot, t))

  def makeSelfDef(name: Name, tpt: Tree): ValDef =
    ValDef(Modifiers(PRIVATE), name, tpt, EmptyTree)

  /** If tree is a variable pattern, return Some("its name and type").
   *  Otherwise return none */
  private def matchVarPattern(tree: Tree): Option[(Name, Tree)] = tree match {
    case Ident(name) => Some((name, TypeTree()))
    case Bind(name, Ident(nme.WILDCARD)) => Some((name, TypeTree()))
    case Typed(Ident(name), tpt) => Some((name, tpt))
    case Bind(name, Typed(Ident(nme.WILDCARD), tpt)) => Some((name, tpt))
    case _ => None
  }

  /** Create tree representing (unencoded) binary operation expression or pattern. */
  def makeBinop(isExpr: Boolean, left: Tree, op: Name, right: Tree): Tree = {
    val arguments = right match {
      case Parens(args) => args
      case _ => List(right)
    }
    if (isExpr) {
      if (treeInfo.isLeftAssoc(op)) {
        Apply(Select(stripParens(left), op.encode), arguments)
      } else {
        val x = freshName()
        Block(
          List(ValDef(Modifiers(SYNTHETIC), x, TypeTree(), stripParens(left))),
          Apply(Select(stripParens(right), op.encode), List(Ident(x))))
      }
    } else {
      Apply(Ident(op.encode), stripParens(left) :: arguments)
    }
  }

  /** Create tree representing an object creation <new parents { stats }> */
  def makeNew(parents: List[Tree], self: ValDef, stats: List[Tree], argss: List[List[Tree]]): Tree =
    if (parents.isEmpty)
      makeNew(List(scalaAnyRefConstr), self, stats, argss)
    else if (parents.tail.isEmpty && stats.isEmpty)
      New(parents.head, argss)
    else {
      val x = nme.ANON_CLASS_NAME.toTypeName
      Block(
        List(ClassDef(
          Modifiers(FINAL), x, Nil,
          Template(parents, self, NoMods, List(Nil), argss, stats))),
        New(Ident(x), List(Nil)))
    }

  /** Create a tree represeting an assignment &lt;lhs = rhs&gt; */
  def makeAssign(lhs: Tree, rhs: Tree): Tree = lhs match {
    case Apply(fn, args) =>
      Apply(atPos(fn.pos) { Select(fn, nme.update) }, args ::: List(rhs))
    case _ =>
      Assign(lhs, rhs)
  }

  /** A type tree corresponding to (possibly unary) intersection type */
  def makeIntersectionTypeTree(tps: List[Tree]): Tree =
    if (tps.tail.isEmpty) tps.head
    else CompoundTypeTree(Template(tps, emptyValDef, Nil))

  /** Create tree representing a while loop */
  def makeWhile(lname: Name, cond: Tree, body: Tree): Tree = {
    val continu = Apply(Ident(lname), Nil)
    val rhs = If(cond, Block(List(body), continu), Literal(()))
    LabelDef(lname, Nil, rhs)
  }

  /** Create tree representing a do-while loop */
  def makeDoWhile(lname: Name, body: Tree, cond: Tree): Tree = {
    val continu = Apply(Ident(lname), Nil)
    val rhs = Block(List(body), If(cond, continu, Literal(())))
    LabelDef(lname, Nil, rhs)
  }

  /** Create block of statements `stats'  */
  def makeBlock(stats: List[Tree]): Tree =
    if (stats.isEmpty) Literal(())
    else if (!stats.last.isTerm) Block(stats, Literal(()))
    else if (stats.length == 1) stats.head
    else Block(stats.init, stats.last)

  /** Create tree for for-comprehension generator &lt;val pat0 &lt;- rhs0&gt; */
  def makeGenerator(pos: Position, pat: Tree, valeq: Boolean, rhs: Tree): Enumerator = {
    val pat1 = patvarTransformer.transform(pat);
    val rhs1 =
      if (valeq) rhs
      else matchVarPattern(pat1) match {
        case Some(_) =>
          rhs
        case None =>
          atPos(pos) {
            Apply(
              Select(rhs, nme.filter),
              List(
                makeVisitor(
                  List(
                    CaseDef(pat1.syntheticDuplicate, EmptyTree, Literal(true)),
                    CaseDef(Ident(nme.WILDCARD), EmptyTree, Literal(false))),
                  false,
                  nme.CHECK_IF_REFUTABLE_STRING
                )))
          }
      }
    if (valeq) ValEq(pos, pat1, rhs1) else ValFrom(pos, pat1, rhs1)
  }

  def makeSyntheticParam(pname: Name) =
    ValDef(Modifiers(PARAM | SYNTHETIC), pname, TypeTree(), EmptyTree)

  def makeSyntheticTypeParam(pname: Name, bounds: Tree) =
    TypeDef(Modifiers(DEFERRED | SYNTHETIC), pname, Nil, bounds)

  abstract class Enumerator { def pos: Position }
  case class ValFrom(pos: Position, pat: Tree, rhs: Tree) extends Enumerator
  case class ValEq(pos: Position, pat: Tree, rhs: Tree) extends Enumerator
  case class Filter(pos: Position, test: Tree) extends Enumerator

  /** Create tree for for-comprehension &lt;for (enums) do body&gt; or
  *   &lt;for (enums) yield body&gt; where mapName and flatMapName are chosen
  *  corresponding to whether this is a for-do or a for-yield.
  *  The creation performs the following rewrite rules:
  *
  *  1.
  *
  *    for (P <- G) E   ==>   G.foreach (P => E)
  *
  *     Here and in the following (P => E) is interpreted as the function (P => E)
  *     if P is a a variable pattern and as the partial function { case P => E } otherwise.
  *
  *  2.
  *
  *    for (P <- G) yield E  ==>  G.map (P => E)
  *
  *  3.
  *
  *    for (P_1 <- G_1; val P_2 <- G_2; ...) ...
  *      ==>
  *    G_1.flatMap (P_1 => for (P_2 <- G_2; ...) ...)
  *
  *  4.
  *
  *    for (P <- G; E; ...) ...
  *      =>
  *    for (P <- G.filter (P => E); ...) ...
  *
  *  5. For N < MaxTupleArity:
  *
  *    for (P_1 <- G; val P_2 = E_2; val P_N = E_N; ...)
  *      ==>
  *    for (TupleN(P_1, P_2, ... P_N) <-
  *      for (x_1 @ P_1 <- G) yield {
  *        val x_2 @ P_2 = E_2
  *        ...
  *        val x_N & P_N = E_N
  *        TupleN(x_1, ..., x_N)
  *      } ...)
  *
  *    If any of the P_i are variable patterns, the corresponding `x_i @ P_i' is not generated
  *    and the variable constituting P_i is used instead of x_i
  *
  */
  private def makeFor(mapName: Name, flatMapName: Name, enums: List[Enumerator], body: Tree): Tree = {

    def makeClosure(pat: Tree, body: Tree): Tree = matchVarPattern(pat) match {
      case Some((name, tpt)) =>
        Function(List(ValDef(Modifiers(PARAM), name, tpt, EmptyTree)), body)
      case None =>
        makeVisitor(List(CaseDef(pat, EmptyTree, body)), false)
    }

    def makeCombination(meth: Name, qual: Tree, pat: Tree, body: Tree): Tree =
      Apply(Select(qual, meth), List(makeClosure(pat, body)));

    def patternVar(pat: Tree): Option[Name] = pat match {
      case Bind(name, _) => Some(name)
      case _ => None
    }

    def makeBind(pat: Tree): Tree = pat match {
      case Bind(_, _) => pat
      case _ => Bind(freshName(), pat)
    }

    def makeValue(pat: Tree): Tree = pat match {
      case Bind(name, _) => Ident(name)
    }

    enums match {
      case ValFrom(pos, pat, rhs) :: Nil =>
        atPos(pos union body.pos) {
          makeCombination(mapName, rhs, pat, body)
        }
      case ValFrom(pos, pat, rhs) :: (rest @ (ValFrom(_,  _, _) :: _)) =>
        atPos(pos union body.pos) {
          makeCombination(flatMapName, rhs, pat, makeFor(mapName, flatMapName, rest, body))
        }
      case ValFrom(pos, pat, rhs) :: Filter(_, test) :: rest =>
        makeFor(mapName, flatMapName,
                ValFrom(pos, pat, makeCombination(nme.filter, rhs, pat.syntheticDuplicate, test)) :: rest,
                body)
      case ValFrom(pos, pat, rhs) :: rest =>
        val valeqs = rest.take(definitions.MaxTupleArity - 1).takeWhile(_.isInstanceOf[ValEq]);
        assert(!valeqs.isEmpty)
        val rest1 = rest.drop(valeqs.length)
        val pats = valeqs map { case ValEq(_, pat, _) => pat }
        val rhss = valeqs map { case ValEq(_, _, rhs) => rhs }
        val defpats = pats map (x => makeBind(x.syntheticDuplicate))
        val pdefs = List.flatten(List.map2(defpats, rhss)(makePatDef))
        val patX1 = makeBind(pat.syntheticDuplicate);
        val ids = (patX1 :: defpats) map makeValue
        val rhs1 = makeForYield(
          List(ValFrom(pos, patX1, rhs)),
          Block(pdefs, makeTupleTerm(ids, true)))
        makeFor(mapName, flatMapName, ValFrom(pos, makeTuple(pat :: pats, false), rhs1) :: rest1, body)
      case _ =>
        EmptyTree //may happen for erroneous input
    }
  }

  /** Create tree for for-do comprehension &lt;for (enums) body&gt; */
  def makeFor(enums: List[Enumerator], body: Tree): Tree =
    makeFor(nme.foreach, nme.foreach, enums, body)

  /** Create tree for for-yield comprehension &lt;for (enums) yield body&gt; */
  def makeForYield(enums: List[Enumerator], body: Tree): Tree =
    makeFor(nme.map, nme.flatMap, enums, body)

  /** Create tree for a lifted expression XX-LIFTING
   */
  def makeLifted(gs: List[ValFrom], body: Tree): Tree = {
    def combine(gs: List[ValFrom]): ValFrom = (gs: @unchecked) match {
      case g :: Nil => g
      case ValFrom(pos1, pat1, rhs1) :: gs2 =>
        val ValFrom(pos2, pat2, rhs2) = combine(gs2)
        ValFrom(pos1, makeTuple(List(pat1, pat2), false), Apply(Select(rhs1, nme.zip), List(rhs2)))
    }
    makeForYield(List(combine(gs)), body)
  }

  /** Create tree for a pattern alternative */
  def makeAlternative(ts: List[Tree]): Tree = {
    def alternatives(t: Tree): List[Tree] = t match {
      case Alternative(ts)  => ts
      case _                => List(t)
    }
    Alternative(ts flatMap alternatives)
  }

  /** Create tree for a pattern sequence */
  def makeSequence(ts: List[Tree]): Tree = {
    def elements(t: Tree): List[Tree] = t match {
      case Sequence(ts) => ts
      case _            => List(t)
    }
    Sequence(ts flatMap elements)
  }

  /** Create visitor <x => x match cases> */
  def makeVisitor(cases: List[CaseDef], checkExhaustive: Boolean): Tree =
    makeVisitor(cases, checkExhaustive, "x$")

  private def makeUnchecked(expr: Tree): Tree = atPos(expr.pos) {
    Annotated(New(scalaDot(definitions.UncheckedClass.name), List(Nil)), expr)
  }

  /** Create visitor <x => x match cases> */
  def makeVisitor(cases: List[CaseDef], checkExhaustive: Boolean, prefix: String): Tree = {
    val x = freshName(prefix)
    val sel = if (checkExhaustive) Ident(x) else makeUnchecked(Ident(x))
    Function(List(makeSyntheticParam(x)), Match(sel, cases))
  }

  /** Create tree for case definition &lt;case pat if guard => rhs&gt; */
  def makeCaseDef(pat: Tree, guard: Tree, rhs: Tree): CaseDef =
    CaseDef(patvarTransformer.transform(pat), guard, rhs)

  /** Create tree for pattern definition &lt;val pat0 = rhs&gt; */
  def makePatDef(pat: Tree, rhs: Tree): List[Tree] =
    makePatDef(Modifiers(0), pat, rhs)

  /** Create tree for pattern definition <mods val pat0 = rhs> */
  def makePatDef(mods: Modifiers, pat: Tree, rhs: Tree): List[Tree] = matchVarPattern(pat) match {
    case Some((name, tpt)) =>
      List(ValDef(mods, name, tpt, rhs))

    case None =>
      //  in case there is exactly one variable x_1 in pattern
      //  val/var p = e  ==>  val/var x_1 = e.match (case p => (x_1))
      //
      //  in case there are zero or more than one variables in pattern
      //  val/var p = e  ==>  private synthetic val t$ = e.match (case p => (x_1, ..., x_N))
      //                  val/var x_1 = t$._1
      //                  ...
      //                  val/var x_N = t$._N
      val pat1 = patvarTransformer.transform(pat)
      val vars = getVariables(pat1)
      val matchExpr = atPos(rhs.pos){
        Match(
          makeUnchecked(rhs),
          List(
            makeSynthetic(
              atPos(rhs.pos) {
                CaseDef(pat1, EmptyTree, makeTupleTerm(vars map (_._1) map Ident, true))
              })))

      }
      vars match {
        case List((vname, tpt, pos)) =>
          List(ValDef(mods, vname, tpt, matchExpr))
        case _ =>
          val tmp = freshName()
          val firstDef = ValDef(Modifiers(PRIVATE | LOCAL | SYNTHETIC | (mods.flags & LAZY)),
                                tmp, TypeTree(), matchExpr)
          var cnt = 0
          val restDefs = for ((vname, tpt, pos) <- vars) yield atPos(pos) {
            cnt = cnt + 1
            ValDef(mods, vname, tpt, Select(Ident(tmp), newTermName("_" + cnt)))
          }
          firstDef :: restDefs
      }
  }

  /** Append implicit view section if for `implicitViews' if nonempty */
  def addImplicitViews(owner: Name, vparamss: List[List[ValDef]], implicitViews: List[Tree]): List[List[ValDef]] = {
    val mods = Modifiers(if (owner.isTypeName) PARAMACCESSOR | LOCAL | PRIVATE else PARAM)
    def makeViewParam(tpt: Tree) = ValDef(mods | IMPLICIT, freshName("view$"), tpt, EmptyTree)
    if (implicitViews.isEmpty) vparamss
    else vparamss ::: List(implicitViews map makeViewParam)
  }

}
