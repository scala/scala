/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package ast.parser

import symtab.Flags._
import scala.collection.mutable.ListBuffer

/** Methods for building trees, used in the parser.  All the trees
 *  returned by this class must be untyped.
 */
abstract class TreeBuilder {

  val global: Global
  import global._

  def freshName(): Name = freshName("x$")
  def freshTermName(): TermName = freshTermName("x$")

  def freshName(prefix: String): Name
  def freshTermName(prefix: String): TermName
  def freshTypeName(prefix: String): TypeName
  def o2p(offset: Int): Position
  def r2p(start: Int, point: Int, end: Int): Position

  def rootId(name: Name)       = gen.rootId(name)
  def rootScalaDot(name: Name) = gen.rootScalaDot(name)
  def scalaDot(name: Name)     = gen.scalaDot(name)
  def scalaAnyRefConstr        = scalaDot(tpnme.AnyRef)
  def scalaAnyValConstr        = scalaDot(tpnme.AnyVal)
  def scalaAnyConstr           = scalaDot(tpnme.Any)
  def scalaUnitConstr          = scalaDot(tpnme.Unit)
  def productConstr            = scalaDot(tpnme.Product)
  def productConstrN(n: Int)   = scalaDot(newTypeName("Product" + n))
  def serializableConstr       = scalaDot(tpnme.Serializable)

  def convertToTypeName(t: Tree) = gen.convertToTypeName(t)

  /** Convert all occurrences of (lower-case) variables in a pattern as follows:
   *    x                  becomes      x @ _
   *    x: T               becomes      x @ (_: T)
   */
  private object patvarTransformer extends Transformer {
    override def transform(tree: Tree): Tree = tree match {
      case Ident(name) if (treeInfo.isVarPattern(tree) && name != nme.WILDCARD) =>
        atPos(tree.pos)(Bind(name, atPos(tree.pos.focus) (Ident(nme.WILDCARD))))
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
      case Alternative(_) | Star(_) =>
        super.transform(tree)
      case _ =>
        tree
    }
  }

  /** Traverse pattern and collect all variable names with their types in buffer
   *  The variables keep their positions; whereas the pattern is converted to be
   *  synthetic for all nodes that contain a variable position.
   */
  class GetVarTraverser extends Traverser {
    val buf = new ListBuffer[(Name, Tree, Position)]

    def namePos(tree: Tree, name: Name): Position =
      if (!tree.pos.isRange || name.containsName(nme.raw.DOLLAR)) tree.pos.focus
      else {
        val start = tree.pos.start
        val end = start + name.decode.length
        r2p(start, start, end)
      }

    override def traverse(tree: Tree): Unit = {
      def seenName(name: Name)     = buf exists (_._1 == name)
      def add(name: Name, t: Tree) = if (!seenName(name)) buf += ((name, t, namePos(tree, name)))
      val bl = buf.length

      tree match {
        case Bind(nme.WILDCARD, _)          =>
          super.traverse(tree)

        case Bind(name, Typed(tree1, tpt))  =>
          val newTree = if (treeInfo.mayBeTypePat(tpt)) TypeTree() else tpt.duplicate
          add(name, newTree)
          traverse(tree1)

        case Bind(name, tree1)              =>
          // can assume only name range as position, as otherwise might overlap
          // with binds embedded in pattern tree1
          add(name, TypeTree())
          traverse(tree1)

        case _ =>
          super.traverse(tree)
      }
      if (buf.length > bl)
        tree setPos tree.pos.makeTransparent
    }
    def apply(tree: Tree) = {
      traverse(tree)
      buf.toList
    }
  }

  /** Returns list of all pattern variables, possibly with their types,
   *  without duplicates
   */
  private def getVariables(tree: Tree): List[(Name, Tree, Position)] =
    new GetVarTraverser apply tree

  def byNameApplication(tpe: Tree): Tree =
    AppliedTypeTree(rootScalaDot(tpnme.BYNAME_PARAM_CLASS_NAME), List(tpe))
  def repeatedApplication(tpe: Tree): Tree =
    AppliedTypeTree(rootScalaDot(tpnme.REPEATED_PARAM_CLASS_NAME), List(tpe))

  def makeImportSelector(name: Name, nameOffset: Int): ImportSelector =
    ImportSelector(name, nameOffset, name, nameOffset)

  private def makeTuple(trees: List[Tree], isType: Boolean): Tree = {
    val tupString = "Tuple" + trees.length
    Apply(scalaDot(if (isType) newTypeName(tupString) else newTermName(tupString)), trees)
  }

  def makeTupleTerm(trees: List[Tree], flattenUnary: Boolean): Tree = trees match {
    case Nil => Literal(Constant())
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

  def makeAnnotated(t: Tree, annot: Tree): Tree =
    atPos(annot.pos union t.pos)(Annotated(annot, t))

  def makeSelfDef(name: TermName, tpt: Tree): ValDef =
    ValDef(Modifiers(PRIVATE), name, tpt, EmptyTree)

  /** If tree is a variable pattern, return Some("its name and type").
   *  Otherwise return none */
  private def matchVarPattern(tree: Tree): Option[(Name, Tree)] = {
    def wildType(t: Tree): Option[Tree] = t match {
      case Ident(x) if x.toTermName == nme.WILDCARD             => Some(TypeTree())
      case Typed(Ident(x), tpt) if x.toTermName == nme.WILDCARD => Some(tpt)
      case _                                                    => None
    }
    tree match {
      case Ident(name)             => Some((name, TypeTree()))
      case Bind(name, body)        => wildType(body) map (x => (name, x))
      case Typed(Ident(name), tpt) => Some((name, tpt))
      case _                       => None
    }
  }

  /** Create tree representing (unencoded) binary operation expression or pattern. */
  def makeBinop(isExpr: Boolean, left: Tree, op: TermName, right: Tree, opPos: Position): Tree = {
    def mkNamed(args: List[Tree]) =
      if (isExpr) args map {
        case a @ Assign(id @ Ident(name), rhs) =>
          atPos(a.pos) { AssignOrNamedArg(id, rhs) }
        case e => e
      } else args
    val arguments = right match {
      case Parens(args) => mkNamed(args)
      case _ => List(right)
    }
    if (isExpr) {
      if (treeInfo.isLeftAssoc(op)) {
        Apply(atPos(opPos union left.pos) { Select(stripParens(left), op.encode) }, arguments)
      } else {
        val x = freshTermName()
        Block(
          List(ValDef(Modifiers(SYNTHETIC), x, TypeTree(), stripParens(left))),
          Apply(atPos(opPos union right.pos) { Select(stripParens(right), op.encode) }, List(Ident(x))))
      }
    } else {
      Apply(Ident(op.encode), stripParens(left) :: arguments)
    }
  }

  /** Create positioned tree representing an object creation <new parents { stats }
   *  @param npos  the position of the new
   *  @param cpos  the position of the anonymous class starting with parents
   */
  def makeNew(parents: List[Tree], self: ValDef, stats: List[Tree], argss: List[List[Tree]],
              npos: Position, cpos: Position): Tree =
    if (parents.isEmpty)
      makeNew(List(scalaAnyRefConstr), self, stats, argss, npos, cpos)
    else if (parents.tail.isEmpty && stats.isEmpty)
      atPos(npos union cpos) { New(parents.head, argss) }
    else {
      val x = tpnme.ANON_CLASS_NAME
      atPos(npos union cpos) {
        Block(
          List(
            atPos(cpos) {
              ClassDef(
                Modifiers(FINAL), x, Nil,
                Template(parents, self, NoMods, List(Nil), argss, stats, cpos.focus))
            }),
          atPos(npos) {
            New(
              Ident(x) setPos npos.focus,
              List(Nil))
          }
        )
      }
    }

  /** Create a tree representing an assignment <lhs = rhs> */
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
  def makeWhile(lname: TermName, cond: Tree, body: Tree): Tree = {
    val continu = atPos(o2p(body.pos.endOrPoint)) { Apply(Ident(lname), Nil) }
    val rhs = If(cond, Block(List(body), continu), Literal(Constant()))
    LabelDef(lname, Nil, rhs)
  }

  /** Create tree representing a do-while loop */
  def makeDoWhile(lname: TermName, body: Tree, cond: Tree): Tree = {
    val continu = Apply(Ident(lname), Nil)
    val rhs = Block(List(body), If(cond, continu, Literal(Constant())))
    LabelDef(lname, Nil, rhs)
  }

  /** Create block of statements `stats`  */
  def makeBlock(stats: List[Tree]): Tree =
    if (stats.isEmpty) Literal(Constant())
    else if (!stats.last.isTerm) Block(stats, Literal(Constant()))
    else if (stats.length == 1) stats.head
    else Block(stats.init, stats.last)

  /** Create tree for for-comprehension generator <val pat0 <- rhs0> */
  def makeGenerator(pos: Position, pat: Tree, valeq: Boolean, rhs: Tree): Enumerator = {
    val pat1 = patvarTransformer.transform(pat)
    val rhs1 =
      if (valeq) rhs
      else matchVarPattern(pat1) match {
        case Some(_) =>
          rhs
        case None =>
          atPos(rhs.pos) {
            Apply(
              Select(rhs, nme.filter),
              List(
                makeVisitor(
                  List(
                    CaseDef(pat1.duplicate, EmptyTree, Literal(Constant(true))),
                    CaseDef(Ident(nme.WILDCARD), EmptyTree, Literal(Constant(false)))),
                  false,
                  nme.CHECK_IF_REFUTABLE_STRING
                )))
          }
      }
    if (valeq) ValEq(pos, pat1, rhs1) else ValFrom(pos, pat1, rhs1)
  }

  def makeParam(pname: TermName, tpe: Tree) =
    ValDef(Modifiers(PARAM), pname, tpe, EmptyTree)

  def makeSyntheticParam(pname: TermName) =
    ValDef(Modifiers(PARAM | SYNTHETIC), pname, TypeTree(), EmptyTree)

  def makeSyntheticTypeParam(pname: TypeName, bounds: Tree) =
    TypeDef(Modifiers(DEFERRED | SYNTHETIC), pname, Nil, bounds)

  abstract class Enumerator { def pos: Position }
  case class ValFrom(pos: Position, pat: Tree, rhs: Tree) extends Enumerator
  case class ValEq(pos: Position, pat: Tree, rhs: Tree) extends Enumerator
  case class Filter(pos: Position, test: Tree) extends Enumerator

  /** Create tree for for-comprehension <for (enums) do body> or
  *   <for (enums) yield body> where mapName and flatMapName are chosen
  *  corresponding to whether this is a for-do or a for-yield.
  *  The creation performs the following rewrite rules:
  *
  *  1.
  *
  *    for (P <- G) E   ==>   G.foreach (P => E)
  *
  *     Here and in the following (P => E) is interpreted as the function (P => E)
  *     if P is a variable pattern and as the partial function { case P => E } otherwise.
  *
  *  2.
  *
  *    for (P <- G) yield E  ==>  G.map (P => E)
  *
  *  3.
  *
  *    for (P_1 <- G_1; P_2 <- G_2; ...) ...
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
  *    for (P_1 <- G; P_2 = E_2; val P_N = E_N; ...)
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
  *  @param mapName      The name to be used for maps (either map or foreach)
  *  @param flatMapName  The name to be used for flatMaps (either flatMap or foreach)
  *  @param enums        The enumerators in the for expression
  *  @param body          The body of the for expression
  */
  private def makeFor(mapName: TermName, flatMapName: TermName, enums: List[Enumerator], body: Tree): Tree = {

    /** make a closure pat => body.
     *  The closure is assigned a transparent position with the point at pos.point and
     *  the limits given by pat and body.
     */
    def makeClosure(pos: Position, pat: Tree, body: Tree): Tree = {
      def splitpos = wrappingPos(List(pat, body)).withPoint(pos.point).makeTransparent
      matchVarPattern(pat) match {
        case Some((name, tpt)) =>
          Function(
            List(atPos(pat.pos) { ValDef(Modifiers(PARAM), name.toTermName, tpt, EmptyTree) }),
            body) setPos splitpos
        case None =>
          atPos(splitpos) {
            makeVisitor(List(CaseDef(pat, EmptyTree, body)), false)
          }
      }
    }

    /** Make an application  qual.meth(pat => body) positioned at `pos`.
     */
    def makeCombination(pos: Position, meth: TermName, qual: Tree, pat: Tree, body: Tree): Tree =
      Apply(Select(qual, meth) setPos qual.pos, List(makeClosure(pos, pat, body))) setPos pos

    /** Optionally, if pattern is a `Bind`, the bound name, otherwise None.
     */
    def patternVar(pat: Tree): Option[Name] = pat match {
      case Bind(name, _) => Some(name)
      case _ => None
    }

    /** If `pat` is not yet a `Bind` wrap it in one with a fresh name
     */
    def makeBind(pat: Tree): Tree = pat match {
      case Bind(_, _) => pat
      case _ => Bind(freshName(), pat) setPos pat.pos
    }

    /** A reference to the name bound in Bind `pat`.
     */
    def makeValue(pat: Tree): Tree = pat match {
      case Bind(name, _) => Ident(name) setPos pat.pos.focus
    }

    /** The position of the closure that starts with generator at position `genpos`.
     */
    def closurePos(genpos: Position) = {
      val end = body.pos match {
        case NoPosition => genpos.point
        case bodypos => bodypos.endOrPoint
      }
      r2p(genpos.startOrPoint, genpos.point, end)
    }

//    val result =
    enums match {
      case ValFrom(pos, pat, rhs) :: Nil =>
        makeCombination(closurePos(pos), mapName, rhs, pat, body)
      case ValFrom(pos, pat, rhs) :: (rest @ (ValFrom(_,  _, _) :: _)) =>
        makeCombination(closurePos(pos), flatMapName, rhs, pat,
                        makeFor(mapName, flatMapName, rest, body))
      case ValFrom(pos, pat, rhs) :: Filter(_, test) :: rest =>
        makeFor(mapName, flatMapName,
                ValFrom(pos, pat, makeCombination(rhs.pos union test.pos, nme.withFilter, rhs, pat.duplicate, test)) :: rest,
                body)
      case ValFrom(pos, pat, rhs) :: rest =>
        val valeqs = rest.take(definitions.MaxTupleArity - 1).takeWhile(_.isInstanceOf[ValEq]);
        assert(!valeqs.isEmpty)
        val rest1 = rest.drop(valeqs.length)
        val pats = valeqs map { case ValEq(_, pat, _) => pat }
        val rhss = valeqs map { case ValEq(_, _, rhs) => rhs }
        val defpat1 = makeBind(pat)
        val defpats = pats map makeBind
        val pdefs = (defpats, rhss).zipped flatMap makePatDef
        val ids = (defpat1 :: defpats) map makeValue
        val rhs1 = makeForYield(
          List(ValFrom(pos, defpat1, rhs)),
          Block(pdefs, atPos(wrappingPos(ids)) { makeTupleTerm(ids, true) }) setPos wrappingPos(pdefs))
        val allpats = (pat :: pats) map (_.duplicate)
        val vfrom1 = ValFrom(r2p(pos.startOrPoint, pos.point, rhs1.pos.endOrPoint), atPos(wrappingPos(allpats)) { makeTuple(allpats, false) } , rhs1)
        makeFor(mapName, flatMapName, vfrom1 :: rest1, body)
      case _ =>
        EmptyTree //may happen for erroneous input
    }
//    println("made for "+result)
//    result
  }

  /** Create tree for for-do comprehension <for (enums) body> */
  def makeFor(enums: List[Enumerator], body: Tree): Tree =
    makeFor(nme.foreach, nme.foreach, enums, body)

  /** Create tree for for-yield comprehension <for (enums) yield body> */
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

  /** Create visitor <x => x match cases> */
  def makeVisitor(cases: List[CaseDef], checkExhaustive: Boolean): Tree =
    makeVisitor(cases, checkExhaustive, "x$")

  /** Create visitor <x => x match cases> */
  def makeVisitor(cases: List[CaseDef], checkExhaustive: Boolean, prefix: String): Tree = {
    val x   = freshTermName(prefix)
    val id  = Ident(x)
    val sel = if (checkExhaustive) id else gen.mkUnchecked(id)
    Function(List(makeSyntheticParam(x)), Match(sel, cases))
  }

  /** Create tree for case definition <case pat if guard => rhs> */
  def makeCaseDef(pat: Tree, guard: Tree, rhs: Tree): CaseDef =
    CaseDef(patvarTransformer.transform(pat), guard, rhs)

  /** Creates tree representing:
   *    { case x: Throwable =>
   *        val catchFn = catchExpr
   *        if (catchFn isDefinedAt x) catchFn(x) else throw x
   *    }
   */
  def makeCatchFromExpr(catchExpr: Tree): CaseDef = {
    val binder   = freshTermName("x")
    val pat      = Bind(binder, Typed(Ident(nme.WILDCARD), Ident(tpnme.Throwable)))
    val catchDef = ValDef(NoMods, freshTermName("catchExpr"), TypeTree(), catchExpr)
    val catchFn  = Ident(catchDef.name)
    val body     = atPos(catchExpr.pos.makeTransparent)(Block(
      List(catchDef),
      If(
        Apply(Select(catchFn, nme.isDefinedAt), List(Ident(binder))),
        Apply(Select(catchFn, nme.apply), List(Ident(binder))),
        Throw(Ident(binder))
      )
    ))
    makeCaseDef(pat, EmptyTree, body)
  }

  /** Create tree for pattern definition <val pat0 = rhs> */
  def makePatDef(pat: Tree, rhs: Tree): List[Tree] =
    makePatDef(Modifiers(0), pat, rhs)

  /** For debugging only.  Desugar a match statement like so:
   *  val x = scrutinee
   *  x match {
   *    case case1 => ...
   *    case _ => x match {
   *       case case2 => ...
   *       case _ => x match ...
   *    }
   *  }
   *
   *  This way there are never transitions between nontrivial casedefs.
   *  Of course many things break: exhaustiveness and unreachable checking
   *  do not work, no switches will be generated, etc.
   */
  def makeSequencedMatch(selector: Tree, cases: List[CaseDef]): Tree = {
    require(cases.nonEmpty)

    val selectorName = freshTermName()
    val valdef = atPos(selector.pos)(ValDef(Modifiers(PrivateLocal | SYNTHETIC), selectorName, TypeTree(), selector))
    val nselector = Ident(selectorName)

    def loop(cds: List[CaseDef]): Match = {
      def mkNext = CaseDef(Ident(nme.WILDCARD), EmptyTree, loop(cds.tail))

      if (cds.size == 1) Match(nselector, cds)
      else Match(selector, List(cds.head, mkNext))
    }

    Block(List(valdef), loop(cases))
  }

  /** Create tree for pattern definition <mods val pat0 = rhs> */
  def makePatDef(mods: Modifiers, pat: Tree, rhs: Tree): List[Tree] = matchVarPattern(pat) match {
    case Some((name, tpt)) =>
      List(atPos(pat.pos union rhs.pos) {
        ValDef(mods, name.toTermName, tpt, rhs)
      })

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
      val matchExpr = atPos((pat1.pos union rhs.pos).makeTransparent) {
        Match(
          gen.mkUnchecked(rhs),
          List(
            atPos(pat1.pos) {
              CaseDef(pat1, EmptyTree, makeTupleTerm(vars map (_._1) map Ident, true))
            }
          ))
      }
      vars match {
        case List((vname, tpt, pos)) =>
          List(atPos(pat.pos union pos union rhs.pos) {
            ValDef(mods, vname.toTermName, tpt, matchExpr)
          })
        case _ =>
          val tmp = freshTermName()
          val firstDef =
            atPos(matchExpr.pos) {
              ValDef(Modifiers(PrivateLocal | SYNTHETIC | (mods.flags & LAZY)),
                     tmp, TypeTree(), matchExpr)
            }
          var cnt = 0
          val restDefs = for ((vname, tpt, pos) <- vars) yield atPos(pos) {
            cnt += 1
            ValDef(mods, vname.toTermName, tpt, Select(Ident(tmp), newTermName("_" + cnt)))
          }
          firstDef :: restDefs
      }
  }

  /** Create a tree representing the function type (argtpes) => restpe */
  def makeFunctionTypeTree(argtpes: List[Tree], restpe: Tree): Tree =
    AppliedTypeTree(rootScalaDot(newTypeName("Function" + argtpes.length)), argtpes ::: List(restpe))

  /** Append implicit parameter section if `contextBounds` nonempty */
  def addEvidenceParams(owner: Name, vparamss: List[List[ValDef]], contextBounds: List[Tree]): List[List[ValDef]] = {
    if (contextBounds.isEmpty) vparamss
    else {
      val mods = Modifiers(if (owner.isTypeName) PARAMACCESSOR | LOCAL | PRIVATE else PARAM)
      def makeEvidenceParam(tpt: Tree) = ValDef(mods | IMPLICIT, freshTermName(nme.EVIDENCE_PARAM_PREFIX), tpt, EmptyTree)
      val evidenceParams = contextBounds map makeEvidenceParam

      val vparamssLast = if(vparamss.nonEmpty) vparamss.last else Nil
      if(vparamssLast.nonEmpty && vparamssLast.head.mods.hasFlag(IMPLICIT))
        vparamss.init ::: List(evidenceParams ::: vparamssLast)
      else
        vparamss ::: List(evidenceParams)
    }
  }
}
