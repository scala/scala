/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package ast.parser

import symtab.Flags._
import scala.collection.mutable.ListBuffer
import scala.reflect.internal.util.{Position, SourceFile, FreshNameCreator}

/** Methods for building trees, used in the parser.  All the trees
 *  returned by this class must be untyped.
 */
abstract class TreeBuilder {
  val global: Global
  import global._

  def unit: CompilationUnit
  def source: SourceFile

  implicit def fresh: FreshNameCreator              = unit.fresh
  def o2p(offset: Int): Position                    = Position.offset(source, offset)
  def r2p(start: Int, mid: Int, end: Int): Position = rangePos(source, start, mid, end)

  def rootScalaDot(name: Name) = gen.rootScalaDot(name)
  def scalaDot(name: Name)     = gen.scalaDot(name)
  def scalaAnyRefConstr        = scalaDot(tpnme.AnyRef)
  def scalaUnitConstr          = scalaDot(tpnme.Unit)

  def convertToTypeName(t: Tree) = gen.convertToTypeName(t)

  def byNameApplication(tpe: Tree): Tree =
    AppliedTypeTree(rootScalaDot(tpnme.BYNAME_PARAM_CLASS_NAME), List(tpe))
  def repeatedApplication(tpe: Tree): Tree =
    AppliedTypeTree(rootScalaDot(tpnme.REPEATED_PARAM_CLASS_NAME), List(tpe))

  def makeImportSelector(name: Name, nameOffset: Int): ImportSelector =
    ImportSelector(name, nameOffset, name, nameOffset)

  def makeTupleTerm(elems: List[Tree]) = gen.mkTuple(elems)

  def makeTupleType(elems: List[Tree]) = gen.mkTupleType(elems)

  def makeAnnotated(t: Tree, annot: Tree): Tree =
    atPos(annot.pos union t.pos)(Annotated(annot, t))

  def makeSelfDef(name: TermName, tpt: Tree): ValDef =
    ValDef(Modifiers(PRIVATE), name, tpt, EmptyTree)

  /** Tree for `od op`, start is start0 if od.pos is borked. */
  def makePostfixSelect(start0: Int, end: Int, od: Tree, op: Name): Tree = {
    val start = if (od.pos.isDefined) od.pos.start else start0
    atPos(r2p(start, end, end + op.length)) { new PostfixSelect(od, op.encode) }
  }

  /** Create tree representing a while loop */
  def makeWhile(startPos: Int, cond: Tree, body: Tree): Tree = {
    val lname = freshTermName(nme.WHILE_PREFIX)
    def default = wrappingPos(List(cond, body)) match {
      case p if p.isDefined => p.end
      case _                => startPos
    }
    val continu = atPos(o2p(body.pos pointOrElse default)) { Apply(Ident(lname), Nil) }
    val rhs = If(cond, Block(List(body), continu), Literal(Constant(())))
    LabelDef(lname, Nil, rhs)
  }

  /** Create tree representing a do-while loop */
  def makeDoWhile(lname: TermName, body: Tree, cond: Tree): Tree = {
    val continu = Apply(Ident(lname), Nil)
    val rhs = Block(List(body), If(cond, continu, Literal(Constant(()))))
    LabelDef(lname, Nil, rhs)
  }

  /** Create block of statements `stats`  */
  def makeBlock(stats: List[Tree]): Tree = gen.mkBlock(stats)

  def makeParam(pname: TermName, tpe: Tree) =
    ValDef(Modifiers(PARAM), pname, tpe, EmptyTree)

  def makeSyntheticTypeParam(pname: TypeName, bounds: Tree) =
    TypeDef(Modifiers(DEFERRED | SYNTHETIC), pname, Nil, bounds)

  /** Create tree for a pattern alternative */
  def makeAlternative(ts: List[Tree]): Tree = {
    def alternatives(t: Tree): List[Tree] = t match {
      case Alternative(ts)  => ts
      case _                => List(t)
    }
    Alternative(ts flatMap alternatives)
  }

  /** Create tree for case definition <case pat if guard => rhs> */
  def makeCaseDef(pat: Tree, guard: Tree, rhs: Tree): CaseDef =
    CaseDef(gen.patvarTransformer.transform(pat), guard, rhs)

  /** Creates tree representing:
   *    { case x: Throwable =>
   *        val catchFn = catchExpr
   *        if (catchFn isDefinedAt x) catchFn(x) else throw x
   *    }
   */
  def makeCatchFromExpr(catchExpr: Tree): CaseDef = {
    val binder   = freshTermName()
    val pat      = Bind(binder, Typed(Ident(nme.WILDCARD), Ident(tpnme.Throwable)))
    val catchDef = ValDef(Modifiers(ARTIFACT), freshTermName("catchExpr$"), TypeTree(), catchExpr)
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

  /** Create a tree representing the function type (argtpes) => restpe */
  def makeFunctionTypeTree(argtpes: List[Tree], restpe: Tree): Tree = gen.mkFunctionTypeTree(argtpes, restpe)

  /** Append implicit parameter section if `contextBounds` nonempty */
  def addEvidenceParams(owner: Name, vparamss: List[List[ValDef]], contextBounds: List[Tree]): List[List[ValDef]] = {
    if (contextBounds.isEmpty) vparamss
    else {
      val mods = Modifiers(if (owner.isTypeName) PARAMACCESSOR | LOCAL | PRIVATE else PARAM)
      def makeEvidenceParam(tpt: Tree) = ValDef(mods | IMPLICIT | SYNTHETIC, freshTermName(nme.EVIDENCE_PARAM_PREFIX), tpt, EmptyTree)
      val evidenceParams = contextBounds map makeEvidenceParam

      val vparamssLast = if(vparamss.nonEmpty) vparamss.last else Nil
      if(vparamssLast.nonEmpty && vparamssLast.head.mods.hasFlag(IMPLICIT))
        vparamss.init ::: List(evidenceParams ::: vparamssLast)
      else
        vparamss ::: List(evidenceParams)
    }
  }

  def makePatDef(mods: Modifiers, pat: Tree, rhs: Tree) = gen.mkPatDef(mods, pat, rhs)
}
