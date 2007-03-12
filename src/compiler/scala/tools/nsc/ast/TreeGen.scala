/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc.ast

import scala.tools.nsc.util.Position
import symtab.Flags._
import scala.collection.mutable.ListBuffer

abstract class TreeGen {

  val global: Global

  import global._
  import definitions._
  import posAssigner.atPos

  /** Builds a reference to value whose type is given stable prefix.
   */
  def mkAttributedQualifier(tpe: Type): Tree = tpe match {
    case NoPrefix =>
      EmptyTree
    case ThisType(clazz) =>
      if (clazz.isRoot || clazz.isEmptyPackageClass) EmptyTree
      else mkAttributedThis(clazz)
    case SingleType(pre, sym) =>
      val qual = mkAttributedStableRef(pre, sym)
      qual.tpe match {
        case MethodType(List(), restpe) =>
          Apply(qual, List()) setType restpe
        case _ =>
          qual
      }
    case TypeRef(pre, sym, args) =>
      if (sym.isRoot) {
        mkAttributedThis(sym)
      } else if (sym.isModuleClass) {
        val qual = mkAttributedRef(pre, sym.sourceModule);
        qual.tpe match {
          case MethodType(List(), restpe) =>
            Apply(qual, List()) setType restpe
          case _ =>
            qual
        }
      } else {
        assert(phase.erasedTypes)
        mkAttributedThis(sym)
      }
    case _ =>
      throw new Error("bad qualifier: " + tpe)
  }

  /** Builds a reference to given symbol with given stable prefix. */
  def mkAttributedRef(pre: Type, sym: Symbol): Tree = {
    val qual = mkAttributedQualifier(pre)
    qual match {
      case EmptyTree => mkAttributedIdent(sym)
      case This(clazz) if (qual.symbol.isRoot || qual.symbol.isEmptyPackageClass) => mkAttributedIdent(sym)
      case _ => mkAttributedSelect(qual, sym)
    }
  }

  /** Builds a reference to given symbol. */
  def mkAttributedRef(sym: Symbol): Tree =
    if (sym.owner.isClass) mkAttributedRef(sym.owner.thisType, sym)
    else mkAttributedIdent(sym)

  /** Replaces tree type with a stable type if possible */
  def stabilize(tree: Tree): Tree = tree match {
    case Ident(_) =>
      if (tree.symbol.isStable) tree.setType(singleType(tree.symbol.owner.thisType, tree.symbol))
      else tree
    case Select(qual, _) =>
      assert(tree.symbol ne null)
      assert(qual.tpe ne null)
      if (tree.symbol.isStable && qual.tpe.isStable)
        tree.setType(singleType(qual.tpe, tree.symbol))
      else tree
    case _ =>
      tree
  }

  /** Cast `tree' to type `pt' */
  def mkAttributedCast(tree: Tree, pt: Type): Tree = {
    if (settings.debug.value) log("casting " + tree + ":" + tree.tpe + " to " + pt)
    assert(!tree.tpe.isInstanceOf[MethodType], tree)
    typer.typed {
      atPos(tree.pos) {
        Apply(TypeApply(mkAttributedSelect(tree, Object_asInstanceOf), List(TypeTree(pt))), List())
      }
    }
  }

  /** Builds a reference with stable type to given symbol */
  def mkAttributedStableRef(pre: Type, sym: Symbol): Tree =
    stabilize(mkAttributedRef(pre, sym))

  def mkAttributedStableRef(sym: Symbol): Tree =
    stabilize(mkAttributedRef(sym))

  def mkAttributedThis(sym: Symbol): Tree =
    This(sym.name) setSymbol sym setType sym.thisType

  def mkAttributedIdent(sym: Symbol): Tree = {
    assert(sym.isTerm)
    Ident(sym.name) setSymbol sym setType sym.tpe
  }

  def mkAttributedSelect(qual: Tree, sym: Symbol): Tree =
    if ((qual.symbol ne null) &&
        (qual.symbol.name.toTermName == nme.ROOT ||
         qual.symbol.name.toTermName == nme.EMPTY_PACKAGE_NAME)) {
      mkAttributedIdent(sym)
    } else {
      assert(sym.isTerm)
      val result = Select(qual, sym.name) setSymbol sym
      if (qual.tpe ne null) result setType qual.tpe.memberType(sym)
      result
    }

  /** Builds an instance test with given value and type. */
  def mkIsInstanceOf(value: Tree, tpe: Type, erased: Boolean): Tree = { // buraq: we ignore erase, no rtt
    val sym = definitions.Any_isInstanceOf
    /*
    val sym =
      if (erased) definitions.Any_isInstanceOfErased
      else definitions.Any_isInstanceOf
        */
    Apply(
      TypeApply(
        mkAttributedSelect(value, sym),
        List(TypeTree(tpe))),
      List())
  }

  def mkIsInstanceOf(value: Tree, tpe: Type): Tree = {
    mkIsInstanceOf(value, tpe, false/*global.phase.erasedTypes*/) // buraq: ignore which phase it is
  }

  /** Builds a cast with given value and type. */
  def mkAsInstanceOf(value: Tree, tpe: Type, erased: Boolean): Tree = {
    val sym =
      if (erased) definitions.Any_asInstanceOfErased
      else definitions.Any_asInstanceOf

    Apply(
      TypeApply(
        mkAttributedSelect(value, sym),
        List(TypeTree(tpe))),
      List())
  }

  def mkAsInstanceOf(value: Tree, tpe: Type): Tree =
    mkAsInstanceOf(value, tpe, global.phase.erasedTypes)

  /** Builds a list with given head and tail. */
  def mkNewCons(head: Tree, tail: Tree): Tree =
    New(Apply(mkAttributedRef(definitions.ConsClass), List(head, tail)))

  /** Builds a list with given head and tail. */
  def mkNil: Tree =
    mkAttributedRef(definitions.NilModule)

  /** Builds a tuple */
  def mkTuple(elems: List[Tree]): Tree =
    if (elems.isEmpty) Literal(())
    else Apply(mkAttributedRef(definitions.TupleClass(elems.length).caseFactory), elems)

  def mkAnd(tree1: Tree, tree2: Tree) =
    Apply(Select(tree1, Boolean_and), List(tree2))

  def mkOr(tree1: Tree, tree2: Tree) =
    Apply(Select(tree1, Boolean_or), List(tree2))

  def mkCached(cvar: Symbol, expr: Tree): Tree = {
    val cvarRef = if (cvar.owner.isClass) Select(This(cvar.owner), cvar) else Ident(cvar)
    Block(
      List(
        If(Apply(Select(cvarRef, nme.eq), List(Literal(Constant(null)))),
           Assign(cvarRef, expr),
           EmptyTree)),
      cvarRef
    )
  }

  // var m$: T = null; or, if class member: local var m$: T = _;
  def mkModuleVarDef(accessor: Symbol) = {
    val mvar = accessor.owner.newVariable(accessor.pos, nme.moduleVarName(accessor.name))
      .setInfo(accessor.tpe.finalResultType)
      .setFlag(MODULEVAR);
    if (mvar.owner.isClass) {
      mvar setFlag (PRIVATE | LOCAL | SYNTHETIC);
      mvar.owner.info.decls.enter(mvar);
    }
    ValDef(mvar, if (mvar.owner.isClass) EmptyTree else Literal(Constant(null)))
  }

  // def m: T = { if (m$ eq null) m$ = new m$class; m$ }
  def mkModuleAccessDef(accessor: Symbol, mvar: Symbol) =
    DefDef(accessor, vparamss =>
      mkCached(mvar,
        New(TypeTree(mvar.tpe),
            List(for (val pt <- mvar.tpe.symbol.primaryConstructor.info.paramTypes)
                 yield This(accessor.owner.enclClass)))))

  // def m: T;
  def mkModuleAccessDcl(accessor: Symbol) =
    DefDef(accessor setFlag lateDEFERRED, vparamss => EmptyTree)

  def mkRuntimeCall(meth: Name, args: List[Tree]): Tree =
    Apply(Select(mkAttributedRef(ScalaRunTimeModule), meth), args)

  def evalOnce(expr: Tree, owner: Symbol, unit: CompilationUnit)(within: (() => Tree) => Tree): Tree =
    if (treeInfo.isPureExpr(expr)) {
      within(() => expr);
    } else {
      val temp = owner.newValue(expr.pos, unit.fresh.newName())
      .setFlag(SYNTHETIC).setInfo(expr.tpe);
      atPos(expr.pos) {
        Block(List(ValDef(temp, expr)), within(() => Ident(temp) setType expr.tpe))
      }
    }

  def evalOnceAll(exprs: List[Tree], owner: Symbol, unit: CompilationUnit)(within: (List[() => Tree]) => Tree): Tree = {
    val vdefs = new ListBuffer[ValDef]
    val exprs1 = new ListBuffer[() => Tree]
    for (val expr <- exprs) {
      if (treeInfo.isPureExpr(expr)) {
        exprs1 += (() => expr)
      } else {
        val temp = owner.newValue(expr.pos, unit.fresh.newName())
          .setFlag(SYNTHETIC).setInfo(expr.tpe)
        vdefs += ValDef(temp, expr)
        exprs1 += (() => Ident(temp) setType expr.tpe)
      }
    }
    val prefix = vdefs.toList
    val result = within(exprs1.toList)
    if (prefix.isEmpty) result
    else Block(prefix, result) setPos prefix.head.pos
  }
}
