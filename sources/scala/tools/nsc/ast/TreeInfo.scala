/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc.ast;

import symtab.Flags._;

abstract class TreeInfo {

  val global: Global;
  import global._;

  def isTerm(tree: Tree): boolean = tree.isTerm;
  def isType(tree: Tree): boolean = tree.isType;

  def isOwnerDefinition(tree: Tree): boolean = tree match {
    case PackageDef(_, _)
       | ClassDef(_, _, _, _, _)
       | ModuleDef(_, _, _, _)
       | DefDef(_, _, _, _, _, _)
       | Import(_, _) => true
    case _ => false
  }

  def isDefinition(tree: Tree): boolean = tree.isDef;

  def isDeclaration(tree: Tree): boolean = tree match {
    case DefDef(_, _, _, _, _, EmptyTree)
       | ValDef(_, _, _, EmptyTree)
       | AbsTypeDef(_, _, _, _)
       | AliasTypeDef(_, _, _, _) => true
    case _ => false
  }

  /** Is tree a pure definition?
   */
  def isPureDef(tree: Tree): boolean = tree match {
    case EmptyTree
       | ClassDef(_, _, _, _, _)
       | ModuleDef(_, _, _, _)
       | AbsTypeDef(_, _, _, _)
       | AliasTypeDef(_, _, _, _)
       | Import(_, _)
       | DefDef(_, nme.CONSTRUCTOR, _, _, _, _) =>
      true
    case ValDef(mods, _, _, rhs) =>
      (mods & MUTABLE) == 0 && isPureExpr(rhs)
    case DocDef(_, definition) =>
      isPureDef(definition)
    case _ =>
      false
  }

  /** Is tree a stable & pure expression?
   */
  def isPureExpr(tree: Tree): boolean = tree match {
    case EmptyTree
       | This(_)
       | Super(_, _)
       | Literal(_) =>
      true
    case Ident(_) =>
      tree.symbol.isStable
    case Select(qual, _) =>
      tree.symbol.isStable && isPureExpr(qual)
    case TypeApply(fn, _) =>
      isPureExpr(fn)
    case Apply(fn, List()) =>
      isPureExpr(fn)
    case Typed(expr, _) =>
      isPureExpr(expr)
    case _ =>
      false
  }

  /** Is tree a pure constructor?
   */
  def isPureConstr(tree: Tree): boolean = tree match {
    case Ident(_)
       | Select(_, _) =>
      tree.symbol != null && tree.symbol.isPrimaryConstructor;
    case TypeApply(fn, _) =>
      isPureConstr(fn)
    case Apply(fn, List()) =>
      isPureConstr(fn)
    case _ =>
      false
  }

  /** Is tree a self constructor call?
   */
  def isSelfConstrCall(tree: Tree): boolean = tree match {
    case Ident(nme.CONSTRUCTOR) =>
      true
    case TypeApply(constr, _) =>
      isSelfConstrCall(constr)
    case Apply(constr, _) =>
      isSelfConstrCall(constr)
    case _ =>
      false
  }

  /** Is tree a variable pattern */
  def isVarPattern(pat: Tree): boolean = pat match {
    case Ident(name) => isVariableName(name)
    case _ => false
  }

  /** The first constructor in a list of statements */
  def firstConstructor(stats: List[Tree]): Tree = stats.head match {
    case DefDef(_, nme.CONSTRUCTOR, _, _, _, _) => stats.head
    case _ => firstConstructor(stats.tail)
  }

  /** Is name a variable name */
  def isVariableName(name: Name): boolean = {
    val first = name(0);
    (('a' <= first && first <= 'z') || first == '_')
    && name != nme.false_
    && name != nme.true_
    && name != nme.null_
  }

  /** Is tree a this node which belongs to `enclClass'? */
  def isSelf(tree: Tree, enclClass: Symbol): boolean = tree match {
    case This(_) => tree.symbol == enclClass
    case _ => false
  }

  /** The method symbol of an application node, or NoSymbol, if none exists.
   */
  def methSymbol(tree: Tree): Symbol = {
    val meth = methPart(tree);
    if (meth.hasSymbol) meth.symbol else NoSymbol
  }

  /** The method part of an application node
   */
  def methPart(tree: Tree): Tree = tree match {
    case Apply(fn, _) => methPart(fn)
    case TypeApply(fn, _) => methPart(fn)
    case AppliedTypeTree(fn, _) => methPart(fn)
    case _ => tree
  }

  /** Is name imported explicitly, not via wildcard? */
  def isExplicitImport(tree: Import, name: Name): boolean =
    tree.selectors exists (._2.==(name.toTermName));

  /** The symbol with name `name' imported from import clause `tree'.
   */
  def importedSymbol(tree: Import, name: Name): Symbol = {
    var result: Symbol = NoSymbol;
    var renamed = false;
    var selectors = tree.selectors;
    while (selectors != Nil && result == NoSymbol) {
      if (selectors.head._2 == name.toTermName)
	result = tree.expr.tpe.member(
          if (name.isTypeName) selectors.head._1.toTypeName else selectors.head._1);
      else if (selectors.head._1 == name.toTermName)
        renamed = true
      else if (selectors.head._1 == nme.WILDCARD && !renamed)
        result = tree.expr.tpe.member(name);
      selectors = selectors.tail
    }
    result
  }
}
