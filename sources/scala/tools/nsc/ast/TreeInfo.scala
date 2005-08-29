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
       | ModuleDef(_, _, _)
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

  /** Is tree legal as a member definition of an interface?
   */
  def isInterfaceMember(tree: Tree): boolean = tree match {
    case EmptyTree                     => true
    case Import(_, _)                  => true
    case AbsTypeDef(_, _, _, _)        => true
    case AliasTypeDef(_, _, _, _)      => true
    case DefDef(mods, _, _, _, _, __)  => (mods & DEFERRED) != 0
    case ValDef(mods, _, _, _)         => (mods & DEFERRED) != 0
    case DocDef(_, definition)         => isInterfaceMember(definition)
    case Attributed(_, definition)     => isInterfaceMember(definition)
    case _ => false
  }


  /** Is tree a pure definition?
   */
  def isPureDef(tree: Tree): boolean = tree match {
    case EmptyTree
       | ClassDef(_, _, _, _, _)
       | ModuleDef(_, _, _)
       | AbsTypeDef(_, _, _, _)
       | AliasTypeDef(_, _, _, _)
       | Import(_, _)
       | DefDef(_, _, _, _, _, _) =>
      true
    case ValDef(mods, _, _, rhs) =>
      (mods & MUTABLE) == 0 && isPureExpr(rhs)
    case DocDef(_, definition) =>
      isPureDef(definition)
    case Attributed(_, definition) =>
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
    case Block(stats, expr) =>
      (stats forall isPureDef) && isPureExpr(expr)
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

  /** The longest statement suffix that starts with a constructor */
  def firstConstructor(stats: List[Tree]): Tree = stats.head match {
    case constr @ DefDef(_, nme.CONSTRUCTOR, _, _, _, _) => constr
    case _ => firstConstructor(stats.tail)
  }

  /** Is name a left-associative operator? */
  def isLeftAssoc(operator: Name): boolean =
    operator.length > 0 && operator(operator.length - 1) != ':';

  /** Is name a variable name? */
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

  /** Is this pattern node a catch-all (wildcard or variable) pattern? */
  def isDefaultCase(cdef: CaseDef) = cdef match {
    case CaseDef(Ident(nme.WILDCARD), EmptyTree, _) => true
    case CaseDef(Bind(_, Ident(nme.WILDCARD)), EmptyTree, _) => true
    case _ => false
  }

  /** Is this pattern node a sequence-valued pattern? */
  def isSequenceValued(tree: Tree): boolean = tree match {
    case Bind(_, body) => isSequenceValued(body)
    case Sequence(_) => true
    case ArrayValue(_, _) => true
    case Star(_) => true
    case Alternative(ts) => ts exists isSequenceValued
    case _ => false
  }

  /** The method part of an application node
   */
  def methPart(tree: Tree): Tree = tree match {
    case Apply(fn, _) => methPart(fn)
    case TypeApply(fn, _) => methPart(fn)
    case AppliedTypeTree(fn, _) => methPart(fn)
    case _ => tree
  }
}
