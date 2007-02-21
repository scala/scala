/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc.ast

import symtab.Flags._
import util.{Set, HashSet}

/** This class ...
 *
 *  @author Martin Odersky
 *  @version 1.0
 */
abstract class TreeInfo {

  val global: Global
  import global._

  def isTerm(tree: Tree): boolean = tree.isTerm
  def isType(tree: Tree): boolean = tree.isType

  def isOwnerDefinition(tree: Tree): boolean = tree match {
    case PackageDef(_, _)
       | ClassDef(_, _, _, _, _)
       | ModuleDef(_, _, _)
       | DefDef(_, _, _, _, _, _)
       | Import(_, _) => true
    case _ => false
  }

  def isDefinition(tree: Tree): boolean = tree.isDef

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
    case DefDef(mods, _, _, _, _, __)  => mods.hasFlag(DEFERRED)
    case ValDef(mods, _, _, _)         => mods.hasFlag(DEFERRED)
    case DocDef(_, definition)         => isInterfaceMember(definition)
    case _ => false
  }

  /** Is tree a pure (i.e. non-side-effecting) definition?
   */
  def isPureDef(tree: Tree): boolean = tree match {
    case EmptyTree
       | ClassDef(_, _, _, _, _)
       | AbsTypeDef(_, _, _, _)
       | AliasTypeDef(_, _, _, _)
       | Import(_, _)
       | DefDef(_, _, _, _, _, _) =>
      true
    case ValDef(mods, _, _, rhs) =>
      !mods.hasFlag(MUTABLE) && isPureExpr(rhs)
    case DocDef(_, definition) =>
      isPureDef(definition)
    case _ =>
      false
  }

  /** Is tree a stable and pure expression?
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

  def mayBeVarGetter(sym: Symbol) = sym.info match {
    case PolyType(List(), _) => sym.owner.isClass && !sym.isStable
    case _: ImplicitMethodType => sym.owner.isClass && !sym.isStable
    case _ => false
  }

  def isVariableOrGetter(tree: Tree) = tree match {
    case Ident(_) =>
      tree.symbol.isVariable
    case Select(qual, _) =>
      tree.symbol.isVariable ||
      (mayBeVarGetter(tree.symbol) &&
       tree.symbol.owner.info.decl(nme.getterToSetter(tree.symbol.name)) != NoSymbol)
    case Apply(Select(qual, nme.apply), _) =>
      qual.tpe.decl(nme.update) != NoSymbol
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
    case Ident(name) => isVariableName(name) && !pat.isInstanceOf[BackQuotedIdent]
    case _ => false
  }

  /** The first constructor definitions in `stats' */
  def firstConstructor(stats: List[Tree]): Tree = stats match {
    case List() => EmptyTree
    case (constr @ DefDef(_, nme.CONSTRUCTOR, _, _, _, _)) :: _ => constr
    case _ :: stats1 => firstConstructor(stats1)
  }
/*
  /** The super call that calls mixin `mix' in stats */
  def superCall(stats: List[Tree], mix: Name): Tree = stats match {
    case scall @ Apply(Select(Super(_, mix1), name), List()) :: _
    if ((name == nme.CONSTRUCTOR || name == nme.MIXIN_CONSTRUCTOR) && mix1 == mix) =>
      scall
    case _ :: stats1 =>
      superCall(stats1, name)
    case _ =>
      assert(false, "no supercall to " + mix + " in " + stats)
  }
*/
  /** Is name a left-associative operator? */
  def isLeftAssoc(operator: Name): boolean =
    operator.length > 0 && operator(operator.length - 1) != ':';

  private val reserved = new HashSet[Name]
  reserved addEntry nme.false_
  reserved addEntry nme.true_
  reserved addEntry nme.null_
  reserved addEntry newTypeName("byte")
  reserved addEntry newTypeName("char")
  reserved addEntry newTypeName("short")
  reserved addEntry newTypeName("int")
  reserved addEntry newTypeName("long")
  reserved addEntry newTypeName("float")
  reserved addEntry newTypeName("double")
  reserved addEntry newTypeName("boolean")
  reserved addEntry newTypeName("unit")

  /** Is name a variable name? */
  def isVariableName(name: Name): boolean = {
    val first = name(0)
    (('a' <= first && first <= 'z') || first == '_') && !(reserved contains name)
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

  /** Is this pattern node a catch-all or type-test pattern? */
  def isCatchCase(cdef: CaseDef) = cdef match {
    case CaseDef(Typed(Ident(nme.WILDCARD), tpt), EmptyTree, _) => isSimple(tpt.tpe)
    case CaseDef(Bind(_, Typed(Ident(nme.WILDCARD), tpt)), EmptyTree, _) => isSimple(tpt.tpe)
    case _ => isDefaultCase(cdef)
  }

  private def isSimple(tp: Type): boolean = true
  /* If we have run-time types, and these are used for pattern matching,
     we should replace this  by something like:

      tp match {
        case TypeRef(pre, sym, args) =>
          args.isEmpty && (sym.owner.isPackageClass || isSimple(pre))
        case NoPrefix =>
          true
        case _ =>
          false
      }
*/

  /** Is this pattern node a sequence-valued pattern? */
  def isSequenceValued(tree: Tree): boolean = tree match {
    case Bind(_, body) => isSequenceValued(body)
    case Sequence(_) => true
    case ArrayValue(_, _) => true
    case Star(_) => true
    case Alternative(ts) => ts exists isSequenceValued
    case _ => false
  }

  /** is this pattern of the form S(...) where S is a subclass of Seq
   *  and S is not a case class?  The pattern might be wrapped in binds or alternatives.
   */
  def isSequencePattern(tree: Tree): boolean = tree match {
    case Apply(fn, _) =>
      (fn.symbol ne null) &&
      !fn.symbol.hasFlag(CASE) &&
      fn.symbol.isNonBottomSubClass(definitions.SeqClass)
    case Bind(name, body) =>
      isSequencePattern(body)
    case Alternative(ts) =>
      ts forall isSequencePattern
    case _ =>
      false
  }

  /** The method part of an application node
   */
  def methPart(tree: Tree): Tree = tree match {
    case Apply(fn, _) => methPart(fn)
    case TypeApply(fn, _) => methPart(fn)
    case AppliedTypeTree(fn, _) => methPart(fn)
    case _ => tree
  }

  /** Top-level definition sequence contains a leading import of Predef or scala.Predef
   */
  def containsLeadingPredefImport(defs: List[Tree]): boolean = defs match {
    case List(PackageDef(_, defs1)) =>
      containsLeadingPredefImport(defs1)
    case Import(Ident(nme.Predef), _) :: _ =>
      true
    case Import(Select(Ident(nme.scala_), nme.Predef), _) :: _ =>
      true
    case Import(_, _) :: defs1 =>
      containsLeadingPredefImport(defs1)
    case _ =>
      false
  }
}
