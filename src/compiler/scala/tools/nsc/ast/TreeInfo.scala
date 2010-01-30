/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc
package ast

import symtab.Flags._
import symtab.SymbolTable
import util.HashSet

/** This class ...
 *
 *  @author Martin Odersky
 *  @version 1.0
 */
abstract class TreeInfo {

  val trees: SymbolTable
  import trees._
  import definitions.ThrowableClass

  def isTerm(tree: Tree): Boolean = tree.isTerm
  def isType(tree: Tree): Boolean = tree.isType

  def isOwnerDefinition(tree: Tree): Boolean = tree match {
    case PackageDef(_, _)
       | ClassDef(_, _, _, _)
       | ModuleDef(_, _, _)
       | DefDef(_, _, _, _, _, _)
       | Import(_, _) => true
    case _ => false
  }

  def isDefinition(tree: Tree): Boolean = tree.isDef

  def isDeclaration(tree: Tree): Boolean = tree match {
    case DefDef(_, _, _, _, _, EmptyTree)
       | ValDef(_, _, _, EmptyTree)
       | TypeDef(_, _, _, _) => true
    case _ => false
  }

  /** Is tree legal as a member definition of an interface?
   */
  def isInterfaceMember(tree: Tree): Boolean = tree match {
    case EmptyTree                     => true
    case Import(_, _)                  => true
    case TypeDef(_, _, _, _)           => true
    case DefDef(mods, _, _, _, _, __)  => mods.hasFlag(DEFERRED)
    case ValDef(mods, _, _, _)         => mods.hasFlag(DEFERRED)
    case DocDef(_, definition)         => isInterfaceMember(definition)
    case _ => false
  }

  /** Is tree a pure (i.e. non-side-effecting) definition?
   */
  def isPureDef(tree: Tree): Boolean = tree match {
    case EmptyTree
       | ClassDef(_, _, _, _)
       | TypeDef(_, _, _, _)
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
  def isPureExpr(tree: Tree): Boolean = tree match {
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
       tree.symbol.owner.info.member(nme.getterToSetter(tree.symbol.name)) != NoSymbol)
    case Apply(Select(qual, nme.apply), _) =>
      qual.tpe.member(nme.update) != NoSymbol
    case _ =>
      false
  }

  /** Is tree a self constructor call?
   */
  def isSelfConstrCall(tree: Tree): Boolean = methPart(tree) match {
    case Ident(nme.CONSTRUCTOR)
       | Select(This(_), nme.CONSTRUCTOR) => true
    case _ => false
  }

  def isSuperConstrCall(tree: Tree): Boolean = methPart(tree) match {
    case Select(Super(_, _), nme.CONSTRUCTOR) => true
    case _ => false
  }

  def isSelfOrSuperConstrCall(tree: Tree): Boolean = methPart(tree) match {
    case Ident(nme.CONSTRUCTOR)
       | Select(This(_), nme.CONSTRUCTOR)
       | Select(Super(_, _), nme.CONSTRUCTOR) => true
    case _ => false
  }

  /** Is tree a variable pattern */
  def isVarPattern(pat: Tree): Boolean = pat match {
    case Ident(name) => isVariableName(name) && !pat.isInstanceOf[BackQuotedIdent]
    case _ => false
  }

  /** The first constructor definitions in `stats' */
  def firstConstructor(stats: List[Tree]): Tree = stats match {
    case List() => EmptyTree
    case (constr @ DefDef(_, name, _, _, _, _)) :: _
    if (name == nme.CONSTRUCTOR || name == nme.MIXIN_CONSTRUCTOR) => constr
    case _ :: stats1 => firstConstructor(stats1)
  }

  /** The value definitions marked PRESUPER in this statement sequence */
  def preSuperFields(stats: List[Tree]): List[ValDef] =
    for (vdef @ ValDef(mods, _, _, _) <- stats if mods hasFlag PRESUPER) yield vdef

  def isEarlyDef(tree: Tree) = tree match {
    case TypeDef(mods, _, _, _) => mods hasFlag PRESUPER
    case ValDef(mods, _, _, _) => mods hasFlag PRESUPER
    case _ => false
  }

  def isEarlyValDef(tree: Tree) = tree match {
    case ValDef(mods, _, _, _) => mods hasFlag PRESUPER
    case _ => false
  }

  def isEarlyTypeDef(tree: Tree) = tree match {
    case TypeDef(mods, _, _, _) => mods hasFlag PRESUPER
    case _ => false
  }

  /** Is type a of the form T* ? */
  def isRepeatedParamType(tpt: Tree) = tpt match {
    case AppliedTypeTree(Select(_, rp), _) =>
      rp == nme.REPEATED_PARAM_CLASS_NAME.toTypeName ||
      rp == nme.JAVA_REPEATED_PARAM_CLASS_NAME.toTypeName
    case TypeTree() => definitions.isRepeatedParamType(tpt.tpe)
    case _ => false
  }

  /** Is tpt a by-name parameter type? */
  def isByNameParamType(tpt: Tree) = tpt match {
    case AppliedTypeTree(Select(_, n), _) => n == nme.BYNAME_PARAM_CLASS_NAME.toTypeName
    case TypeTree() => tpt.tpe.typeSymbol == definitions.ByNameParamClass
    case _ => false
  }

  /** Is name a left-associative operator? */
  def isLeftAssoc(operator: Name): Boolean =
    operator.length > 0 && operator(operator.length - 1) != ':'

  private val reserved = new HashSet[Name]("reserved", 64)
  reserved addEntry nme.false_
  reserved addEntry nme.true_
  reserved addEntry nme.null_

  /** Is name a variable name? */
  def isVariableName(name: Name): Boolean = {
    val first = name(0)
    (('a' <= first && first <= 'z') || first == '_') && !(reserved contains name)
  }

  /** Is tree a this node which belongs to `enclClass'? */
  def isSelf(tree: Tree, enclClass: Symbol): Boolean = tree match {
    case This(_) => tree.symbol == enclClass
    case _ => false
  }

  /** can this type be a type pattern */
  def mayBeTypePat(tree: Tree): Boolean = tree match {
    case CompoundTypeTree(Template(tps, _, List())) => tps exists mayBeTypePat
    case Annotated(_, tp) => mayBeTypePat(tp)
    case AppliedTypeTree(constr, args) =>
      mayBeTypePat(constr) || args.exists(_.isInstanceOf[Bind])
    case SelectFromTypeTree(tp, _) => mayBeTypePat(tp)
    case _ => false
  }

  /** Is this argument node of the form <expr> : _* ?
   */
  def isWildcardStarArg(tree: Tree): Boolean = tree match {
    case Typed(expr, Ident(name)) => name == nme.WILDCARD_STAR.toTypeName
    case _ => false
  }

  /** Is this pattern node a catch-all (wildcard or variable) pattern? */
  def isDefaultCase(cdef: CaseDef) = cdef match {
    case CaseDef(Ident(nme.WILDCARD), EmptyTree, _) => true
    case CaseDef(Bind(_, Ident(nme.WILDCARD)), EmptyTree, _) => true
    case _ => false
  }

  /** Does this CaseDef catch Throwable? */
  def catchesThrowable(cdef: CaseDef) = catchesAllOf(cdef, ThrowableClass.tpe)

  /** Does this CaseDef catch everything of a certain Type? */
  def catchesAllOf(cdef: CaseDef, threshold: Type) =
    isDefaultCase(cdef) || (cdef.guard.isEmpty && (unbind(cdef.pat) match {
      case Typed(Ident(nme.WILDCARD), tpt)  => (tpt.tpe != null) && (threshold <:< tpt.tpe)
      case _                                => false
    }))

  /** Is this pattern node a catch-all or type-test pattern? */
  def isCatchCase(cdef: CaseDef) = cdef match {
    case CaseDef(Typed(Ident(nme.WILDCARD), tpt), EmptyTree, _) =>
      isSimpleThrowable(tpt.tpe)
    case CaseDef(Bind(_, Typed(Ident(nme.WILDCARD), tpt)), EmptyTree, _) =>
      isSimpleThrowable(tpt.tpe)
    case _ =>
      isDefaultCase(cdef)
  }

  private def isSimpleThrowable(tp: Type): Boolean = tp match {
    case TypeRef(pre, sym, args) =>
      (pre == NoPrefix || pre.widen.typeSymbol.isStatic) &&
      (sym isNonBottomSubClass ThrowableClass) &&  /* bq */ !sym.isTrait
    case _ =>
      false
  }

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
  def isSequenceValued(tree: Tree): Boolean = tree match {
    case Bind(_, body) => isSequenceValued(body)
    case ArrayValue(_, _) => true
    case Star(_) => true
    case Alternative(ts) => ts exists isSequenceValued
    case _ => false
  }

  /** The underlying pattern ignoring any bindings */
  def unbind(x: Tree): Tree = x match {
    case Bind(_, y) => unbind(y)
    case y          => y
  }

  /** Is this tree a Star(_) after removing bindings? */
  def isStar(x: Tree) = unbind(x) match {
    case Star(_)  => true
    case _        => false
  }

  /** The method part of an application node
   */
  def methPart(tree: Tree): Tree = tree match {
    case Apply(fn, _) => methPart(fn)
    case TypeApply(fn, _) => methPart(fn)
    case AppliedTypeTree(fn, _) => methPart(fn)
    case _ => tree
  }

  def firstArgument(tree: Tree): Tree = tree match {
    case Apply(fn, args) =>
      val f = firstArgument(fn)
      if (f == EmptyTree && !args.isEmpty) args.head else f
    case _ =>
      EmptyTree
  }

  /** Top-level definition sequence contains a leading import of
   *  <code>Predef</code> or <code>scala.Predef</code>.
   */
  def containsLeadingPredefImport(defs: List[Tree]): Boolean = defs match {
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

  /** Compilation unit is the predef object
   */
  def isUnitInScala(tree: Tree, name: Name) = tree match {
    case PackageDef(Ident(nme.scala_), defs) => isObject(defs, name)
    case _ => false
  }

  private def isObject(trees: List[Tree], name: Name): Boolean = trees match {
    case Import(_, _) :: xs => isObject(xs, name)
    case DocDef(_, tree1) :: Nil => isObject(List(tree1), name)
    case Annotated(_, tree1) :: Nil => isObject(List(tree1), name)
    case ModuleDef(_, `name`, _) :: Nil => true
    case _ => false
  }

  def isAbsTypeDef(tree: Tree) = tree match {
    case TypeDef(_, _, _, TypeBoundsTree(_, _)) => true
    case TypeDef(_, _, _, rhs) => rhs.tpe.isInstanceOf[TypeBounds]
    case _ => false
  }

  def isAliasTypeDef(tree: Tree) = tree match {
    case TypeDef(_, _, _, _) => !isAbsTypeDef(tree)
    case _ => false
  }

  /** Some handy extractors for spotting true and false expressions
   *  through the haze of braces.
   */
  abstract class SeeThroughBlocks[T] {
    protected def unapplyImpl(x: Tree): T
    def unapply(x: Tree): T = x match {
      case Block(Nil, expr)         => unapply(expr)
      case _                        => unapplyImpl(x)
    }
  }
  object IsTrue extends SeeThroughBlocks[Boolean] {
    protected def unapplyImpl(x: Tree): Boolean = x equalsStructure Literal(Constant(true))
  }
  object IsFalse extends SeeThroughBlocks[Boolean] {
    protected def unapplyImpl(x: Tree): Boolean = x equalsStructure Literal(Constant(false))
  }
}
