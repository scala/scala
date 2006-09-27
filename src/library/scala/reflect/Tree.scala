/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.reflect

/**
 *  <dl>
 *    <dt><b>Direct Known Subclasses:</b></dt>
 *    <dd>
 *      <a href="Apply.html" target="contentFrame">Apply</a>,
 *      <a href="Assign.html" target="contentFrame">Assign</a>,
 *      <a href="Block.html" target="contentFrame">Block</a>,
 *      <a href="ClassDef.html" target="contentFrame">ClassDef</a>,
 *      <a href="DefDef.html" target="contentFrame">DefDef</a>,
 *      <a href="Function.html" target="contentFrame">Function</a>,
 *      <a href="Goto.html" target="contentFrame">Goto</a>,
 *      <a href="Ident.html" target="contentFrame">Ident</a>,
 *      <a href="If.html" target="contentFrame">If</a>,
 *      <a href="Literal.html" target="contentFrame">Literal</a>,
 *      <a href="Super.html" target="contentFrame">Super</a>,
 *      <a href="ValDef.html" target="contentFrame">ValDef</a>
 *   </dd>
 *  </dl>
 */
abstract class Tree

case class Ident(sym: Symbol) extends Tree
case class Select(qual: Tree, sym: Symbol) extends Tree
case class Literal(value: Any) extends Tree
case class Apply(fun: Tree, args: List[Tree]) extends Tree
case class TypeApply(fun: Tree, args: List[Type]) extends Tree
case class Function(params: List[Symbol], body: Tree) extends Tree
case class This(sym: Symbol) extends Tree
case class Block(stats: List[Tree], expr: Tree) extends Tree
case class New(sym: Tree) extends Tree
case class If(condition: Tree, trueCase: Tree, falseCase: Tree) extends Tree
case class Assign(destination: Tree, source: Tree) extends Tree
case class Target(sym: LabelSymbol, body: Tree) extends Tree
case class Goto(target: LabelSymbol) extends Tree
case class ValDef(sym: Symbol, rhs: Tree) extends Tree

//Monomorphic
case class ClassDef(sym: Symbol, tpe: Type, impl: Template) extends Tree
case class DefDef(sym: Symbol, vparamss: List[List[Tree]], ret: Type, rhs: Tree) extends Tree
case class Super(psym: Symbol) extends Tree
case class Template(parents: List[Type], body: List[Tree]) extends Tree
