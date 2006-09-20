/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.reflect

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
