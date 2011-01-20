/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.reflect

/** This type is required by the compiler and <b>should not be used in client code</b>. */
abstract class Tree

/** This type is required by the compiler and <b>should not be used in client code</b>. */
case class Ident(sym: Symbol) extends Tree
/** This type is required by the compiler and <b>should not be used in client code</b>. */
case class Select(qual: Tree, sym: Symbol) extends Tree
/** This type is required by the compiler and <b>should not be used in client code</b>. */
case class Literal(value: Any) extends Tree
/** This type is required by the compiler and <b>should not be used in client code</b>. */
case class Apply(fun: Tree, args: List[Tree]) extends Tree
/** This type is required by the compiler and <b>should not be used in client code</b>. */
case class TypeApply(fun: Tree, args: List[Type]) extends Tree
/** This type is required by the compiler and <b>should not be used in client code</b>. */
case class Function(params: List[Symbol], body: Tree) extends Tree
/** This type is required by the compiler and <b>should not be used in client code</b>. */
case class This(sym: Symbol) extends Tree
/** This type is required by the compiler and <b>should not be used in client code</b>. */
case class Block(stats: List[Tree], expr: Tree) extends Tree
/** This type is required by the compiler and <b>should not be used in client code</b>. */
case class New(sym: Tree) extends Tree
/** This type is required by the compiler and <b>should not be used in client code</b>. */
case class If(condition: Tree, trueCase: Tree, falseCase: Tree) extends Tree
/** This type is required by the compiler and <b>should not be used in client code</b>. */
case class Assign(destination: Tree, source: Tree) extends Tree
/** This type is required by the compiler and <b>should not be used in client code</b>. */
case class Target(sym: LabelSymbol, body: Tree) extends Tree
/** This type is required by the compiler and <b>should not be used in client code</b>. */
case class Goto(target: LabelSymbol) extends Tree
/** This type is required by the compiler and <b>should not be used in client code</b>. */
case class ValDef(sym: Symbol, rhs: Tree) extends Tree

//Monomorphic
/** This type is required by the compiler and <b>should not be used in client code</b>. */
case class ClassDef(sym: Symbol, tpe: Type, impl: Template) extends Tree
/** This type is required by the compiler and <b>should not be used in client code</b>. */
case class DefDef(sym: Symbol, vparamss: List[List[Tree]], ret: Type, rhs: Tree) extends Tree
/** This type is required by the compiler and <b>should not be used in client code</b>. */
case class Super(psym: Symbol) extends Tree
/** This type is required by the compiler and <b>should not be used in client code</b>. */
case class Template(parents: List[Type], body: List[Tree]) extends Tree
