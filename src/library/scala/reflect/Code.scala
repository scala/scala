/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2005, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.reflect;

abstract class Code;

case class Ident(sym: Symbol) extends Code;
case class Select(qual: Code, sym: Symbol) extends Code;
case class Literal(value: Any) extends Code;
case class Apply(fun: Code, args: List[Code]) extends Code;
case class TypeApply(fun: Code, args: List[Type]) extends Code;
case class Function(params: List[Symbol], body: Code) extends Code;
case class This(sym: Symbol) extends Code;
case class Block(stats: List[Code], expr: Code) extends Code;