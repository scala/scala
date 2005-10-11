/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */
package scala.reflect;

abstract class Type;

case object NoPrefix extends Type;
case object NoType extends Type;

case class NamedType(fullname: String) extends Type;
case class PrefixedType(pre: Type, sym: Symbol) extends Type;
case class SingleType(pre: Type, sym: Symbol) extends Type;
case class ThisType(clazz: Symbol) extends Type;
case class AppliedType(tpe: Type, args: List[Type]) extends Type;
case class TypeBounds(lo: Type, hi: Type) extends Type;
case class MethodType(formals: List[Type], restpe: Type) extends Type;
class ImplicitMethodType(formals: List[Type], restpe: Type) extends MethodType(formals, restpe);
