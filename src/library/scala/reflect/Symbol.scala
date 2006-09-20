/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.reflect


abstract class Symbol {
  val owner: Symbol
  val name: String
  val tpe: Type
}

abstract class GlobalSymbol(val fullname: String) extends Symbol {
  private val pointIndex = fullname.lastIndexOf(".")
  val owner: Symbol =
    if (pointIndex < 0) RootSymbol
    else Class(fullname.substring(0, pointIndex))
  val name: String =
    if (pointIndex < 0) fullname
    else fullname.substring(pointIndex, fullname.length())
}

abstract class LocalSymbol extends Symbol

case class Class(override val fullname: String) extends GlobalSymbol(fullname) {
  val tpe = NamedType(fullname)
}

case class Method(override val fullname: String, tpe: Type) extends GlobalSymbol(fullname)

case class Field(override val fullname: String, tpe: Type) extends GlobalSymbol(fullname)

case class TypeField(override val fullname: String, tpe: Type) extends GlobalSymbol(fullname)

case class LocalValue(owner: Symbol, name: String, tpe: Type) extends LocalSymbol

case class LocalMethod(owner: Symbol, name: String, tpe: Type) extends LocalSymbol

case object NoSymbol extends Symbol {
  val owner = null
  val name = null
  val tpe = NoType
}

case object RootSymbol extends Symbol {
  val owner = NoSymbol
  val name = "<root>"
  val tpe = NoPrefix
}

case class LabelSymbol(val name: String) extends Symbol {
  val owner = NoSymbol
  val tpe = NamedType("scala.Unit")
}
