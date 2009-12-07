/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.reflect

/** This type is required by the compiler and <b>should not be used in client code</b>. */
abstract class Symbol {
  val owner: Symbol
  val name: String
  val tpe: Type
}

/** This type is required by the compiler and <b>should not be used in client code</b>. */
abstract class GlobalSymbol(val fullname: String) extends Symbol {
  private val pointIndex = fullname.lastIndexOf(".")
  val owner: Symbol =
    if (pointIndex < 0) RootSymbol
    else Class(fullname.substring(0, pointIndex))
  val name: String =
    if (pointIndex < 0) fullname
    else fullname.substring(pointIndex+1, fullname.length())
}

/** This type is required by the compiler and <b>should not be used in client code</b>. */
abstract class LocalSymbol extends Symbol

/** This type is required by the compiler and <b>should not be used in client code</b>. */
case class Class(override val fullname: String) extends GlobalSymbol(fullname) {
  val tpe = NamedType(fullname)
}

/** This type is required by the compiler and <b>should not be used in client code</b>. */
case class Method(override val fullname: String, tpe: Type) extends GlobalSymbol(fullname)

/** This type is required by the compiler and <b>should not be used in client code</b>. */
case class Field(override val fullname: String, tpe: Type) extends GlobalSymbol(fullname)

/** This type is required by the compiler and <b>should not be used in client code</b>. */
case class TypeField(override val fullname: String, tpe: Type) extends GlobalSymbol(fullname)

/** This type is required by the compiler and <b>should not be used in client code</b>. */case class LocalValue(owner: Symbol, name: String, tpe: Type) extends LocalSymbol

/** This type is required by the compiler and <b>should not be used in client code</b>. */
case class LocalMethod(owner: Symbol, name: String, tpe: Type) extends LocalSymbol

/** This type is required by the compiler and <b>should not be used in client code</b>. */
case object NoSymbol extends Symbol {
  val owner = null
  val name = null
  val tpe = NoType
}

/** This type is required by the compiler and <b>should not be used in client code</b>. */
case object RootSymbol extends Symbol {
  val owner = NoSymbol
  val name = "<root>"
  val tpe = NoPrefix
}

/** This type is required by the compiler and <b>should not be used in client code</b>. */
case class LabelSymbol(val name: String) extends Symbol {
  val owner = NoSymbol
  val tpe = NamedType("scala.Unit")
}


/* Standard pattern match:

    case reflect.Class(fullname) =>
    case reflect.Method(fullname, tpe) =>
    case reflect.Field(fullname, tpe) =>
    case reflect.TypeField(fullname, tpe) =>
    case reflect.LocalValue(owner, name, tpe) =>
    case reflect.LocalMethod(owner, name, tpe) =>
    case reflect.NoSymbol =>
    case reflect.RootSymbol =>
    case reflect.LabelSymbol(name) =>
*/
