/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2005, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $id: $


package scala.xml.xsd;


abstract class XsTypeSymbol extends xml.TypeSymbol;

object xsdAny extends XsTypeSymbol ;

class SimpleTypeSymbol(val name: String) extends  XsTypeSymbol {
  var decl: SimpleTypeDecl = null;
}

class ComplexTypeSymbol(val name: String) extends XsTypeSymbol {
  var decl: ComplexTypeDecl = null;
}

abstract class  DerivSym;

case class Extends(sym:XsTypeSymbol) extends DerivSym;

case class Restricts(sym:XsTypeSymbol) extends DerivSym;

object xsBoolean extends SimpleTypeSymbol("boolean") {}
object xsDouble  extends SimpleTypeSymbol("double") {}
object xsFloat   extends SimpleTypeSymbol("float") {}
object xsInt     extends SimpleTypeSymbol("int") {}
object xsLong    extends SimpleTypeSymbol("long") {}
object xsShort   extends SimpleTypeSymbol("short") {}
object xsString  extends SimpleTypeSymbol("string") {}
object xsDate    extends SimpleTypeSymbol("date") {}
