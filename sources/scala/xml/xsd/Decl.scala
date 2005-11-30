/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2005, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.xml.xsd ;

abstract class Decl ;

/** name     - label of the element
 *  typeName - reference to a (possibly generated) type name
 */
case class ElemDecl(name: String, tpe: TypeSymbol) ;

case class ComplexTypeDecl(name: String, derivedFrom: DerivSym, contentModel: ContentModel) ;

case class SimpleTypeDecl(name: String);

abstract class xsdBuiltin(name: String) extends SimpleTypeDecl(name);
