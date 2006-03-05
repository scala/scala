/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.xml.xsd;


abstract class Decl;

/** name     - label of the element
 *  typeName - reference to a (possibly generated) type name
 */
case class ElemDecl(name: String, tpe: TypeSymbol) extends Decl;

abstract class TypeDecl ;

case class ComplexTypeDecl(name: String, derivedFrom: DerivSym, contentModel: ContentModel) extends TypeDecl;

case class SimpleTypeDecl(name: String) extends TypeDecl;;

abstract class xsdBuiltin(name: String) extends SimpleTypeDecl(name);
