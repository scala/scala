/* NSC -- new Scala compiler
* Copyright 2005-2011 LAMP/EPFL
* @author  Martin Odersky
*/

package scala.reflect
package base

trait StandardNames {
  self: Universe =>

  val nme: TermNamesBase
  val tpnme: TypeNamesBase

  trait NamesBase {
    type NameType >: Null <: Name
    val EMPTY: NameType
    val ROOT: NameType
    val EMPTY_PACKAGE_NAME: NameType
    val WILDCARD: NameType
  }

  trait TypeNamesBase extends NamesBase

  trait TermNamesBase extends NamesBase {
    val CONSTRUCTOR: TermName
    val NO_NAME: NameType
  }
}
