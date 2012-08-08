/* NSC -- new Scala compiler
* Copyright 2005-2012 LAMP/EPFL
* @author  Martin Odersky
*/
package scala.reflect
package api

// Q: I have a pretty name. Where do I put it - into base.StandardNames or into api.StandardNames?
// A: <see base.StandardNames>

trait StandardNames extends base.StandardNames {
  self: Universe =>

  val nme: TermNamesApi
  val tpnme: TypeNamesApi

  trait NamesApi extends NamesBase {
    val ROOT: NameType
    val EMPTY: NameType
    val ERROR: NameType
    val PACKAGE: NameType
  }

  trait TermNamesApi extends NamesApi with TermNamesBase {
    val LOCAL_SUFFIX_STRING: String
  }

  trait TypeNamesApi extends NamesApi with TypeNamesBase {
  }
}
