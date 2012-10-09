/* NSC -- new Scala compiler
* Copyright 2005-2012 LAMP/EPFL
* @author  Martin Odersky
*/
package scala.reflect
package api

// Q: I have a pretty name. Can I put it here?
// A: Is it necessary to construct trees (like EMPTY or WILDCARD_STAR)? If yes, then sure.
//    Is it necessary to perform reflection (like ERROR or LOCAL_SUFFIX_STRING)? If yes, then sure.
//    Otherwise you'd better not - reflection API should stay minimalistic.

// TODO: document better
/**
 * Names necessary to create Scala trees.
 */
trait StandardNames {
  self: Universe =>

  val nme: TermNamesApi
  val tpnme: TypeNamesApi

  trait NamesApi {
    type NameType >: Null <: Name
    val WILDCARD: NameType
    val EMPTY: NameType
    val ERROR: NameType
    val PACKAGE: NameType
  }

  trait TermNamesApi extends NamesApi {
    type NameType = TermName
    val CONSTRUCTOR: NameType
    val ROOTPKG: NameType
    val LOCAL_SUFFIX_STRING: String
  }

  trait TypeNamesApi extends NamesApi {
    type NameType = TypeName
    val WILDCARD_STAR: NameType
  }
}
