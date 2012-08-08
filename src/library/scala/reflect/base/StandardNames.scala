/* NSC -- new Scala compiler
* Copyright 2005-2012 LAMP/EPFL
* @author  Martin Odersky
*/

package scala.reflect
package base

// Q: I have a pretty name. Where do I put it - into base.StandardNames or into api.StandardNames?
// A: Is it necessary to construct trees (like EMPTY or WILDCARD_STAR)? If yes, then it goes to base.StandardNames.
//    Is it necessary to perform reflection (like ERROR or LOCAL_SUFFIX_STRING)? If yes, then it goes to api.StandardNames.
//    Otherwise it goes nowhere - reflection API should stay minimalistic.

trait StandardNames {
  self: Universe =>

  val nme: TermNamesBase
  val tpnme: TypeNamesBase

  trait NamesBase {
    type NameType >: Null <: Name
    val WILDCARD: NameType
  }

  trait TermNamesBase extends NamesBase {
    val CONSTRUCTOR: TermName
    val ROOTPKG: TermName
  }

  trait TypeNamesBase extends NamesBase {
    val EMPTY: NameType
    val WILDCARD_STAR: NameType
  }
}
