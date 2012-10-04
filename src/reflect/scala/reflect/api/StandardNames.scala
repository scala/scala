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

/** A slice of [[scala.reflect.api.Universe the Scala reflection cake]] that defines standard names.
 *
 *  Standard names are names that are essential to creating trees or to reflecting Scala artifacts.
 *  For example, `CONSTRUCTOR` (aka `<init>` on JVM) is necessary to create and invoke constructors.
 *
 *  These standard names can be referred to using `nme` for term names (listed in [[scala.reflect.api.StandardNames#TermNamesApi]])
 *  and using `tpnme` for type names (listed in [[scala.reflect.api.StandardNames#TypeNamesApi]])
 */
trait StandardNames {
  self: Universe =>

  /** A value containing all standard term names. */
  val nme: TermNamesApi

  /** A value containing all standard type names. */
  val tpnme: TypeNamesApi

  /** Defines standard names, common for term and type names. */
  trait NamesApi {
    /** An abstract type that represents the exact flavor of the name. */
    type NameType >: Null <: Name

    /** The term or type name `_`.
     *  Used to construct trees that correspond to underscores in Scala.
     */
    val WILDCARD: NameType

    /** The term or type name corresponding to an empty string.
     *  Represents an empty name, used to denote the fact that no name was specified
     *  for `privateWithin` in [[scala.reflect.api.Trees#Modifiers]], for [[scala.reflect.api.Trees#This]],
     *  for [[scala.reflect.api.Trees#Super]], etc.
     */
    val EMPTY: NameType

    /** The term or type name `<error>`.
     *  Indicates that the enclosing tree or symbol contains a compilation error.
     */
    val ERROR: NameType

    /** The term or type name `package`.
     *  Used to get modules representing package objects.
     */
    val PACKAGE: NameType
  }

  /** Defines standard term names. */
  trait TermNamesApi extends NamesApi {
    /** @inheritdoc */
    type NameType = TermName

    /** The term name `<init>`.
     *  Represents the constructor name on the JVM.
     */
    val CONSTRUCTOR: NameType

    /** The term name `_root_`.
     *  Represents the root package.
     */
    val ROOTPKG: NameType

    /** The string " " (a single whitespace).
     *  `LOCAL_SUFFIX_STRING` is appended to the names of local identifiers,
     *  when it's necessary to prevent a naming conflict. For example, underlying fields
     *  of non-private vals and vars are renamed using `LOCAL_SUFFIX_STRING`.
     */
    val LOCAL_SUFFIX_STRING: String
  }

  /** Defines standard type names. */
  trait TypeNamesApi extends NamesApi {
    /** @inheritdoc */
    type NameType = TypeName

    /** The type name `_*`.
     *  Used to construct types that specify sequence arguments to repeated parameters.
     */
    val WILDCARD_STAR: NameType
  }
}
