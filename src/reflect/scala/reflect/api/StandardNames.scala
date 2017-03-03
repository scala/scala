/* NSC -- new Scala compiler
* Copyright 2005-2013 LAMP/EPFL
* @author  Martin Odersky
*/
package scala
package reflect
package api

// Q: I have a pretty name. Can I put it here?
// A: Is it necessary to construct trees (like EMPTY or WILDCARD_STAR)? If yes, then sure.
//    Is it necessary to perform reflection (like ERROR or LOCAL_SUFFIX_STRING)? If yes, then sure.
//    Otherwise you'd better not - reflection API should stay minimalistic.

/**
 * <span class="badge badge-red" style="float: right;">EXPERIMENTAL</span>
 *
 *  Standard names are names that are essential to creating trees or to reflecting Scala artifacts.
 *  For example, `CONSTRUCTOR` (aka `<init>` on JVM) is necessary to create and invoke constructors.
 *
 *  These standard names can be referred to using [[nme `nme`]] for term names and [[tpnme `tpnme`]] for type names
 *
 *  @see [[Names]]
 *
 *  The API for names in Scala reflection.
 *  @groupname StandardNames Standard Names
 *  @group ReflectionAPI
 */
trait StandardNames {
  self: Universe =>

  /** @see [[termNames]] */
  @deprecated("use `termNames` instead", "2.11.0")
  val nme: TermNamesApi

  /** A value containing all [[TermNamesApi standard term names]].
   *  @group StandardNames
   */
  val termNames: TermNamesApi

  /** @see [[typeNames]] */
  @deprecated("use `typeNames` instead", "2.11.0")
  val tpnme: TypeNamesApi

  /** A value containing all [[TypeNamesApi standard type names]].
   *  @group StandardNames
   */
  val typeNames: TypeNamesApi

  /** Defines standard names, common for term and type names: These can be accessed via the [[nme]] and [[tpnme]] members.
   *  @group API
   */
  trait NamesApi {
    /** An abstract type that represents the exact flavor of the name. */
    type NameType >: Null <: Name

    /** The term or type name `_`.
     *  Used to construct trees that correspond to underscores in Scala.
     */
    val WILDCARD: NameType

    /** The term or type name corresponding to an empty string.
     *  Represents an empty name, used to denote the fact that no name was specified
     *  for `privateWithin` in [[Trees#Modifiers]], for [[Trees#This]],
     *  for [[Trees#Super]], etc.
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

  /** Defines standard term names that can be accessed via the [[nme]] member.
   *  @group API
   */
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

    /** The term name `<empty>`.
     *  Represents the empty package.
     */
    val EMPTY_PACKAGE_NAME: NameType

    /** The string " " (a single whitespace).
     *  `LOCAL_SUFFIX_STRING` is appended to the names of local identifiers,
     *  when it's necessary to prevent a naming conflict. For example, underlying fields
     *  of non-private vals and vars are renamed using `LOCAL_SUFFIX_STRING`.
     */
    val LOCAL_SUFFIX_STRING: String
  }

  /** Defines standard type names that can be accessed via the [[tpnme]] member.
   *  @group API
   */
  trait TypeNamesApi extends NamesApi {
    /** @inheritdoc */
    type NameType = TypeName

    /** The type name `_*`.
     *  Used to construct types that specify sequence arguments to repeated parameters.
     */
    val WILDCARD_STAR: NameType
  }
}
