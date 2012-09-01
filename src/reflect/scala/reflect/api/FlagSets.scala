package scala.reflect
package api

import scala.language.implicitConversions

trait FlagSets extends base.FlagSets { self: Universe =>

  type FlagSet

  trait FlagOps extends Any {
    def | (right: FlagSet): FlagSet
  }

  implicit def addFlagOps(left: FlagSet): FlagOps

  val Flag: FlagValues

  type FlagValues >: Null <: FlagValuesApi

  // Q: I have a pretty flag. Can I put it here?
  // A: Only if there's a tree that cannot be built without it.
  //    If you want to put a flag here so that it can be tested against,
  //    introduce an `isXXX` method in one of the `api.Symbols` classes instead.

  trait FlagValuesApi {

    /** Flag indicating that tree represents a trait */
    val TRAIT: FlagSet

    /** Flag indicating that a tree is an interface (i.e. a trait which defines only abstract methods) */
    val INTERFACE: FlagSet

    /** Flag indicating that tree represents a mutable variable */
    val MUTABLE: FlagSet

    /** Flag indicating that tree represents a macro definition. */
    val MACRO: FlagSet

    /** Flag indicating that tree represents an abstract type, method, or value */
    val DEFERRED: FlagSet

    /** Flag indicating that tree represents an abstract class */
    val ABSTRACT: FlagSet

    /** Flag indicating that tree has `final` modifier set */
    val FINAL: FlagSet

    /** Flag indicating that tree has `sealed` modifier set */
    val SEALED: FlagSet

    /** Flag indicating that tree has `implicit` modifier set */
    val IMPLICIT: FlagSet

    /** Flag indicating that tree has `lazy` modifier set */
    val LAZY: FlagSet

    /** Flag indicating that tree has `override` modifier set */
    val OVERRIDE: FlagSet

    /** Flag indicating that tree has `private` modifier set */
    val PRIVATE: FlagSet

    /** Flag indicating that tree has `protected` modifier set */
    val PROTECTED: FlagSet

    /** Flag indicating that tree represents a member local to current class
     *  (i.e. private[this] or protected[this].
     *  This requires having either PRIVATE or PROTECTED set as well.
     */
    val LOCAL: FlagSet

    /** Flag indicating that tree has `case` modifier set */
    val CASE: FlagSet

    /** Flag indicating that tree has `abstract` and `override` modifiers set */
    val ABSOVERRIDE: FlagSet

    /** Flag indicating that tree represents a by-name parameter */
    val BYNAMEPARAM: FlagSet

    /** Flag indicating that tree represents a class or parameter.
     *  Both type and value parameters carry the flag. */
    val PARAM: FlagSet

    /** Flag indicating that tree represents a covariant
     *  type parameter (marked with `+`). */
    val COVARIANT: FlagSet

    /** Flag indicating that tree represents a contravariant
     *  type parameter (marked with `-`). */
    val CONTRAVARIANT: FlagSet

    /** Flag indicating that tree represents a parameter that has a default value */
    val DEFAULTPARAM: FlagSet

    /** Flag indicating that tree represents an early definition */
    val PRESUPER: FlagSet

    /** Flag indicating that tree represents a variable or a member initialized to the default value */
    val DEFAULTINIT: FlagSet
  }
}
