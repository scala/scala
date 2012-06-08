package scala.reflect
package api

import scala.language.implicitConversions

trait FlagSets { self: Universe =>

  type FlagSet

  trait FlagOps extends Any {
    def | (right: FlagSet): FlagSet
    def & (right: FlagSet): FlagSet
    def containsAll (right: FlagSet): Boolean
  }

  implicit def addFlagOps(left: FlagSet): FlagOps

  val Flag: FlagValues

  type FlagValues >: Null <: FlagValuesApi

  // [Eugene++] any other flags we would like to expose?

  trait FlagValuesApi {

    /** Flag indicating that symbol or tree represents a trait */
    val TRAIT: FlagSet

    /** Flag indicating that symbol or tree represents a module or its internal module class */
    val MODULE: FlagSet

    /** Flag indicating that symbol or tree represents a mutable variable */
    val MUTABLE: FlagSet

    /** Flag indicating that symbol or tree represents a package or its internal package class */
    val PACKAGE: FlagSet

    /** Flag indicating that symbol or tree represents a method */
    val METHOD: FlagSet

    /** Flag indicating that symbol or tree represents a macro definition. */
    val MACRO: FlagSet

    /** Flag indicating that symbol or tree represents an abstract type, method, or value */
    val DEFERRED: FlagSet

    /** Flag indicating that symbol or tree represents an abstract class */
    val ABSTRACT: FlagSet

    /** Flag indicating that symbol or tree has `final` modifier set */
    val FINAL: FlagSet

    /** Flag indicating that symbol or tree has `sealed` modifier set */
    val SEALED: FlagSet

    /** Flag indicating that symbol or tree has `implicit` modifier set */
    val IMPLICIT: FlagSet

    /** Flag indicating that symbol or tree has `lazy` modifier set */
    val LAZY: FlagSet

    /** Flag indicating that symbol or tree has `override` modifier set */
    val OVERRIDE: FlagSet

    /** Flag indicating that symbol or tree has `private` modifier set */
    val PRIVATE: FlagSet

    /** Flag indicating that symbol or tree has `protected` modifier set */
    val PROTECTED: FlagSet

    /** Flag indicating that symbol or tree has `case` modifier set */
    val CASE: FlagSet

    /** Flag indicating that symbol or tree has `abstract` and `override` modifiers set */
    val ABSOVERRIDE: FlagSet

    /** Flag indicating that symbol or tree represents a by-name parameter */
    val BYNAMEPARAM: FlagSet

    /** Flag indicating that symbol or tree represents a class or parameter.
     *  Both type and value parameters carry the flag. */
    val PARAM: FlagSet

    /** Flag indicating that symbol or tree represents a field of a class
     *  that was generated from a parameter of that class */
    val PARAMACCESSOR: FlagSet

    /** Flag indicating that symbol or tree represents a field of a case class
     *  that corresponds to a parameter in the first parameter list of the
     *  primary constructor of that class */
    val CASEACCESSOR: FlagSet

    /** Flag indicating that symbol or tree represents a contravariant
     *  type parameter (marked with `+`). */
    val COVARIANT: FlagSet

    /** Flag indicating that symbol or tree represents a contravariant
     *  type parameter (marked with `-`). */
    val CONTRAVARIANT: FlagSet

    /** Flag indicating that parameter has a default value */
    val DEFAULTPARAM: FlagSet

    /** Flag indicating that trait has neither method implementations nor fields.
     *  This means the trait can be represented as a Java interface. */
    val INTERFACE: FlagSet

    def union(flags: FlagSet*): FlagSet
    def intersection(flag: FlagSet*): FlagSet
    def containsAll(superset: FlagSet, subset: FlagSet): Boolean
  }
}
