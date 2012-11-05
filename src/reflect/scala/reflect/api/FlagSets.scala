package scala.reflect
package api

import scala.language.implicitConversions

/**
 * <span class="badge badge-red" style="float: right;">EXPERIMENTAL</span>
 *
 * The trait that defines flag sets and operations on them.
 *
 * `Flag`s are used to provide modifiers for abstract syntax trees that represent definitions
 * via the `flags` field of [[scala.reflect.api.Trees#Modifiers]]. Trees that accept modifiers are:
 *
 *   - '''[[scala.reflect.api.Trees#ClassDef]]'''. Classes and traits.
 *   - '''[[scala.reflect.api.Trees#ModuleDef]]'''. Objects.
 *   - '''[[scala.reflect.api.Trees#ValDef]]'''. Vals, vars, parameters and self-type annotations.
 *   - '''[[scala.reflect.api.Trees#DefDef]]'''. Methods and constructors.
 *   - '''[[scala.reflect.api.Trees#TypeDef]]'''. Type aliases, abstract type members and type parameters.
 *
 * For example, to create a class named `C` one would write something like:
 * {{{
 *  ClassDef(Modifiers(NoFlags), newTypeName("C"), Nil, ...)
 * }}}
 *
 * Here, the flag set is empty.
 *
 * To make `C` private, one would write something like:
 * {{{
 *  ClassDef(Modifiers(PRIVATE), newTypeName("C"), Nil, ...)
 * }}}
 *
 * Flags can also be combined with the vertical bar operator (`|`).
 * For example, a private final class is written something like:
 * {{{
 *  ClassDef(Modifiers(PRIVATE | FINAL), newTypeName("C"), Nil, ...)
 * }}}
 *
 * The list of all available flags is defined in [[scala.reflect.api.FlagSets#FlagValues]], available via
 * [[scala.reflect.api.FlagSets#Flag]]. (Typically one writes a wildcard import for this, e.g.
 * `import scala.reflect.runtime.universe.Flag._`).
 *
 * Definition trees are compiled down to symbols, so flags on modifiers of these trees are transformed into flags
 * on the resulting symbols. Unlike trees, symbols don't expose flags, but rather provide `isXXX` test methods
 * (e.g. `isFinal` can be used to test finality). These test methods might require an upcast with `asTerm`,
 * `asType` or `asClass` as some flags only make sense for certain kinds of symbols.
 *
 * ''Of Note:'' This part of the Reflection API is being considered as a candidate for redesign. It is
 * quite possible that in future releases of the reflection API, flag sets could be replaced with something else.
 *
 * For more details about `FlagSet`s and other aspects of Scala reflection, see the
 * [[http://docs.scala-lang.org/overviews/reflection/overview.html Reflection Guide]]
 *
 * @group ReflectionAPI
 *
 */
trait FlagSets { self: Universe =>

  /** An abstract type representing sets of flags (like private, final, etc.) that apply to definition trees and symbols
   *  @template
   *  @group Flags
   */
  type FlagSet

  /** A tag that preserves the identity of the `FlagSet` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   *  @group Tags
   */
  implicit val FlagSetTag: ClassTag[FlagSet]

  /** The API of `FlagSet` instances.
   *  The main source of information about flag sets is the [[scala.reflect.api.FlagSets]] page.
   *  @group Flags
   */
  trait FlagOps extends Any {
    /** Produces a flag set that's a union of this flag set and the provided flag set. */
    def | (right: FlagSet): FlagSet
  }

  /** The API of `FlagSet` instances.
   *  @group Flags
   */
  implicit def addFlagOps(left: FlagSet): FlagOps

  /** A module that contains all possible values that can constitute flag sets.
   *  @group Flags
   */
  val Flag: FlagValues

  // Q: I have a pretty flag. Can I put it here?
  // A: Only if there's a tree that cannot be built without it.
  //    If you want to put a flag here so that it can be tested against,
  //    introduce an `isXXX` method in one of the `api.Symbols` classes instead.

  /** All possible values that can constitute flag sets.
   *  The main source of information about flag sets is the [[scala.reflect.api.FlagSets]] page.
   *  @group Flags
   */
  trait FlagValues {

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

  /** The empty set of flags
   *  @group Flags
   */
  val NoFlags: FlagSet
}
