package scala.reflect
package base

trait Scopes { self: Universe =>

  type Scope >: Null <: ScopeBase

  /** The base API that all scopes support */
  trait ScopeBase extends Iterable[Symbol]

  /** A tag that preserves the identity of the `Scope` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val ScopeTag: ClassTag[Scope]

  type MemberScope >: Null <: Scope with MemberScopeBase

  /** The base API that all member scopes support */
  trait MemberScopeBase extends ScopeBase {
    /** Sorts the symbols included in this scope so that:
     *    1) Symbols appear the linearization order of their owners.
     *    2) Symbols with the same owner appear in reverse order of their declarations.
     *    3) Synthetic members (e.g. getters/setters for vals/vars) might appear in arbitrary order.
     */
    def sorted: List[Symbol]
  }

  /** A tag that preserves the identity of the `MemberScope` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val MemberScopeTag: ClassTag[MemberScope]

  /** Create a new scope */
  def newScope: Scope

  /** Create a new scope nested in another one with which it shares its elements */
  def newNestedScope(outer: Scope): Scope

  /** Create a new scope with given initial elements */
  def newScopeWith(elems: Symbol*): Scope
}