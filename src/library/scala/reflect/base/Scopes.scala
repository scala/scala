package scala.reflect
package base

trait Scopes { self: Universe =>

  type Scope >: Null <: Iterable[Symbol]

  /** A tag that preserves the identity of the `Scope` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val ScopeTag: ClassTag[Scope]

  /** Create a new scope */
  def newScope: Scope

  /** Create a new scope nested in another one with which it shares its elements */
  def newNestedScope(outer: Scope): Scope

  /** Create a new scope with given initial elements */
  def newScopeWith(elems: Symbol*): Scope
}