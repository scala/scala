package scala.reflect
package api

trait Scopes { self: Universe =>

  type Scope <: Iterable[Symbol]

  /** Create a new scope */
  def newScope: Scope

  /** Create a new scope nested in another one with which it shares its elements */
  def newNestedScope(outer: Scope): Scope

  /** Create a new scope with given initial elements */
  def newScopeWith(elems: Symbol*): Scope
}