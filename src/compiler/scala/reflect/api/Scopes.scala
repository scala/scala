package scala.reflect
package api

trait Scopes { self: Universe =>

  type Scope <: Iterable[Symbol]

  def newScope(): Scope
}


