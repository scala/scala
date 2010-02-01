package scala.reflect
package generic

trait Scopes { self: Universe =>

  abstract class AbsScope extends Iterable[Symbol] {
    def enter(sym: Symbol): Symbol
  }

  type Scope <: AbsScope

  def newScope(): Scope
}


