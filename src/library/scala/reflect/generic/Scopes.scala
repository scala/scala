package scala.reflect
package generic

trait Scopes { self: Universe =>

  abstract class AbsScope extends Iterable[Symbol] {
    private[reflect] def enter(sym: Symbol): Symbol
  }

  type Scope <: AbsScope

  def newScope(): Scope
}


