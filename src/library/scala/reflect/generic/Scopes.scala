package scala.reflect
package generic

@deprecated("scala.reflect.generic will be removed", "2.9.1") trait Scopes { self: Universe =>

  abstract class AbsScope extends Iterable[Symbol] {
    private[reflect] def enter(sym: Symbol): Symbol
  }

  type Scope <: AbsScope

  def newScope(): Scope
}


