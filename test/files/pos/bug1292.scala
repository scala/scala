trait Foo[T <: Foo[T, Enum], Enum <: Enumeration] {
  type StV = Enum#Value
  type Meta = MegaFoo[T, Enum]

  type Slog <: Enumeration

  def getSingleton: Meta
}

trait MegaFoo[T <: Foo[T, Enum], Enum <: Enumeration] extends Foo[T, Enum] {
  def doSomething(what: T, misc: StV, dog: Meta#Event) = None
  abstract class Event
  object Event

  def stateEnumeration: Slog
  def se2: Enum
}

object E extends Enumeration {
  val A = Value
  val B = Value
}

class RFoo extends Foo[RFoo, E.type] {
  def getSingleton = MegaRFoo

  type Slog = E.type
}

object MegaRFoo extends RFoo with MegaFoo[RFoo, E.type] {
  def stateEnumeration = E
  def se2 = E
}
