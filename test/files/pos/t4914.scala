trait Type {
  type S
}

class ConcreteType extends Type {
  type S = Double
}

trait Base {
  type T <: Type
  val m: Map[t#S, t#S] forSome { type t <: T with Singleton }
  val n: Map[x.type#S, x.type#S] forSome { val x: T }
}

abstract class Derived extends Base {
  override type T = ConcreteType
  override val m = Map[Double, Double]()
  /** This does not work. §3.2.10 indicates that types n is shorthand for type of m. */
  override val n = Map[Double, Double]()
}
