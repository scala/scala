package tastytest

trait Nats {
  type Nat
  val zero: Nat
  val one: Nat
  def add(n: Nat, m: Nat): Nat
  def mul(n: Nat, m: Nat): Nat
}
