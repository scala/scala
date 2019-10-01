package tastytest

trait Nats {

  type Nat
  val zero: Nat
  val one: Nat
  def add(n: Nat, m: Nat): Nat
  def mul(n: Nat, m: Nat): Nat

  object Laws {

    def additiveIdentity(n: Nat)                            = add(n, zero) == n
    def additiveCommutativity(n: Nat, m: Nat)               = add(n, m) == add(m, n)
    def additiveAssociativity(n: Nat, m: Nat, o: Nat)       = add(n, add(m, o)) == add(add(n, m), o)
    def multiplicativeIdentity(n: Nat)                      = mul(n, one) == n
    def multiplicativeCommutativity(n: Nat, m: Nat)         = mul(n, m) == mul(m, n)
    def multiplicativeAssociativity(n: Nat, m: Nat, o: Nat) = mul(n, mul(m, o)) == mul(mul(n, m), o)

    def multiplicativeDistributivity(n: Nat, m: Nat, o: Nat) =
      mul(n, add(m, o)) == add(mul(n, m), mul(n, o)) && mul(add(n, m), o) == add(mul(n, o), mul(m, o))

  }
}
