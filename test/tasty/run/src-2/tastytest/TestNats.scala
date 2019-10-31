package tastytest

object TestNats extends Suite("TestNats") {

  object Longs extends Nats {
    type Nat                = Long
    val zero                = 0L
    val one                 = 1L
    def add(n: Nat, m: Nat) = n + m
    def mul(n: Nat, m: Nat) = n * m
  }

  import Longs.Laws._

  test(assert(additiveIdentity(getRandom)))
  test(assert(additiveCommutativity(getRandom, getRandom)))
  test(assert(additiveAssociativity(getRandom, getRandom, getRandom)))
  test(assert(multiplicativeIdentity(getRandom)))
  test(assert(multiplicativeCommutativity(getRandom, getRandom)))
  test(assert(multiplicativeAssociativity(getRandom, getRandom, getRandom)))
  test(assert(multiplicativeDistributivity(getRandom, getRandom, getRandom)))

  override def reps = 1_000_000
}
