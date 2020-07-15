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

  test(assert(additiveIdentity(getRandomNat)))
  test(assert(additiveCommutativity(getRandomNat, getRandomNat)))
  test(assert(additiveAssociativity(getRandomNat, getRandomNat, getRandomNat)))
  test(assert(multiplicativeIdentity(getRandomNat)))
  test(assert(multiplicativeCommutativity(getRandomNat, getRandomNat)))
  test(assert(multiplicativeAssociativity(getRandomNat, getRandomNat, getRandomNat)))
  test(assert(multiplicativeDistributivity(getRandomNat, getRandomNat, getRandomNat)))

  override val reps = 1_000_000
}
