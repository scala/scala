package tastytest

object TestNats {

  object Longs extends Nats {
    type Nat                = Long
    val zero                = 0L
    val one                 = 1L
    def add(n: Nat, m: Nat) = n + m
    def mul(n: Nat, m: Nat) = n * m
  }

  import Longs.Laws._

  def test1 = assert(additiveIdentity(getRandom))
  def test2 = assert(additiveCommutativity(getRandom, getRandom))
  def test3 = assert(additiveAssociativity(getRandom, getRandom, getRandom))
  def test4 = assert(multiplicativeIdentity(getRandom))
  def test5 = assert(multiplicativeCommutativity(getRandom, getRandom))
  def test6 = assert(multiplicativeAssociativity(getRandom, getRandom, getRandom))
  def test7 = assert(multiplicativeDistributivity(getRandom, getRandom, getRandom))

  def main(args: Array[String]): Unit = {
    for (_ <- 1 to 1000000) {
      test1
      test2
      test3
      test4
      test5
      test6
      test7
    }
    println("Suite passed!")
  }
}