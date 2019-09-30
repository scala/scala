package tastytest

object TestNats {
  import Laws._

  object Longs extends Nats {
    type Nat                = Long
    val zero                = 0L
    val one                 = 1L
    def add(n: Nat, m: Nat) = n + m
    def mul(n: Nat, m: Nat) = n * m
  }

  //TODO inline into Nats
  object Laws {
    def additiveIdentity(n: Longs.Nat) =
      Longs.add(n, Longs.zero) == n
    def additiveCommutativity(n: Longs.Nat, m: Longs.Nat) =
      Longs.add(n, m) == Longs.add(m, n)
    def additiveAssociativity(n: Longs.Nat, m: Longs.Nat, o: Longs.Nat) =
      Longs.add(n, Longs.add(m, o)) == Longs.add(Longs.add(n, m), o)
    def multiplicativeIdentity(n: Longs.Nat) =
      Longs.mul(n, Longs.one) == n
    def multiplicativeCommutativity(n: Longs.Nat, m: Longs.Nat) =
      Longs.mul(n, m) == Longs.mul(m, n)
    def multiplicativeAssociativity(n: Longs.Nat, m: Longs.Nat, o: Longs.Nat) =
      Longs.mul(n, Longs.mul(m, o)) == Longs.mul(Longs.mul(n, m), o)
  }


  def test1 = assert(additiveIdentity(getRandom))
  def test2 = assert(additiveCommutativity(getRandom, getRandom))
  def test3 = assert(additiveAssociativity(getRandom, getRandom, getRandom))
  def test4 = assert(multiplicativeIdentity(getRandom))
  def test5 = assert(multiplicativeCommutativity(getRandom, getRandom))
  def test6 = assert(multiplicativeAssociativity(getRandom, getRandom, getRandom))

  def main(args: Array[String]): Unit = {
    for (_ <- 1 to 100) {
      test1
      test2
      test3
      test4
      test5
      test6
    }
    println("Suite passed!")
  }
}