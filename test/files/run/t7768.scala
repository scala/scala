// scalac: -Xsource:3.0
class A
class B extends A
class C extends B
class D extends C
class E extends D

class Inv[T](override val toString: String)
class Contra[-T](override val toString: String)
class Cov[+T](override val toString: String)

object InvTest {
  implicit val a : Inv[A] = new Inv[A]("Inv[A]")
  implicit val b : Inv[B] = new Inv[B]("Inv[B]")
  implicit val c : Inv[C] = new Inv[C]("Inv[C]")
  implicit val d : Inv[D] = new Inv[D]("Inv[D]")
  implicit val e : Inv[E] = new Inv[E]("Inv[E]")
}
object ContraTest {
  implicit val a : Contra[A] = new Contra[A]("Contra[A]")
  implicit val c : Contra[C] = new Contra[C]("Contra[C]")
  implicit val e : Contra[E] = new Contra[E]("Contra[E]")
}
object CovTest {
  implicit val a : Cov[A] = new Cov[A]("Cov[A]")
  implicit val c : Cov[C] = new Cov[C]("Cov[C]")
  implicit val e : Cov[E] = new Cov[E]("Cov[E]")
}

object Test {
  def f0(): Unit = {
    import InvTest._
    println(List(
      implicitly[Inv[A]],
      implicitly[Inv[B]],
      implicitly[Inv[C]],
      implicitly[Inv[D]],
      implicitly[Inv[E]]
    ) mkString " ")
  }
  def f1(): Unit = {
    import ContraTest._
    println(List(
      implicitly[Contra[A]],
      implicitly[Contra[B]],
      implicitly[Contra[C]],
      implicitly[Contra[D]],
      implicitly[Contra[E]]
    ) mkString " ")
  }
  def f2(): Unit = {
    import CovTest._
    println(List(
      implicitly[Cov[A]],
      implicitly[Cov[B]],
      implicitly[Cov[C]],
      implicitly[Cov[D]],
      implicitly[Cov[E]]
    ) mkString " ")
  }

  def main(args: Array[String]): Unit = {
    f0()
    f1()
    f2()
  }
}

/***

Previously:

Inv[A] Inv[B] Inv[C] Inv[D] Inv[E]
Contra[A] Contra[A] Contra[A] Contra[A] Contra[A]
Cov[E] Cov[E] Cov[E] Cov[E] Cov[E]

Currently (and in Dotty):

Inv[A] Inv[B] Inv[C] Inv[D] Inv[E]
Contra[A] Contra[A] Contra[C] Contra[C] Contra[E]
Cov[E] Cov[E] Cov[E] Cov[E] Cov[E]

Note that @paulp thinks that f2 should produce,

Cov[A] Cov[C] Cov[C] Cov[E] Cov[E]

I don't think that behaviour would be desirable: in the covariant case the
expectation that the most derived should be selected as the most specific
seems reasonable.
***/
