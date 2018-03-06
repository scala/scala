class A
class B extends A
class C extends B
class D extends C
class E extends D

class Inv[T](override val toString: String)
class Con[-T](override val toString: String)
class Cov[+T](override val toString: String)

object InvTest {
  implicit val a : Inv[A] = new Inv[A]("Inv[A]")
  implicit val b : Inv[B] = new Inv[B]("Inv[B]")
  implicit val c : Inv[C] = new Inv[C]("Inv[C]")
  implicit val d : Inv[D] = new Inv[D]("Inv[D]")
  implicit val e : Inv[E] = new Inv[E]("Inv[E]")
}
object ConTest {
  implicit val a : Con[A] = new Con[A]("Con[A]")
  implicit val c : Con[C] = new Con[C]("Con[C]")
  implicit val e : Con[E] = new Con[E]("Con[E]")
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
    import ConTest._
    println(List(
      implicitly[Con[A]],
      implicitly[Con[B]],
      implicitly[Con[C]],
      implicitly[Con[D]],
      implicitly[Con[E]]
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
Con[A] Con[A] Con[A] Con[A] Con[A]
Cov[E] Cov[E] Cov[E] Cov[E] Cov[E]

Currently (and in Dotty):

Inv[A] Inv[B] Inv[C] Inv[D] Inv[E]
Con[A] Con[A] Con[C] Con[C] Con[E]
Cov[E] Cov[E] Cov[E] Cov[E] Cov[E]

Note that @paulp thinks that f2 should produce,

Cov[A] Cov[C] Cov[C] Cov[E] Cov[E]

I don't think that behaviour would be desirable: in the covariant case the
expectation that the most derived should be selected as the most specific
seems reasonable.
***/
