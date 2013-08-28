class A
class B extends A

class Crash(b1: Seq[B], b2: Seq[B]) {
  def this(a: Seq[A]) = this(a.collect{ case b: B => b}, a.collect{ case b: B => b})
}

object Main extends App {

  // runtime exception with either constructor
  val c1 = new Crash(Seq(new B, new B))
  val c2 = new Crash(Seq(new B), Seq(new B))

}
