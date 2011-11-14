// we get now: t2336.scala:5: error: type Foo[Int] is not a stable prefix
class Foo[A] {
  class Bar[B >: A](x: B) extends Foo[B]
}
object bug {
  new Foo[Int]#Bar(0)
}
