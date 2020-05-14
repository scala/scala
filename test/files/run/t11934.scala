trait T[@specialized(Int, Unit) S] {
  var value: S = _
}

final class C[@specialized(Int, Unit) S] extends T[S]

object Test extends App {
  new C[Int]
}