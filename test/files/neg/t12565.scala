// scalac: -Werror --release 8
// javaVersion: 9+

class C {
  def f = new java.time.Instant
}
object Test extends App {
  println(new C().f)
}
