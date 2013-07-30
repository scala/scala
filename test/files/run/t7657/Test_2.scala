object Test extends App {
  val c = new C()
  println(c.t())
  println((c: T).t())
  println((c: A).t())
}