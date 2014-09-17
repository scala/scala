object Test extends App {
  val c = Class.forName("A$$nestedInAnon$1$lambda$$run$1")
  println(c.getName)
}
