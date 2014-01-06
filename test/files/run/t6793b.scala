package a {
  class C1(val v0: String)
  class C2(v1: String) extends a.C1(v1) { def foo = v1 }
}

object Test extends App {
  new a.C2("x")

  val c2Fields = classOf[a.C2].getDeclaredFields
  assert(c2Fields.isEmpty, c2Fields.map(_.getName).mkString(", "))
}
