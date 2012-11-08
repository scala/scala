object Test extends App {
	val a = Array("1")
  val a2 = Array(a: _*)
  a2(0) = "2"
  assert(a(0) == "1")
}
