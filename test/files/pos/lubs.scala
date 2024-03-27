object Test {
  List(new { def f = 1; def g = 1}, new { def f = 2}).map(_.f)
}

object inv {
  val m1 = if (this.hashCode == 0) Map("" -> "") else Map.empty
  val m2: Map[String, String] = m1
  val m3 = m2 ++ Map("a" -> "a")
  val m4: Map[String, String] = m3
}
