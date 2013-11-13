// test that dependent types work
// TODO: def apply(x: String): x.type does NOT work yet
object Test {
  val s: String = ""

  trait T { def apply(x: s.type): s.type }

  val preservedResult: s.type = ((x => x): T)(s)
}