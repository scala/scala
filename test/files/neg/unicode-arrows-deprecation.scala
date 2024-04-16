//> using options -deprecation -Xfatal-warnings
//
object Test {
  val a: Int ⇒ Int = x ⇒ x

  val b = for { x ← (1 to 10) } yield x

  val c: (Int, Int) = 1 → 1
}
