object Kafi {
  // OK in 2.12, fails in 2.13 with "missing parameter type for expanded function"
  val c1: Cache[(Seq[String], Class[_]), String] = build {
    case (sq, cs) => mk(sq, cs)
  }

  // OK in both 2.12 and 2.13
  val c2: Cache[(Seq[String], Class[_]), String] = build(
    key => mk(key._1, key._2)
  )

  def mk(sq: Seq[String], cs: Class[_]): String = ""

  def build[K, V](): Cache[K, V] = null
  def build[K, V](c: Cache[K, V]): Cache[K, V] = null

  trait Cache[K, V] { def load(k: K): V }
}
