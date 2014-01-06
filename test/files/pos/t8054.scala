trait D {
  trait Manifest {
    class Entry
  }

  val M: Manifest

  def m: M.Entry = ???
}

object D1 extends D {
  object M extends Manifest
}

object D2 extends D {
  val M: Manifest = ???
}

object Hello {

  def main(args: Array[String]) {
    // 2.10.3 - ok
    // 2.11.0-M7 - type mismatch; found : Seq[DB1.MANIFEST.Entry]
    // required: Seq[DB1.MANIFEST.Entry]
    val t1: D1.M.Entry = D1.m

    // 2.10.3 - ok
    // 2.11.0-M7 - ok
    val t2: D2.M.Entry = D2.m
  }
}
