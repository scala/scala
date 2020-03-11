class Test {
  lazy val test: Seq[String] = Seq.empty
}

class Test2 extends Test {
  override val test: Seq[String] = super.test
}
