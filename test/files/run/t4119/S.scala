class S extends foo.bar.J {
  sss =>

  val fn = () => {
    foo(S.this)
  }
  fn()
}

object Test {
  def main(args: Array[String]): Unit = {
    new S
  }
}
