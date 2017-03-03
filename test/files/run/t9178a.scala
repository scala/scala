trait Sam { def apply(): Unit }
abstract class Test {
  def foo(): Sam
  // no parens, instantiateToMethodType would wrap in a `new Sam { def apply = foo }`
  // rather than applying to an empty param list() */
  val f: Sam = foo
}

object Test extends Test {
  lazy val samIAm = new Sam { def apply() {} }
  def foo() = samIAm
  def main(args: Array[String]): Unit = {
    assert(f eq samIAm, f)
  }
}
