package tastytest

object TestMacroCompat {
  import MacroCompat._

  def test = {
    val result = MacroCompat.testCase("foo")(pos)
    println(result)
  }

}
