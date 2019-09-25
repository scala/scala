package tastytest

object TestCurriedOld {

  def test1 = assert(new CurriedCtorOld(0)("foo")(false).s === "foo")
  def test2 = assert(new OverriderOld(0)(true).a === true)

  def main(args: Array[String]): Unit = {
    test1
    test2
    println("Suite passed!")
  }
}