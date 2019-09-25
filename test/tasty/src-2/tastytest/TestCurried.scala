package tastytest

object TestCurried {

  def test1 = assert(new CurriedCtor(99)("")(false).a === 99)
  def test2 = assert(new CurriedCtor(0)("foo")(false).s === "foo")
  def test3 = assert(new CurriedCtor(0)("")(true).b === true)
  def test4 = assert(new Overrider(101)(false).a === 101)
  def test5 = assert(new Overrider(0)(false).s === "overrider")
  def test6 = assert(new Overrider(0)(true).b === true)

  def main(args: Array[String]): Unit = {
    test1
    test2
    test3
    test4
    test5
    test6
    println("Suite passed!")
  }
}