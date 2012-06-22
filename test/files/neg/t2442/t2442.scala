class Test {
  import MyEnum._

  def f(e: MyEnum) = e match {
    case ONE => println("one")
    case TWO => println("two")
    // missing case --> exhaustivity warning!
  }
}