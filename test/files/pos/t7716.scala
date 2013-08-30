object Test {
  def test: Unit = {
    val e: java.lang.Enum[_] = java.util.concurrent.TimeUnit.SECONDS
    e match { case x => println(x) }


    trait TA[X <: CharSequence]
    val ta: TA[_] = new TA[String] {}

    ta match {
      case _ => println("hi")
    }

    def f(ta: TA[_]) = ta match { case _ => "hi" }
  }
}
