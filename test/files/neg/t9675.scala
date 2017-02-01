object Test {
  case class A(x: String)

  val func1 = (x: A) => { x != "x" }

  val func2  = (x: A) => { x != "x" }: Boolean

  val func3: Function1[A, Boolean] = (x) => { x != "x" }

  val func4 = new Function1[A, Boolean] {
    def apply(x: A): Boolean = { x != "x" }
  }

  def method(x: A): Boolean = { x != "x" }
  case class PersonInfo(rankPayEtc: Unit)

  def main(args: Array[String]) {
    A("x") != "x"

    val func5: Function1[A, Boolean] = (x) => { x != "x" }

    List(A("x")).foreach((item: A) => item != "x")
  }
}
