case class wrong(`a b`: Int, a: Int)
case class verywrong(`a b`: Int, a: String)
case class privateCase1(private var `a b`: Int, private var a: String)
case class privateCase2(private var a: String, private var `a b`: Int)

object Test {
  def main(args: Array[String]): Unit = {
    val a @ wrong(1, 2) = wrong(1, 2)
    assert(a.`a b` == 1)
    assert(a.a == 2)

    val b @ verywrong(1, "2") = verywrong(1, "2")
    assert(b.`a b` == 1)
    assert(b.a == "2")

    val privateCase1(1, "2") = privateCase1(1, "2")
    val privateCase2("1", 2) = privateCase2("1", 2)
  }
}
