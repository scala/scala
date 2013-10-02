package patmat

abstract class BaseType

case class CaseOne(x: Int, y: List[Int]) extends BaseType
case class CaseTwo(str: String) extends BaseType

class PatMatTests {

  def foo(x: BaseType) {
    x match {
      case CaseOne/*#*/(10, first :: second :: Nil) =>
        val tmp = 23
        println(first/*#*/)
        println(tmp/*#*/)

      case CaseTwo/*#*/(mystring) =>
        println(mystring/*#*/)
    }
  }

  def multipleAssign() {
    val (x, y) = ("abc", "def")

    println(x/*#*/, y/*#*/)
  }

}