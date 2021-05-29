case class CaseClass(a: String, b: Int)
case object CaseObject
case class ImplicitParamsCaseClass[A: Ordering](a: A)
case class CurriedCaseClass(i: Int)(s: String)

object Test extends App {

  try {
    CaseClass("foo", 123).productElementName(99)
  } catch {
    case e: IndexOutOfBoundsException =>
      println(e)
      e.getStackTrace.take(4).foreach(s => println(s.toString.takeWhile(_ != '(')))
  }

  println()

  try {
    CaseObject.productElementName(99)
  } catch {
    case e: IndexOutOfBoundsException =>
      println(e)
      e.getStackTrace.take(4).foreach(s => println(s.toString.takeWhile(_ != '(')))
  }

  println()

  try {
    ImplicitParamsCaseClass(42).productElementName(1)
  } catch {
    case e: IndexOutOfBoundsException =>
      println(e)
      e.getStackTrace.take(4).foreach(s => println(s.toString.takeWhile(_ != '(')))
  }

  println()

  try {
    CurriedCaseClass(42)("").productElementName(1)
  } catch {
    case e: IndexOutOfBoundsException =>
      println(e)
      e.getStackTrace.take(4).foreach(s => println(s.toString.takeWhile(_ != '(')))
  }

}

