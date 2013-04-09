import scala.reflect.runtime.universe._

class A {
  def $$ = 1
  def $times = 1
}

object Test {
  def main(args: Array[String]): Unit = {
    val memberSet: Set[String] = typeOf[A].members.map{ _.toString }.toSet
    assert(memberSet contains "method *")
    assert(memberSet contains "method $$")
    assert(! (memberSet contains "method"))
  }
}
