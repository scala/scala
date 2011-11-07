package fr.up5.mi.noel.scala
object Test {
  def make(t: Test) : Test = TestList(t.args.toList)
}
case class TestList[T](elements: List[T])(implicit f: T => Test)
 
class Test {
  val args: Array[Test]
}
