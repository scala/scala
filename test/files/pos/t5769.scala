// a.scala

class A {
  type AI = Array[Int]

  def f1 = arrayTag[Array[Int]]
  def f2 = arrayTag[AI]
}