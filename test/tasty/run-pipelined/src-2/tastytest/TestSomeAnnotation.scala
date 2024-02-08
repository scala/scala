package tastytest

import lib.{SomeAnnotation, SomeAnnotated}

object TestSomeAnnotation extends scala.App {

  @SomeAnnotation(value = "hello")
  def method = 23

  @SomeAnnotation(value = "hello", year = 1996)
  def method2 = 23

  @SomeAnnotation(value = "hello", year = 1996, classes = Array(classOf[Long]))
  def method3 = 23

  assert(SomeAnnotated.method == 23)
  assert(SomeAnnotated.method2 == 23)
  assert(SomeAnnotated.method3 == 23)

}
