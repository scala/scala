package foo.scala

import scala.reflect.runtime.universe._

object Foo {

  def bar[T: TypeTag](): Unit = {
  }

  import foo._
  bar[String]()
}
