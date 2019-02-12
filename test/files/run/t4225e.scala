
import scala.language.implicitConversions

object Test extends App {
  class Foo {
    class Bar {
      override def toString() = "bar"
    }
    object Bar {
      implicit def fromString(a: String): Bar = { println("convert bar") ; new Bar }
    }

    def andThen_:(b: Bar) = { println("pre") ; println(s"use $b") ; println("post") }
    def andThenByName_:(b: => Bar) = { println("pre") ; println(s"use $b") ; println(s"use $b") ; println("post") }
  }

  def mkFoo: Foo = { println("foo") ; new Foo }
  def mkBarString: String = { println("bar"); "Bar" }

  mkBarString andThen_: mkFoo

  println()

  mkFoo.andThen_:(mkBarString)

  println()

  mkBarString andThenByName_: mkFoo

  println()
  
  mkFoo.andThenByName_:(mkBarString)
}
