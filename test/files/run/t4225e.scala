//> using options -Yrangepos
//
import scala.language.implicitConversions

object Test extends App {
  class Foo {
    class Bar
    object Bar {
      implicit def fromString(a: String): Bar = new Bar
    }

    def andThen_:(b : Bar) = { println("pre") ; b ; println("post") }
    def andThenByName_:(b : => Bar) = { println("pre") ; b ; println("post") }
  }

  def mkFoo: Foo = { println("foo") ; new Foo }
  def mkBarString: String = { println("bar"); "Bar" }

  mkBarString andThen_: mkFoo

  println()

  mkFoo.andThen_:(mkBarString)

  println()

  // bar should be deferred but isn't due to scala/bug#10693
  mkBarString andThenByName_: mkFoo

  println()
  
  mkFoo.andThenByName_:(mkBarString)
}
