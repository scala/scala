//> using options -Yimports:scala,scala.Predef,hello.world
//
import hello.world.{Numb => _, _}  // no effect, world isPackage

class C {
  val v: Numb = 42
  def greet() = println("hello, world!")
}
