//> using options -Werror -Wmacros:both -Wunused:_

case class Foo(foo: String, flag: Boolean)

object Main extends App {
  val randStr = ""
  println {
    Stub.stub(Foo(flag = true, foo = randStr))
  }
}
