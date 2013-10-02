object Test {

  class Foo(final var i:Int)

  def main(args : Array[String]) : Unit = {
  	val foo = new Foo(0)
  	foo.i += 1
  }
}