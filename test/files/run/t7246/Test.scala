object Test extends App {

  val so = new SubOuter
  val si = new so.SubInner
  println(si.bar)
}

class SubOuter extends Outer {

  val foo = "hello"

  class SubInner extends Inner {
    def bar = foo
  }

}