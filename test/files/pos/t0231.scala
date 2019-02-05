class Foo {
  def aaa: Unit = {
    println("a")
  }
}

class Bar extends Foo {
  object b {
    //println("b: " + a) //OK
    println("b: " + Bar.super.aaa)
  }
}

object bug extends App {
  new Bar
  ()
}
