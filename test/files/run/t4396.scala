// #43896 
trait M extends DelayedInit {
    def delayedInit(body : => Unit) {
        println("hallo")
        body
        println("bye")
    }
}

class C(init : Int) extends M {
    def foo = init
    println("constructor")
    var x = init
    println("out:"+x)
}

// #4380
object Main {
  def main(argv: Array[String]) {
    class Bip {
      class Foo { override def toString() = "foo" }
      object Main extends App {
        val cbn = new Foo()
      }
      Main.main(Array())
      println(Main.cbn)
    }
    new Bip
  }
}

object Test extends App {
  new C(22)
  Main.main(Array())
}
