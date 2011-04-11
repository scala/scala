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

object Test extends App {
  new C(22)
}
