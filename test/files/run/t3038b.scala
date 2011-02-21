class A {
    val a1 = 1
    val a2 = 2
    private val b1 = 3
    private val b2 = 4
    @transient val c1 = 5
    @transient val c2 = 6
    def run = {
        println(a1)
        println(a2)
        println(b1)
        println(b2)
        println(c1)
        println(c2)
    }
}

object Test extends App {
    new A().run
}
