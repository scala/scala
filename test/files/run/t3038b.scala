class A {
    val a1 = 1
    val a2 = 2
    private val b1 = 3
    private val b2 = 4
    @transient val c1 = 5
    @transient val c2 = 6
    def run = {
        a1
        a2
        b1
        b2
        c1
        c2
    }
}

object Test extends Application {
    new A().run
}