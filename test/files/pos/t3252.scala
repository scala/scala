class A {
    def f(x : Boolean) : Thread = {
        g {
            x match {
                case false =>
                    B.h { }
            }
        }
    }

    private def g[T](block : => T) = error("")
}
object B {
    def h(block : => Unit) : Nothing = error("")
}