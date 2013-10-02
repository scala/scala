class A {
    def f(x : Boolean) : Thread = {
        g {
            x match {
                case false =>
                    B.h { }
            }
        }
    }

    private def g[T](block : => T) = sys.error("")
}
object B {
    def h(block : => Unit) : Nothing = sys.error("")
}
