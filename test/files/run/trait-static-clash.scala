trait T {
        def foo = 1
        def foo(t: T) = 2
}
object Test extends T {
        def main(args: Array[String]): Unit = {
                assert(foo == 1)
                assert(foo(this) == 2)
        }
}
