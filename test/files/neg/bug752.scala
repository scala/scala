object Test
{
        def f(x : Int => Unit) : Unit = ()
        def g(x : String) : Unit = ()
        def main(argv : Array[String]) = {
                f(g _)
        }
}
