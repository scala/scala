object Test
{
        trait Foo { type T }
        object FooX extends Foo { type T = X; trait X }

        def test(x : Foo { type T = FooX.X }) = {}

        def main(argv : Array[String]) : Unit = {
                test(FooX)
        }
}
