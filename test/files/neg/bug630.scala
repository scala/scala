trait Req1

trait Req2 {
        def test() = Console.println("Test")
}

trait Foo {
        val foo : Req1
}

trait Bar {
        val foo : Req2
        def test() = foo.test
}

object Test
        extends Foo
        with Bar
{
        object foo extends Req1

        def main(argv : Array[String]) = test
}
