abstract class Foo
{
        val x : Bar
}

abstract class Bar

object Test
        extends Foo with App
{
        object x extends Bar
}
