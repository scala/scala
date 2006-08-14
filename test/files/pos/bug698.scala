abstract class Foo
{
        val x : Bar
}

abstract class Bar

object Test
        extends Foo with Application
{
        object x extends Bar
}
