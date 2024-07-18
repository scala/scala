//> using options -Werror
sealed abstract class Foo {
    def bar(): Unit = this match {
        case Foo_1() => //do something
        case Foo_2() => //do something
        // Works fine
    }

    def baz(that: Foo): Unit = (this, that) match {
        case (Foo_1(), _) => //do something
        case (Foo_2(), _) => //do something
        // match may not be exhaustive
    }
}
case class Foo_1() extends Foo
case class Foo_2() extends Foo
