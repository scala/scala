class Foo private (val value : Int)

abstract class Bar(val ctor : (Int) => Foo)

object Foo extends Bar(new Foo(_)) //<--- ILLEGAL ACCESS