// no flag enabling it, fail
trait A { object Foo }
trait B extends A  { override object Foo }
