class Meter(val x: Double) extends AnyVal

class Foo {
 def apply(x: Double) = x.toString
 def apply(x: Meter) = x.toString
}
