
class X(val value: Object) extends AnyVal { def or(alt: => X): X = this }
class Y { def f = new X("") or new X("") }
