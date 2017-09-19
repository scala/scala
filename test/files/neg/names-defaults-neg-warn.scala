object Test extends App {
  object deprNam2 {
    def f(@deprecatedName('s) x: String) = 1
    def f(s: Object) = 2

    def g(@deprecatedName('x) s: Object) = 3
    def g(s: String) = 4
  }

  deprNam2.f(s = new Object)
  deprNam2.f(s = "dlfkj")
  deprNam2.g(x = "dlkjf")
  deprNam2.g(s = new Object)
}

class C {
  def f1(x: Unit): Unit = ()
  def f2(y: Unit): Unit = ()

  def t1 = {
    var x = 0
    f1(x = 1) // 2.12: error, ambiguous (named arg or assign). 2.13: named arg
    f2(x = 1) // 2.12: deprecation warning, compiles. 2.13: error, no parameter named x

    // all of the following are assignments to x

    f1((x = 1))
    f2((x = 1))
    f1({ x = 1 })
    f2({ x = 1 })
    f1 { x = 1 }
    f2 { x = 1 }

    synchronized(x = 1) // deprecation warning in 2.12, error in 2.13
    synchronized((x = 1))   // ok
    synchronized({ x = 1 }) // ok
    synchronized { x = 1 }  // ok
  }

  def t2 = {
    val x = 0
    f1(x = 1) // 2.12, 2.13: ok, named arg (value discard)
    f2(x = 1) // 2.12, 2.13: error (no such parameter). no deprecation warning in 2.12, x is not a variable.
  }
}
