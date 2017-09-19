class C {
  def f1(x: Unit): Int = 0
  def f2(y: Unit): Int = 0

  def t1 = {
    var x = 0
    f1(x = 1) // named arg in 2.13 (value discard), not ambiguous
    f2(x = 1) // error, no parameter named x. error message mentions change in 2.13
  }

  def t2 = {
    val x = 0
    f1(x = 1) // ok, named arg (value discard)
    f2(x = 1) // error (no such parameter). no mention of new semantics in 2.13 
  }
}
