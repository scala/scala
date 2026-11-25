import annotation.tailrec

// The operand of `throw` and the right-hand side of an assignment are not in tail position
class C {
  var x: Any = null
  @tailrec final def thr(n: Int): Exception = if (n == 0) new Exception else throw thr(n - 1)
  @tailrec final def thrMatch(n: Int): Exception = if (n == 0) new Exception else throw (n match { case _ => thrMatch(n - 1) })
  @tailrec final def asg(n: Int): Unit = if (n > 0) x = asg(n - 1)
  def local(): Any = {
    var y: Any = null
    @tailrec def asg(n: Int): Unit = if (n > 0) y = asg(n - 1)
    asg(1)
    y
  }
  @tailrec final def asgUncaptured(n: Int): Unit = { var z: Any = null; if (n > 0) z = asgUncaptured(n - 1) }
  @tailrec final def assignMatch(n: Int): Unit = { var x: Any = null; x = n match { case 0 => 0; case _ => assignMatch(n - 1) } }
  @tailrec final def assignMatchP(n: Int): Unit = { var x: Any = null; x = Option(n) match { case Some(0) => 0; case _ => assignMatchP(n - 1) } }
  // a `return` nested in such an operand is conservatively rejected
  @tailrec final def thrReturn(n: Int): Int = if (n == 0) 0 else throw (if (n < 0) new Exception else return thrReturn(n - 1))
}
