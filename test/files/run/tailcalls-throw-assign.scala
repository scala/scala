import annotation.tailrec

class C {
  var x: Any = "init"

  // The operand of `throw` and the right-hand side of an assignment are not in tail position
  final def thr(n: Int): Exception = if (n == 0) new Exception("base") else throw thr(n - 1)
  final def thrMatch(n: Int): Exception = if (n == 0) new Exception("base") else throw (n match { case _ => thrMatch(n - 1) })
  final def asg(n: Int): Unit = if (n > 0) x = asg(n - 1)
  def asgLocal(): Any = {
    var y: Any = "init"
    def asg(n: Int): Unit = if (n > 0) y = asg(n - 1)
    asg(1)
    y
  }

  // The operand of `return` and of a type ascription still are, as is the expression after a loop
  @tailrec final def ret(n: Int): Int = {
    if (n == 0) return 0
    return (ret(n - 1): Int)
  }
  @tailrec final def loop(n: Int): Int = {
    var i = 0
    while (i < 1) i += 1
    if (n == 0) 0 else loop(n - 1)
  }
}

object Test {
  def t(f: => Any): Unit = println(try f catch { case e: Exception => s"thrown ${e.getMessage}" })
  def main(args: Array[String]): Unit = {
    val c = new C
    t(c.thr(2))
    t(c.thrMatch(2))
    t({ c.asg(2); c.x })
    t(c.asgLocal())
    t(c.ret(1000000))
    t(c.loop(1000000))
  }
}
