object Test {

  def test(i: Int): Int = {
    def escape(x: Int): Int = return x + 1          //     local

    lazy val lzy: Int = return i * 2                // non-local

         if (i == 0) i
    else if (i == 1) return i                       //     local
    else if (i == 2) invoke  { return i }           // non-local
    else if (i == 3) invoke1 { _ => return i }      // non-local
    else if (i == 4) collect { case _ => return i } // non-local; cf. run/t10291.scala
    else if (i == 5) escape(i)
    else if (i == 6) lzy
    else             i
  }

  def invoke[T](fn: => T): T = fn

  def invoke1(f: Int => Int): Int = f(4)

  def collect(f: PartialFunction[Int, Int]): Int = f(5)
}

object Ichoran {

  def trivial(i: Int): Int =
    try {10/i } catch { case _: Exception => return 0 }

  def simple(i: Int): Int =
    ( try { 10/i } catch { case _: Exception => return 0 } ) + 1

  def expanded(i: Int): Int = {
    val temp = try { 10/i } catch { case _: Exception => return 0 }
    temp + 1
  }

  def alternate(i: Int): Int = {
    (try { 10/i } catch { case _: Exception => -1 }) + 1

  }
}
