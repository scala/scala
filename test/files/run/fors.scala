//############################################################################
// for-comprehensions (old and new syntax)
//############################################################################

//############################################################################

object Test extends App {
  val xs = List(1, 2, 3)
  val ys = List(Symbol("a"), Symbol("b"), Symbol("c"))

  def it = 0 until 10

  val ar = "abc".toCharArray

  /////////////////// old syntax ///////////////////

  def testOld: Unit = {
    println("\ntestOld")

    // lists
    for (x <- xs) print(s"$x "); println
    for (x <- xs;
         if x % 2 == 0) print(s"$x "); println
    for {x <- xs
         if x % 2 == 0} print(s"$x "); println
    var n = 0
    for (_ <- xs) n += 1; println(n)
    for ((x, y) <- xs zip ys) print(s"$x "); println
    for (p @ (x, y) <- xs zip ys) print(s"${p._1} "); println

    // iterators
    for (x <- it) print(s"$x "); println
    for (x <- it;
         if x % 2 == 0) print(s"$x "); println
    for {x <- it
         if x % 2 == 0} print(s"$x "); println

    // arrays
    for (x <- ar) print(s"$x "); println
    for (x <- ar;
         if x.toInt > 97) print(s"$x "); println
    for {x <- ar
         if x.toInt > 97} print(s"$x "); println

  }

  /////////////////// new syntax ///////////////////

  def testNew: Unit = {
    println("\ntestNew")

    // lists
    var n = 0
    for (_ <- xs) n += 1; println(n)
    for ((x, y) <- xs zip ys) print(s"$x "); println
    for (p @ (x, y) <- xs zip ys) print(s"${p._1} "); println

    // iterators
    for (x <- it) print(s"$x "); println
    for (x <- it if x % 2 == 0) print(s"$x "); println
    for (x <- it; if x % 2 == 0) print(s"$x "); println
    for (x <- it;
         if x % 2 == 0) print(s"$x "); println
    for (x <- it
         if x % 2 == 0) print(s"$x "); println
    for {x <- it
         if x % 2 == 0} print(s"$x "); println
    for (x <- it;
         y = 2
         if x % y == 0) print(s"$x "); println
    for {x <- it
         y = 2
         if x % y == 0} print(s"$x "); println

    // arrays
    for (x <- ar) print(s"$x "); println

  }

  ////////////////////////////////////////////////////

  testOld
  testNew
}
