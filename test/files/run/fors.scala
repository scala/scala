//############################################################################
// for-comprehensions (old and new syntax)
//############################################################################

//############################################################################

object Test extends App {
  val xs = List(1, 2, 3)
  val ys = List('a, 'b, 'c)

  def it = 0 until 10

  val ar = "abc".toCharArray

  val xml =
    <html>
      <head><title>Scala</title></head>
      <body>{xs}</body>
    </html>;

  /////////////////// old syntax ///////////////////

  def testOld {
    println("\ntestOld")

    // lists
    for (x <- xs) print(x + " "); println
    for (x <- xs;
         if x % 2 == 0) print(x + " "); println
    for {x <- xs
         if x % 2 == 0} print(x + " "); println
    var n = 0
    for (_ <- xs) n += 1; println(n)
    for ((x, y) <- xs zip ys) print(x + " "); println
    for (p @ (x, y) <- xs zip ys) print(p._1 + " "); println

    // iterators
    for (x <- it) print(x + " "); println
    for (x <- it;
         if x % 2 == 0) print(x + " "); println
    for {x <- it
         if x % 2 == 0} print(x + " "); println

    // arrays
    for (x <- ar) print(x + " "); println
    for (x <- ar;
         if x.toInt > 97) print(x + " "); println
    for {x <- ar
         if x.toInt > 97} print(x + " "); println

    // sequences
    for (x <- xml.child) println(x)
    for (x <- xml.child;
         if x.label == "head") println(x)
  }

  /////////////////// new syntax ///////////////////

  def testNew {
    println("\ntestNew")

    // lists
    var n = 0
    for (_ <- xs) n += 1; println(n)
    for ((x, y) <- xs zip ys) print(x + " "); println
    for (p @ (x, y) <- xs zip ys) print(p._1 + " "); println

    // iterators
    for (x <- it) print(x + " "); println
    for (x <- it if x % 2 == 0) print(x + " "); println
    for (x <- it; if x % 2 == 0) print(x + " "); println
    for (x <- it;
         if x % 2 == 0) print(x + " "); println
    for (x <- it
         if x % 2 == 0) print(x + " "); println
    for {x <- it
         if x % 2 == 0} print(x + " "); println
    for (x <- it;
         val y = 2
         if x % y == 0) print(x + " "); println
    for {x <- it
         val y = 2
         if x % y == 0} print(x + " "); println

    // arrays
    for (x <- ar) print(x + " "); println

    // sequences
    for (x <- xml.child) println(x)
    for (x <- xml.child if x.label == "head") println(x)
  }

  ////////////////////////////////////////////////////

  testOld
  testNew
}
