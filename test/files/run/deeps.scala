//############################################################################
// deepEquals / deep.toString
//############################################################################

//############################################################################
// need to revisit array equality
object Test {

  def testEquals1 {
    println(Array(1) == Array(1))
    println(Array(1) equals Array(1))
    println(Array(1).deep == Array(1).deep)
    println
  }

  def testEquals2 {
    println(Array(Array(1), Array(2)) == Array(Array(1), Array(2)))
    println(Array(Array(1), Array(2)) equals Array(Array(1), Array(2)))
    println(Array(Array(1), Array(2)).deep equals Array(Array(1), Array(2)).deep)
    println
  }

  def testEquals3 {
    val a1 = Array(1)
    val b1 = Array(1)
    val a2 = Array(a1, b1)
    val b2 = Array(a1, b1)
    val a3 = Array(a2, b2)
    val b3 = Array(a2, b2)
    def test[T](x: Array[T], y: Array[T]) {
      println("x=" + x.deep.toString)
      println("y=" + y.deep.toString)
      println(x == y)
      println(x equals y)
      println(x.deep == y.deep)
      println
    }
    test(a1, b1)
    test(a2, b2)
    test(a3, b3)
  }

  def testEquals4 {
    println("boo:and:foo".split(':') == "boo:and:foo".split(':'))
    println("boo:and:foo".split(':') equals "boo:and:foo".split(':'))
    println("boo:and:foo".split(':').deep == "boo:and:foo".split(':').deep)

    val xs = new java.util.ArrayList[String](); xs.add("a")
    val ys = new java.util.ArrayList[String](); ys.add("a")
    println(xs.toArray == ys.toArray)
    println(xs.toArray equals ys.toArray)
    println(xs.toArray.deep == ys.toArray.deep)
  }

  def testToString1 {
    def sweep(s: String) = (
      s.replaceAll("D@[0-9a-fA-F]+", "D@0000000")
       .replaceAll("Z@[0-9a-fA-F]+", "Z@0000000")
       .replaceAll(";@[0-9a-fA-F]+", ";@0000000")
    )
    def test[T](a: Array[T]) {
      println(sweep(a.deep.toString))
      println(a.deep.toString)
      println(a.deep.mkString("[", ";", "]"))
      println(a.deep.mkString(";"))
      println
    }

    val ba1 = Array(true, false)
    val ba2 = Array(ba1, ba1)
    val ba3 = Array(ba2, ba2)
    test(ba1)
    test(ba2)
    test(ba3)

    val da1 = Array(1.0d, 0.0d)
    val da2 = Array(da1, da1)
    val da3 = Array(da2, da2)
    test(da1)
    test(da2)
    test(da3)

    val sa1 = Array("a", "b")
    val sa2 = Array(sa1, sa1)
    val sa3 = Array(sa2, sa2)
    test(sa1)
    test(sa2)
    test(sa3)
  }

  def testToString2 {
    println(Array(Array(true, false), Array(false)).deep.mkString("[", "; ", "]"))
    println(Array(Array('1', '2'), Array('3')).deep.mkString("[", "; ", "]"))
    println(Array(Array(1, 2), Array(3)).deep.mkString("[", "; ", "]"))
    println
  }

  def testToString3 {
    println("boo:and:foo".split(':').deep.toString)

    val xs = new java.util.ArrayList[String](); xs.add("a")
    println(xs.toArray.deep.toString)
  }

  def main(args: Array[String]): Unit = {
    println("testEquals1") ; testEquals1
    println("testEquals2") ; testEquals2
    println("testEquals3") ; testEquals3
    println("testEquals4") ; testEquals4
    testToString1
    testToString2
    testToString3
  }
}
