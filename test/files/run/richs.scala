trait RichTest {
  val s1 = """abc"""
  val s2 = """abc\txyz\n"""
  val s3 = """abc
              xyz"""
  val s4 = """abc
              |xyz"""
  val s5 = """abc
              #xyz"""
  def getObjectName: String = {
    val cn = this.getClass().getName()
    cn.substring(0, cn.length-1)
  }
  def length[A](it: Iterator[A]) = it.toList.length
  def length[A](it: Iterable[A]) = it.toList.length
  def run: Unit
}
object RichCharTest1 extends RichTest {
  def run {
    println("\n" + getObjectName + ":")
    println('\40'.isWhitespace)
    println('\011'.isWhitespace)
    println('1'.asDigit == 1)
    println('A'.asDigit == 10)
  }
}
// object RichCharTest2 extends RichTest {
//   case class C(s: String) {
//     private val it = s.iterator
//     private var c: Char = _
//     def ch(): Char = c
//     def nextch(): Unit = { c = if (it.hasNext) it.next else ';' }
//     def err(msg: String) = println(msg)
//     nextch()
//   }
//   def run {
//     println("\n" + getObjectName + ":")
//     val c1 = C("x4A;")
//     val s1 = xml.Utility.parseCharRef(c1.ch, c1.nextch, c1.err)
//     val c2 = C("74;")
//     val s2 = xml.Utility.parseCharRef(c2.ch, c2.nextch, c2.err)
//     println(s1 == s2)
//   }
// }
object RichIntTest extends RichTest {
  private val n = 10
  private val m = -2
  def run {
    println("\n" + getObjectName + ":")
    println(length(0 until n))
    println(length(0 to n))
    println(length(m until n))
    println(length(m to n))
    println(length(n until m))
    println(length(n to m))

    println(16.toBinaryString) // should be "10000"
    println(16.toHexString)    // should be "10"
    println(16.toOctalString)  // should be "20"

    println(65537.toHexString) // should be "10001"
    println((-1).toHexString)  // should be "ffffffff"
  }
}
object RichStringTest1 extends RichTest {
  def run {
    println("\n" + getObjectName + ":")
    println("s1: " + s1)
    println("s2: " + s2)
    println("s3: " + s3)
    println("s4: " + s4)
    println("s5: " + s5)
  }
}
object RichStringTest2 extends RichTest {
  def run {
    println("\n" + getObjectName + ":")
    Console.print("s1: "); s1.lines foreach println
    Console.print("s2: "); s2.lines foreach println
    Console.print("s3: "); s3.lines foreach println
    Console.print("s4: "); s4.lines foreach println
    Console.print("s5: "); s5.lines foreach println
  }
}
object RichStringTest3 extends RichTest {
  def run {
    println("\n" + getObjectName + ":")
    println("s1: " + s1.stripLineEnd)
    println("s2: " + s2.stripLineEnd)
    println("s3: " + s3.stripLineEnd)
    println("s4: " + s4.stripLineEnd)
    println("s5: " + s5.stripLineEnd)
  }
}
object RichStringTest4 extends RichTest {
  def run {
    println("\n" + getObjectName + ":")
    println("s1: " + s1.stripMargin)
    println("s2: " + s2.stripMargin)
    println("s3: " + s3.stripMargin)
    println("s4: " + s4.stripMargin)
    println("s5: " + s5.stripMargin)
  }
}
object RichStringTest5 extends RichTest {
  def run {
    println("\n" + getObjectName + ":")
    println("s1: " + s3.stripMargin('#'))
    println("s2: " + s3.stripMargin('#'))
    println("s3: " + s3.stripMargin('#'))
    println("s4: " + s4.stripMargin('#'))
    println("s5: " + s5.stripMargin('#'))
  }
}
object RichStringTest6 extends RichTest {
  def run {
    println("a:b:c:d".split(':').toList)
    println("a.b.c.d".split('.').toList)
    println("a$b$c$d".split('$').toList)
    println("a^b^c^d".split('^').toList)
    println("a\\b\\c\\d".split('\\').toList)
    println("a:b:c.d".split(Array(':', '.')).toList)
    println("a:b.c$d".split(Array(':', '.', '$')).toList)
  }
}
/** xxx */
object Test {
  def main(args: Array[String]) {
    RichCharTest1.run
    //RichCharTest2.run
    RichIntTest.run
    RichStringTest1.run
    RichStringTest2.run
    RichStringTest3.run
    RichStringTest4.run
    RichStringTest5.run
    RichStringTest6.run
  }
}
