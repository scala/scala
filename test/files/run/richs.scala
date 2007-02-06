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
  def length[A](it: Iterator[A]) = it.toList length
  def run: Unit
}
object RichCharTest1 extends RichTest {
  def run {
    Console.println("\n" + getObjectName + ":")
    Console.println('\40'.isWhitespace)
    Console.println('\011'.isWhitespace)
    Console.println('1'.asDigit == 1)
    Console.println('A'.asDigit == 10)
  }
}
// object RichCharTest2 extends RichTest {
//   case class C(s: String) {
//     private val it = s.elements
//     private var c: Char = _
//     def ch(): Char = c
//     def nextch(): Unit = { c = if (it.hasNext) it.next else ';' }
//     def err(msg: String) = Console.println(msg)
//     nextch()
//   }
//   def run {
//     Console.println("\n" + getObjectName + ":")
//     val c1 = C("x4A;")
//     val s1 = xml.Utility.parseCharRef(c1.ch, c1.nextch, c1.err)
//     val c2 = C("74;")
//     val s2 = xml.Utility.parseCharRef(c2.ch, c2.nextch, c2.err)
//     Console.println(s1 == s2)
//   }
// }
object RichIntTest extends RichTest {
  private val n = 10
  private val m = -2
  def run {
    Console.println("\n" + getObjectName + ":")
    Console.println(length(0 until n))
    Console.println(length(0 to n))
    Console.println(length(m until n))
    Console.println(length(m to n))
    Console.println(length(n until m))
    Console.println(length(n to m))
  }
}
object RichStringTest1 extends RichTest {
  def run {
    Console.println("\n" + getObjectName + ":")
    Console.println("s1: " + s1)
    Console.println("s2: " + s2)
    Console.println("s3: " + s3)
    Console.println("s4: " + s4)
    Console.println("s5: " + s5)
  }
}
object RichStringTest2 extends RichTest {
  def run {
    Console.println("\n" + getObjectName + ":")
    Console.print("s1: "); s1.lines foreach Console.println
    Console.print("s2: "); s2.lines foreach Console.println
    Console.print("s3: "); s3.lines foreach Console.println
    Console.print("s4: "); s4.lines foreach Console.println
    Console.print("s5: "); s5.lines foreach Console.println
  }
}
object RichStringTest3 extends RichTest {
  def run {
    Console.println("\n" + getObjectName + ":")
    Console.println("s1: " + s1.stripLineEnd)
    Console.println("s2: " + s2.stripLineEnd)
    Console.println("s3: " + s3.stripLineEnd)
    Console.println("s4: " + s4.stripLineEnd)
    Console.println("s5: " + s5.stripLineEnd)
  }
}
object RichStringTest4 extends RichTest {
  def run {
    Console.println("\n" + getObjectName + ":")
    Console.println("s1: " + s1.stripMargin)
    Console.println("s2: " + s2.stripMargin)
    Console.println("s3: " + s3.stripMargin)
    Console.println("s4: " + s4.stripMargin)
    Console.println("s5: " + s5.stripMargin)
  }
}
object RichStringTest5 extends RichTest {
  def run {
    Console.println("\n" + getObjectName + ":")
    Console.println("s1: " + s3.stripMargin('#'))
    Console.println("s2: " + s3.stripMargin('#'))
    Console.println("s3: " + s3.stripMargin('#'))
    Console.println("s4: " + s4.stripMargin('#'))
    Console.println("s5: " + s5.stripMargin('#'))
  }
}
object Test {
  def main(args: Array[String]): Unit = {
    RichCharTest1.run
    //RichCharTest2.run
    RichIntTest.run
    RichStringTest1.run
    RichStringTest2.run
    RichStringTest3.run
    RichStringTest4.run
    RichStringTest5.run
  }
}
