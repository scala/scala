trait RichTest extends Runnable {
  val s1 = """abc"""
  val s2 = """abc\txyz\n"""
  val s3 = """abc
              xyz"""
  val s4 = """abc
              |xyz"""
  val s5 = """abc
              #xyz"""
  def getObjectName: String = getClass.getName.init
  def test(): Unit
  override final def run() = {
    println(s"\n$getObjectName:")
    test()
  }
  def length[A](it: Iterator[A]) = it.toList.length
  def length[A](it: Iterable[A]) = it.toList.length
}

// documents undesirable sign extension
object RichByteTest extends RichTest {
  override def test() = {
    val sixteen = 16.toByte
    println(sixteen.toBinaryString)
    println(sixteen.toHexString)
    println(sixteen.toOctalString)
    val max = 0x7F.toByte
    println(max.toBinaryString)
    println(max.toHexString)
    println(max.toOctalString)
    val extended = 0x80.toByte
    println(extended.toBinaryString)
    println(extended.toHexString)
    println(extended.toOctalString)
    val neg = -1.toByte
    println(neg.toBinaryString)
    println(neg.toHexString)
    println(neg.toOctalString)
  }
}

object RichCharTest extends RichTest {
  override def test() = {
    println('1'.asDigit == 1)
    println('A'.asDigit == 10)
    val sixteen = 16.toChar
    println(sixteen.toBinaryString)
    println(sixteen.toHexString)
    println(sixteen.toOctalString)
    val max = 0x7FFF.toChar
    println(max.toBinaryString)
    println(max.toHexString)
    println(max.toOctalString)
    val extended = 0x8000.toChar
    println(extended.toBinaryString)
    println(extended.toHexString)
    println(extended.toOctalString)
    val neg = -1.toChar
    println(neg.toBinaryString)
    println(neg.toHexString)
    println(neg.toOctalString)
  }
}

// documents undesirable sign extension
object RichShortTest extends RichTest {
  override def test() = {
    val sixteen = 16.toShort
    println(sixteen.toBinaryString)
    println(sixteen.toHexString)
    println(sixteen.toOctalString)
    val max = 0x7FFF.toShort
    println(max.toBinaryString)
    println(max.toHexString)
    println(max.toOctalString)
    val extended = 0x8000.toShort
    println(extended.toBinaryString)
    println(extended.toHexString)
    println(extended.toOctalString)
    val neg = -1.toShort
    println(neg.toBinaryString)
    println(neg.toHexString)
    println(neg.toOctalString)
  }
}

object RichLongTest extends RichTest {
  override def test() = {
    val sixteen = 16L
    println(sixteen.toBinaryString)
    println(sixteen.toHexString)
    println(sixteen.toOctalString)
    val max = 0x7FFFL
    println(max.toBinaryString)
    println(max.toHexString)
    println(max.toOctalString)
    val extended = 0x8000L
    println(extended.toBinaryString)
    println(extended.toHexString)
    println(extended.toOctalString)
    val neg = -1L
    println(neg.toBinaryString)
    println(neg.toHexString)
    println(neg.toOctalString)
  }
}

object RichIntTest extends RichTest {
  private val n = 10
  private val m = -2
  def test(): Unit = {
    println(length(0 until n))
    println(length(0 to n))
    println(length(m until n))
    println(length(m to n))
    println(length(n until m))
    println(length(n to m))

    println(16.toBinaryString)
    println(16.toHexString)
    println(16.toOctalString)

    println(65537.toHexString)
    println((-1).toHexString)
  }
}
object RichStringTest1 extends RichTest {
  def test(): Unit = {
    println("s1: " + s1)
    println("s2: " + s2)
    println("s3: " + s3)
    println("s4: " + s4)
    println("s5: " + s5)
  }
}
object RichStringTest2 extends RichTest {
  def test(): Unit = {
    Console.print("s1: "); s1.linesIterator foreach println
    Console.print("s2: "); s2.linesIterator foreach println
    Console.print("s3: "); s3.linesIterator foreach println
    Console.print("s4: "); s4.linesIterator foreach println
    Console.print("s5: "); s5.linesIterator foreach println
  }
}
object RichStringTest3 extends RichTest {
  def test(): Unit = {
    println("s1: " + s1.stripLineEnd)
    println("s2: " + s2.stripLineEnd)
    println("s3: " + s3.stripLineEnd)
    println("s4: " + s4.stripLineEnd)
    println("s5: " + s5.stripLineEnd)
  }
}
object RichStringTest4 extends RichTest {
  def test(): Unit = {
    println("s1: " + s1.stripMargin)
    println("s2: " + s2.stripMargin)
    println("s3: " + s3.stripMargin)
    println("s4: " + s4.stripMargin)
    println("s5: " + s5.stripMargin)
  }
}
object RichStringTest5 extends RichTest {
  def test(): Unit = {
    println("s1: " + s3.stripMargin('#'))
    println("s2: " + s3.stripMargin('#'))
    println("s3: " + s3.stripMargin('#'))
    println("s4: " + s4.stripMargin('#'))
    println("s5: " + s5.stripMargin('#'))
  }
}
object RichStringTest6 extends RichTest {
  def test(): Unit = {
    println("a:b:c:d".split(':').toList)
    println("a.b.c.d".split('.').toList)
    println("a$b$c$d".split('$').toList)
    println("a^b^c^d".split('^').toList)
    println("a\\b\\c\\d".split('\\').toList)
    println("a:b:c.d".split(Array(':', '.')).toList)
    println("a:b.c$d".split(Array(':', '.', '$')).toList)
  }
}
object Test {
  def main(args: Array[String]): Unit =
    List(
      RichByteTest,
      RichShortTest,
      RichCharTest,
      RichIntTest,
      RichLongTest,
      RichStringTest1,
      RichStringTest2,
      RichStringTest3,
      RichStringTest4,
      RichStringTest5,
      RichStringTest6,
    ).foreach(_.run())
}
