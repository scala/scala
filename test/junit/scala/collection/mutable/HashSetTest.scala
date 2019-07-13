package scala.collection.mutable

import java.io.{ByteArrayOutputStream, PrintStream}
import java.util
import scala.jdk.CollectionConverters._

import org.junit.Test
import org.junit.Assert._

class HashSetTest {
  // based on run/hashset.scala partest
  @Test
  def testPar: Unit = {
    val h1 = new HashSet[Int]
    for (i <- 0 until 20) h1 += i
    for (i <- 0 until 20) assertTrue(h1.contains(i))
    for (i <- 20 until 40) assertFalse(h1.contains(i))
    assertEquals((0 until 20).toList.sorted, h1.toList.sorted)

    val h2 = new HashSet[String]
    h2 += null
    for (i <- 0 until 20) h2 +=  "" + i
    assertTrue(h2 contains null)
    for (i <- 0 until 20) assertTrue(h2.contains("" + i))
    for (i <- 20 until 40) assertFalse(h2.contains("" + i))
    assertEquals((0 until 20).map("" + _).toList.sorted.mkString(",") + ",null", h2.toList.map("" + _).sorted.mkString(","))

    h2 -= null
    h2 -= "" + 0
    assertFalse(h2 contains null)
    assertEquals((1 until 20).map("" + _).toList.sorted.mkString(","), h2.toList.map("" + _).sorted.mkString(","))
  }

  @Test
  def si4894: Unit = {
    val hs = HashSet[Int]()
    hs ++= 1 to 10
    hs --= 1 to 10
    assertTrue(hs.isEmpty)
  }

  @Test
  def addRemove(): Unit = {
    val hs = HashSet[Int]()
    hs += 1
    assertFalse(hs.add(1))
    assertFalse(hs.add(1))
    assertTrue(hs.add(2))
    assertFalse(hs.remove(3))
    assertTrue(hs.remove(2))
    assertTrue(hs.remove(1))
    assertFalse(hs.remove(1))
  }

  @Test
  def iterator: Unit = {
    val hs = HashSet.from(1 to 5)
    val it = hs.iterator
    var s = 0
    while(it.hasNext) s += it.next()
    assertEquals((1 to 5).sum, s)
  }

  @Test
  def equality: Unit = {
    val hs = HashSet[Any](1)
    assertTrue(hs.contains(1.0))
  }

  case class PackageEntryImpl(name: String)

  @Test
  def addConflicting: Unit = {
    val hs = HashSet[PackageEntryImpl](PackageEntryImpl("javax"), PackageEntryImpl("java"))
    assertFalse(hs.add(PackageEntryImpl("java")))
  }


  @Test
  def duplicatePrintln:Unit = {
    val outContent: ByteArrayOutputStream = new ByteArrayOutputStream()
    System.setOut(new PrintStream(outContent))
    val jSet = new util.HashSet[String]()
    jSet.add("hello")
    jSet.asScala.map(System.out.println)
    assertEquals("hello\n", outContent.toString)
  }

}
