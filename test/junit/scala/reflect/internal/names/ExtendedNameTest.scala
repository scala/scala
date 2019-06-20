package scala.reflect.internal.names

import java.util
import java.util.concurrent.{CyclicBarrier, Executors, TimeUnit}

import org.junit.Assert._
import org.junit.Test

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Random

abstract class ExtendedNameTest {
  type T <: AnyRef
  val nameTable: NameTable[T]
  def newTermName(s: String) = {
    nameTable.find(s)
  }
  def newTermName(c:Array[Char], off:Int, len:Int) = {
    nameTable.find(c,off,len)
  }
  lazy val sources = Array.tabulate(10000)( i => s"n$i")

  @Test
  def checkSimple(): Unit = {
    assertEquals(0, nameTable.size)
    val n1 = newTermName("xx")
    assertEquals(1, nameTable.size)

    val n2 = newTermName("xx")
    assertEquals(1, nameTable.size)
    assertSame(n1,n2)

    val n3 = newTermName(new String("xx"))
    assertEquals(1, nameTable.size)
    assertSame(n1,n3)
    assertEquals(n1,n3)
  }
  @Test def checkByCharsAndByString() {
    assertEquals(0, nameTable.size)
    val byString = sources map newTermName

    val byChars = sources map {
      s =>
        newTermName(s.toCharArray, 0, s.length)
    }
    val byChars2 = sources map {
      s =>
        newTermName("abc".toCharArray ++ s.toCharArray ++ "xyz", 3, s.length)
    }
    assertEquals(sources.length, nameTable.size)
    for (i <- 0 until sources.length) {
      assertSame(byString(i), byChars(i))
      assertSame(byString(i), byChars2(i))
    }

  }
  @Test def checkByCharsAndByString2() {
    assertEquals(0, nameTable.size)
    //same as previous but do the chars first
    val byChars = sources map {
      s =>
        newTermName(s.toCharArray, 0, s.length)
    }
    val byString = sources map newTermName

    val byChars2 = sources map {
      s =>
        newTermName("abc".toCharArray ++ s.toCharArray ++ "xyz", 3, s.length)
    }
    assertEquals(sources.length, nameTable.size)
    for (i <- 0 until sources.length) {
      assertSame(byString(i), byChars(i))
      assertSame(byString(i), byChars2(i))
    }

  }
  @Test
  def checkSources1(): Unit = {
    assertEquals(0, nameTable.size)
    val terms = sources map newTermName

    assertEquals(sources.length, nameTable.size)
    val different = terms.toSet
    assertEquals(sources.length, different.size)

    val shuffled: List[Int] = Random.shuffle(List.tabulate(sources.length)(i => i))

    shuffled.foreach{
      i =>
        val again = newTermName(new String(sources(i)))
        assertSame(terms(i), again)
        assertEquals(terms(i), again)
    }
  }

  @Test
  def checkSources2(): Unit = {
    assertEquals(0, nameTable.size)
    lookupInRandom
  }
  def lookupInRandom = {
    val random = new Random()
    val size = sources.size
    val all = new Array[AnyRef](sources.length)
    for (count <- 0 until size * 10) {
      val i = random.nextInt(size)
      val term = newTermName(new String(sources(i)))
      if (null eq all(i) ) {
        all(i) = term
      } else {
        assertSame(all(i), term)
      }
    }

    for (i <- 0 until size) {
      val term = newTermName(new String(sources(i)))
      if (null eq all(i) ) {
        all(i) = term
      } else {
        assertSame(all(i), term)
      }
    }
    all
  }
}

