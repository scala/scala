package scala.reflect.internal.util

import java.io._

import org.junit.Assert._
import org.junit._

class FileUtilsTest {

  @Test def writeIsSame(): Unit = {
    val fileTest = File.createTempFile("FileUtilsTest", "t1")
    val fileExpected = File.createTempFile("FileUtilsTest", "t2")

    val sTest = FileUtils.newAsyncBufferedWriter(new FileWriter(fileTest), false)
    val sExpected = new BufferedWriter(new FileWriter(fileExpected))

    def writeBoth(s:String, asChars: Boolean) = {
      if (asChars) {
        sTest.write(s.toCharArray)
        sExpected.write(s.toCharArray)
      } else {
        sTest.write(s)
        sExpected.write(s)
      }
    }

    for (i <- 1 to 2000) {
      writeBoth(s"line $i text;", true)
      writeBoth(s"line $i chars", false)
      sTest.newLine
      sExpected.newLine
    }
    sTest.close()
    sExpected.close()

    assertEquals(fileExpected.length(),fileTest.length())

    val expIn = new BufferedReader(new FileReader(fileExpected))
    val testIn = new BufferedReader(new FileReader(fileTest))

    var exp = expIn.readLine()
    while (exp ne null) {
      val actual = testIn.readLine()
      assertEquals(exp, actual)
      exp = expIn.readLine()
    }
    expIn.close()
    testIn.close()
    fileTest.delete()
    fileExpected.delete()
  }

  @Test def showPerformance: Unit = {
    //warmup
    for (i <- 1 to 1000) {
      writeIsSame()
    }

    val fileTest = File.createTempFile("FileUtilsTest", "t1")
    val fileExpected = File.createTempFile("FileUtilsTest", "t2")

    for (i <- 1 to 10) {
      val sTest = FileUtils.newAsyncBufferedWriter(fileTest.toPath)
      val sExpected = new BufferedWriter(new FileWriter(fileExpected))

      val t1 = System.nanoTime()
      List.tabulate(10000) {i =>
        sTest.write(s"line $i text;")
        sTest.newLine
      }
      val t2 = System.nanoTime()
      sTest.close()
      val t3 = System.nanoTime()
      List.tabulate(10000) {i =>
        sExpected.write(s"line $i text;")
        sExpected.newLine
      }
      val t4 = System.nanoTime()
      sExpected.close()

      println(s"async    took ${t2 - t1} ns")
      println(s"buffered took ${t4 - t3} ns")

      fileTest.delete()
      fileExpected.delete()
    }
  }

}
