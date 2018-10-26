package scala.sys.process

import java.io.{ByteArrayInputStream, File, PrintWriter}

import scala.io.Source
import java.io.{ByteArrayInputStream, File}

import scala.util.Try
// should test from outside the package to ensure implicits work
//import scala.sys.process._
import scala.util.Properties._

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import org.junit.Assert.assertEquals

@RunWith(classOf[JUnit4])
class ProcessTest {
  private def testily(body: => Unit) = if (!isWin) body
  private val tempFiles = Seq(File.createTempFile("foo", "tmp"), File.createTempFile("bar", "tmp"))
  @Test def t10007(): Unit = testily {
    val res = ("cat" #< new ByteArrayInputStream("lol".getBytes)).!!
    assertEquals("lol\n", res)
  }
  // test non-hanging
  @Test def t10055(): Unit = testily {
    val res = ("cat" #< ( () => -1 ) ).!
    assertEquals(0, res)
  }

  @Test def t10953(): Unit = {
    val res = Process.cat(tempFiles).!
    assertEquals(0, res)
  }

  @Test def processApply(): Unit = {
    val res = Process("cat", tempFiles.map(_.getAbsolutePath)).!
    assertEquals(0, res)
  }

  @Test def t10696(): Unit = {
    val res1 = Process("false").lazyLines
    assertEquals("LazyList(?)", res1.toString())
    val ex = Try(res1.head).failed.get
    assert(ex.isInstanceOf[RuntimeException])

    val res2 = Process("true").lazyLines
    assertEquals("LazyList(?)", res2.toString())
    assert(res2.isEmpty)
  }

  @Test def t10823(): Unit = {
    def createFile(prefix: String): File = {
      val file = File.createTempFile(prefix, "tmp")
      file.deleteOnExit()
      val writer = new PrintWriter(file)
      writer.write(prefix)
      writer.close()
      file
    }
    val file1 = createFile("hello")
    val file2 = createFile("world")
    val out = File.createTempFile("out", "tmp")
    out.deleteOnExit()

    val cat = Process.cat(List(file1, file2))
    val writer = cat #> out
    val res = (writer).!

    assertEquals(0, res)
    assertEquals("helloworld", Source.fromFile(out).mkString)

  }
}
