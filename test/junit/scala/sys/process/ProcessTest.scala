package scala.sys.process

import java.io.{ByteArrayInputStream, File, InputStream, IOException}
import java.nio.file.{Files, Paths}, Files.createTempFile
import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicBoolean

import scala.collection.JavaConverters._
import scala.io.{Source => IOSource, StdIn}
import scala.tools.testkit.AssertUtil._
import scala.tools.testkit.ReleasablePath._
import scala.util.Try
// should test from outside the package to ensure implicits work
//import scala.sys.process._
import scala.util.Properties._
import scala.util.Using
import scala.util.chaining._

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import org.junit.Assert._

@RunWith(classOf[JUnit4])
class ProcessTest {
  private def testily(body: => Unit) = if (!isWin) body

  private def withIn[A](in: InputStream)(body: => A): A = {
    val saved = System.in
    System.setIn(in)
    try body
    finally System.setIn(saved)
  }

  // under the old regime, the thread to copy input would block on the read
  // until after the latch, which is after the innocuous process exited,
  // and then attempt to close the process.getOutputStream, which is known
  // to be closed already (because that is how process exit was detected).
  @Test def t7963(): Unit = testily {
    var exception: Exception = null
    val latch: CountDownLatch = new CountDownLatch(1)
    val inputStream = new ByteArrayInputStream("a".getBytes) {
      override def read(b: Array[Byte], off: Int, len: Int): Int = {
        try {
          latch.await(10, TimeUnit.SECONDS)
        } catch {
          case e: Exception => exception = e
        }
        super.read(b, off, len)
      }
    }
    withIn(inputStream) {
      Process("echo -n").run(true).exitValue()
      latch.countDown()
    }
    assertNull(exception)
  }

  @Test def t10007(): Unit = testily {
    val res = ("cat" #< new ByteArrayInputStream("lol".getBytes)).!!
    assertEquals("lol\n", res)
  }
  // test non-hanging
  @Test def t10055(): Unit = testily {
    val res = ("cat" #< ( () => -1 ) ).!
    assertEquals(0, res)
  }

  @Test def t10953(): Unit =
    Using.resources(File.createTempFile("foo", "tmp"), File.createTempFile("bar", "tmp")) {
      (foo, bar) => assertEquals(0, Process.cat(Seq(foo, bar)).!)
    }

  @Test def processApply(): Unit = testily {
    Using.resources(File.createTempFile("foo", "tmp"), File.createTempFile("bar", "tmp")) {
      (foo, bar) => assertEquals(0, Process("cat", Seq(foo, bar).map(_.getAbsolutePath)).!)
    }
  }

  @Test def t10696(): Unit = testily {
    val res1 = Process("false").lazyLines
    assertEquals("LazyList(<not computed>)", res1.toString())
    val ex = Try(res1.head).failed.get
    assert(ex.isInstanceOf[RuntimeException])

    val res2 = Process("true").lazyLines
    assertEquals("LazyList(<not computed>)", res2.toString())
    assert(res2.isEmpty)
  }

  private def createFile(prefix: String) = createTempFile(prefix, "tmp").tap(f => Files.write(f, List(prefix).asJava, UTF_8))

  @Test def t10823(): Unit =
    Using.resources(createFile("hello"), createFile("world"), createTempFile("out", "tmp")) { (file1, file2, out) =>
      val cat = Process.cat(List(file1, file2).map(_.toFile))
      val p = cat #> out.toFile

      assertEquals(0, p.!)
      Using.resource(IOSource.fromFile(out.toFile))(src => assertEquals("hello, world", src.mkString.linesIterator.mkString(", ")))
    }

  // a test for A && B where A fails and B is not started
  @Test def t10823_short_circuit(): Unit = {

    val noFile = Paths.get("total", "junk")
    val p2     = new ProcessMock(error = false)
    val failed = new AtomicBoolean
    val pb2    = new ProcessBuilderMock(p2, error = true) {
      override def run(io: ProcessIO): Process = {
        failed.set(true)
        super.run(io)
      }
    }
    def process = Using.resource(createTempFile("out", "tmp")) { out =>
      val p0 = (noFile.toFile : ProcessBuilder.Source).cat #&& pb2
      val p = p0 #> out.toFile

      assertEquals(1, p.!)
      assertFalse(failed.get)
    }

    def fail(why: String): Option[Throwable] = Some(new AssertionError(why))

    withoutATrace(process) {
      case (None, _)                         => fail("No main result")
      case (_, (_, (_: IOException)) :: Nil) => None
      case (_, other)                        => fail(s"Expected one IOException, got $other")
    }
  }
}
