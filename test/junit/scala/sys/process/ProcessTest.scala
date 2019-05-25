package scala.sys.process

import java.io.{ByteArrayInputStream, File}
import java.nio.file.{Files, Paths}, Files.createTempFile
import java.nio.charset.StandardCharsets.UTF_8

import scala.io.{Source => IOSource}
import scala.util.Try
// should test from outside the package to ensure implicits work
//import scala.sys.process._
import scala.util.Properties._
import scala.collection.JavaConverters._
import scala.tools.testkit.AssertUtil._

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import org.junit.Assert._

@RunWith(classOf[JUnit4])
class ProcessTest {
  private def testily(body: => Unit) = if (!isWin) body
  private val tempFiles = Seq(File.createTempFile("foo", "tmp"), File.createTempFile("bar", "tmp"))

  @Test def t10007(): Unit = testily {
    val res = ("cat" #< new ByteArrayInputStream("lol".getBytes)).!!
    assertEqualTo("lol\n")(res)
  }
  // test non-hanging
  @Test def t10055(): Unit = testily {
    val res = ("cat" #< ( () => -1 ) ).!
    assertEqualTo(0)(res)
  }

  @Test def t10953(): Unit = {
    val res = Process.cat(tempFiles).!
    assertEqualTo(0)(res)
  }

  @Test def processApply(): Unit = {
    val res = Process("cat", tempFiles.map(_.getAbsolutePath)).!
    assertEqualTo(0)(res)
  }

  @Test def t10696(): Unit = {
    val res1 = Process("false").lazyLines
    assertEqualTo("LazyList(<not computed>)")(res1.toString())
    val ex = Try(res1.head).failed.get
    assert(ex.isInstanceOf[RuntimeException])

    val res2 = Process("true").lazyLines
    assertEqualTo("LazyList(<not computed>)")(res2.toString())
    assert(res2.isEmpty)
  }

  @Test def t10823(): Unit = {
    def createFile(prefix: String) = {
      val file = createTempFile(prefix, "tmp")
      Files.write(file, List(prefix).asJava, UTF_8)
      file
    }
    val file1 = createFile("hello")
    val file2 = createFile("world")
    val out = createTempFile("out", "tmp")
    val outf = out.toFile

    try {
      val cat = Process.cat(List(file1, file2).map(_.toFile))
      val p = cat #> outf

      assertEquals(0, p.!)

      val src = IOSource.fromFile(outf)
      try {
        assertEquals("hello, world", src.mkString.linesIterator.mkString(", "))
      } finally {
        src.close()
      }
    } finally {
      Files.delete(file1)
      Files.delete(file2)
      Files.delete(out)
    }
  }

  // a test for A && B where A fails and B is not started
  @Test def t10823_short_circuit(): Unit = testily {
    def createFile(prefix: String) = {
      val file = createTempFile(prefix, "tmp")
      Files.write(file, List(prefix).asJava, UTF_8)
      file
    }
    val noFile = Paths.get("total", "junk")
    val p2 = new ProcessMock(false)
    val failed = new java.util.concurrent.atomic.AtomicBoolean
    val pb2 = new ProcessBuilderMock(p2, error = true) {
      override def run(io: ProcessIO): Process = {
        failed.set(true)
        super.run(io)
      }
    }

    val out = createTempFile("out", "tmp")
    val outf = out.toFile

    val stackless = stacklessly {
      try {
        val p0 = (noFile.toFile : ProcessBuilder.Source).cat #&& pb2
        val p = p0 #> outf

        assertEquals(1, p.!)
        assertFalse(failed.get)
      } finally {
        Files.delete(out)
      }
    }
    stackless.run()
    stackless.assert()
    stackless.getErrors match {
      case (_, (_: java.io.IOException)) :: Nil => ()
      case other => fail(s"Expected one IOException, got $other")
    }
    assertFalse(stackless.getResult.isEmpty)
  }
}
