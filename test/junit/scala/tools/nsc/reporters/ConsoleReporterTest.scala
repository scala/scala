package scala
package tools.nsc
package reporters

import java.io.{ByteArrayOutputStream, StringReader, BufferedReader, PrintStream, PrintWriter}
import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.reflect.internal.util._


@RunWith(classOf[JUnit4])
class ConsoleReporterTest {
  val source = "Test_ConsoleReporter"
  val batchFile = new BatchSourceFile(source, "For testing".toList)
  val posWithSource = new OffsetPosition(batchFile, 4)
  val content = posWithSource.lineContent
  val writerOut = new ByteArrayOutputStream()
  val echoWriterOut = new ByteArrayOutputStream()


  def createConsoleReporter(inputForReader: String, errOut: ByteArrayOutputStream, echoOut: ByteArrayOutputStream = null): ConsoleReporter = {
    val reader = new BufferedReader(new StringReader(inputForReader))
      
    /** Create reporter with the same writer and echoWriter if echoOut is null */
    echoOut match {
      case null => new ConsoleReporter(new Settings(), reader, new PrintWriter(errOut))
      case _ => new ConsoleReporter(new Settings(), reader, new PrintWriter(errOut), new PrintWriter(echoWriterOut))
    }
  }


  def testHelper(pos: Position = NoPosition, msg: String, severity: String = "")(test: Position => Unit) = {
    test(pos)
    if (msg.isEmpty && severity.isEmpty) assertTrue(writerOut.toString.isEmpty)
    else {
      if (!pos.isDefined) assertEquals(severity + msg, writerOut.toString.lines.next)
      else {
        val it = writerOut.toString.lines
        assertEquals(source + ":1: " + severity + msg, it.next)
        assertEquals(content, it.next)
        assertEquals("    ^", it.next)
      }
    }
    writerOut.reset
  }

  
  @Test
  def printMessageTest(): Unit = {
    val reporter = createConsoleReporter("r", writerOut)
    testHelper(msg = "Hello World!")(_ => reporter.printMessage("Hello World!"))
    testHelper(msg = "Testing with NoPosition")(reporter.printMessage(_, "Testing with NoPosition"))
    testHelper(posWithSource, "Testing with Defined Position")(reporter.printMessage(_, "Testing with Defined Position"))
  }


  @Test
  def echoTest(): Unit = {
    val reporter = createConsoleReporter("r", writerOut, echoWriterOut)
    reporter.echo("Hello World!")
    assertEquals("Hello World!", echoWriterOut.toString.lines.next)

    /** Check with constructor which has the same writer and echoWriter */
    val reporter2 = createConsoleReporter("r", writerOut)
    testHelper(msg = "Hello World!")(_ => reporter2.echo("Hello World!"))
  }


  @Test
  def printTest(): Unit = {
    val reporter = createConsoleReporter("r", writerOut)
    testHelper(msg = "test")(reporter.print(_, "test", reporter.INFO))
    testHelper(msg = "test", severity = "warning: ")(reporter.print(_, "test", reporter.WARNING))
    testHelper(msg = "test", severity = "error: ")(reporter.print(_, "test", reporter.ERROR))
    testHelper(posWithSource, msg = "test")(reporter.print(_, "test", reporter.INFO))
    testHelper(posWithSource, msg = "test", severity = "warning: ")(reporter.print(_, "test", reporter.WARNING))
    testHelper(posWithSource, msg = "test", severity = "error: ")(reporter.print(_, "test", reporter.ERROR))
  }


  @Test
  def printColumnMarkerTest(): Unit = {
    val reporter = createConsoleReporter("r", writerOut)
    testHelper(msg = "")(reporter.printColumnMarker(_))

    reporter.printColumnMarker(posWithSource)
    assertEquals("    ^", writerOut.toString.lines.next)
    writerOut.reset
  }


  @Test
  def displayTest(): Unit = {
    val reporter = createConsoleReporter("r", writerOut)

    /** Change maxerrs and maxwarns from default */
    reporter.settings.maxerrs.value = 1
    reporter.settings.maxwarns.value = 1

    testHelper(msg = "Testing display")(reporter.display(_, "Testing display", reporter.INFO))
    testHelper(msg = "Testing display", severity = "warning: ")(reporter.display(_, "Testing display", reporter.WARNING))
    testHelper(msg = "Testing display", severity = "error: ")(reporter.display(_, "Testing display", reporter.ERROR))
    testHelper(posWithSource, msg = "Testing display")(reporter.display(_, "Testing display", reporter.INFO))
    testHelper(posWithSource, msg = "Testing display", severity = "warning: ")(reporter.display(_, "Testing display", reporter.WARNING))
    testHelper(posWithSource, msg = "Testing display", severity = "error: ")(reporter.display(_, "Testing display", reporter.ERROR))

    reporter.resetCount(reporter.ERROR)
    reporter.resetCount(reporter.WARNING)

    reporter.ERROR.count += 1
    testHelper(posWithSource, msg = "Testing display for maxerrs to pass", severity = "error: ")(reporter.display(_, "Testing display for maxerrs to pass", reporter.ERROR))
    reporter.ERROR.count += 1
    testHelper(msg = "")(reporter.display(_, "Testing display for maxerrs to fail", reporter.ERROR))

    reporter.WARNING.count += 1
    testHelper(posWithSource, msg = "Testing display for maxwarns to pass", severity = "warning: ")(reporter.display(_, "Testing display for maxwarns to pass", reporter.WARNING))
    reporter.WARNING.count += 1
    testHelper(msg = "")(reporter.display(_, "Testing display for maxwarns to fail", reporter.WARNING))
  }


  @Test
  def finishTest(): Unit = {
    val reporter = createConsoleReporter("r", writerOut)

    reporter.resetCount(reporter.ERROR)
    reporter.resetCount(reporter.WARNING)
    testHelper(msg = "")(_ => reporter.finish())

    reporter.ERROR.count = 10
    reporter.WARNING.count = 3
    reporter.finish()
    val it = writerOut.toString.lines
    assertEquals("three warnings found", it.next)
    assertEquals("10 errors found", it.next)
    writerOut.reset
  }


  @Test
  def displayPromptTest(): Unit = {
    val output = "a)bort, s)tack, r)esume: "

    /** Check for stack trace */
    val reporter = createConsoleReporter("s", writerOut, echoWriterOut)
    reporter.displayPrompt()
    val it = writerOut.toString.lines
    assertTrue(it.next.isEmpty)
    assertEquals(output + "java.lang.Throwable", it.next)
    assertTrue(it.hasNext)
    
    /** Check for no stack trace */
    val writerOut2 = new ByteArrayOutputStream()
    val reporter2 = createConsoleReporter("w", writerOut2)
    reporter2.displayPrompt()
    val it2 = writerOut2.toString.lines
    assertTrue(it2.next.isEmpty)
    assertEquals(output, it2.next)
    assertFalse(it2.hasNext)

    /** Check for no stack trace */
    val writerOut3 = new ByteArrayOutputStream()
    val reporter3 = createConsoleReporter("r", writerOut3)
    reporter3.displayPrompt()
    val it3 = writerOut3.toString.lines
    assertTrue(it3.next.isEmpty)
    assertEquals(output, it3.next)
    assertFalse(it3.hasNext)
  }
}
