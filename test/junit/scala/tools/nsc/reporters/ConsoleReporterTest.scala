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
      
    // Create reporter with the same writer and echoWriter if echoOut is null
    echoOut match {
      case null => new ConsoleReporter(new Settings, reader, new PrintWriter(errOut))
      case _ => new ConsoleReporter(new Settings, reader, new PrintWriter(errOut), new PrintWriter(echoWriterOut))
    }
  }

  def testHelper(pos: Position = NoPosition, msg: String, severity: String = "")(test: Position => Unit) =
    try {
      test(pos)
      val buf = writerOut.toString
      if (msg.isEmpty && severity.isEmpty) assertTrue(s"Expected no message output but saw: [$buf]", buf.isEmpty)
      else if (!pos.isDefined) assertEquals(severity + msg, buf.linesIterator.next)
      else {
        val it = buf.linesIterator
        assertEquals(source + ":1: " + severity + msg, it.next)
        assertEquals(content, it.next)
        assertEquals("    ^", it.next)
      }
    } finally writerOut.reset

  @Test
  def printMessageTest(): Unit = {
    val reporter = createConsoleReporter("r", writerOut)
    testHelper(msg = "Hello World!")(_ => reporter.display(NoPosition, "Hello World!", null))
    testHelper(posWithSource, "Testing with Defined Position")(reporter.display(_, "Testing with Defined Position", null))
  }

  @Test
  def echoTest(): Unit = {
    val reporter = createConsoleReporter("r", writerOut, echoWriterOut)
    reporter.echo("Hello World!")
    assertEquals("Hello World!", echoWriterOut.toString.linesIterator.next)

    /** Check with constructor which has the same writer and echoWriter */
    val reporter2 = createConsoleReporter("r", writerOut)
    testHelper(msg = "Hello World!")(_ => reporter2.echo("Hello World!"))
  }

  @Test
  def printTest(): Unit = {
    val reporter = createConsoleReporter("r", writerOut)
    testHelper(msg = "test")(reporter.display(_, "test", reporter.INFO))
    testHelper(msg = "test", severity = "warning: ")(reporter.display(_, "test", reporter.WARNING))
    testHelper(msg = "test", severity = "error: ")(reporter.display(_, "test", reporter.ERROR))
    testHelper(posWithSource, msg = "test")(reporter.display(_, "test", reporter.INFO))
    testHelper(posWithSource, msg = "test", severity = "warning: ")(reporter.display(_, "test", reporter.WARNING))
    testHelper(posWithSource, msg = "test", severity = "error: ")(reporter.display(_, "test", reporter.ERROR))
  }

  @Test
  def displayTest(): Unit = {
    val reporter = createConsoleReporter("r", writerOut)

    // Change maxerrs and maxwarns from default
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
    val reporter = createConsoleReporter("", writerOut)

    reporter.resetCount(reporter.ERROR)
    reporter.resetCount(reporter.WARNING)
    testHelper(msg = "")(_ => reporter.finish())

    reporter.ERROR.count = 10
    reporter.WARNING.count = 3
    reporter.finish()
    reporter.flush()
    val it = writerOut.toString.linesIterator
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
    val it = writerOut.toString.linesIterator
    assertTrue(it.next.isEmpty)
    assertEquals(output + "java.lang.Throwable", it.next)
    assertTrue(it.hasNext)

    /** Check for no stack trace */
    val writerOut2 = new ByteArrayOutputStream()
    val reporter2 = createConsoleReporter("w", writerOut2)
    reporter2.displayPrompt()
    val it2 = writerOut2.toString.linesIterator
    assertTrue(it2.next.isEmpty)
    assertEquals(output, it2.next)
    assertFalse(it2.hasNext)

    /** Check for no stack trace */
    val writerOut3 = new ByteArrayOutputStream()
    val reporter3 = createConsoleReporter("r", writerOut3)
    reporter3.displayPrompt()
    val it3 = writerOut3.toString.linesIterator
    assertTrue(it3.next.isEmpty)
    assertEquals(output, it3.next)
    assertFalse(it3.hasNext)
  }

  @Test
  def filterTest(): Unit = {
    val reporter = createConsoleReporter("r", writerOut)
    val filter   = {
      // Change maxerrs and maxwarns from default on filter only
      val settings = new Settings
      settings.maxerrs.value  = 1
      settings.maxwarns.value = 1

      new Reporter.LimitingReporter(settings, reporter)
    }

    // pass one message
    testHelper(msg = "Testing display")(filter.echo(_, "Testing display"))
    testHelper(msg = "Testing display", severity = "warning: ")(filter.warning(_, "Testing display"))
    testHelper(msg = "Testing display", severity = "error: ")(filter.error(_, "Testing display"))
    filter.reset()

    testHelper(posWithSource, msg = "Testing display")(filter.echo(_, "Testing display"))
    testHelper(posWithSource, msg = "Testing display", severity = "warning: ")(filter.warning(_, "Testing display"))
    testHelper(posWithSource, msg = "Testing display", severity = "error: ")(filter.error(_, "Testing display"))
    filter.reset()

    // either reset after each test or emit warn before error so that both are output by AbstractReporter
    assertEquals(0, filter.errorCount)
    assertEquals(0, reporter.errorCount)
    assertEquals(0, filter.warningCount)
    assertEquals(0, reporter.warningCount)

    // try to pass two messages
    // warn first; would be nice to flush too
    testHelper(posWithSource, msg = "Testing display for maxwarns to pass", severity = "warning: ")(filter.warning(_, "Testing display for maxwarns to pass"))
    testHelper(msg = "")(filter.warning(_, "Testing display for maxwarns to fail"))

    testHelper(posWithSource, msg = "Testing display for maxerrs to pass", severity = "error: ")(filter.error(_, "Testing display for maxerrs to pass"))
    testHelper(msg = "")(filter.error(_, "Testing display for maxerrs to fail"))
  }

  @Test
  def filteredInfoTest(): Unit = {
    val reporter = new Reporter.LimitingReporter(new Settings, new StoreReporter)
    // test obsolete API, make sure it doesn't throw
    reporter.info(NoPosition, "goodbye, cruel world", force = false)
  }
}
