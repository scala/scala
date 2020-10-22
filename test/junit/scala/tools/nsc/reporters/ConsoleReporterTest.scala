package scala
package tools.nsc
package reporters

import java.io.{ByteArrayOutputStream, StringReader, BufferedReader, PrintWriter}
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

  def incErrorCount(r: Reporter): Unit = {
    r.error(NoPosition, "")
    writerOut.reset()
  }

  def incWarningCount(r: Reporter): Unit = {
    r.warning(NoPosition, "")
    writerOut.reset()
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
    } finally writerOut.reset()

  @Test
  def printMessageTest(): Unit = {
    val reporter = createConsoleReporter("r", writerOut)
    testHelper(msg = "Hello World!", severity = "error: ")(_ => reporter.error(NoPosition, "Hello World!"))
    testHelper(posWithSource, "Testing with Defined Position", severity = "error: ")(reporter.error(_, "Testing with Defined Position"))
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
    testHelper(msg = "test")(reporter.echo(_, "test"))
    testHelper(msg = "test", severity = "warning: ")(reporter.warning(_, "test"))
    testHelper(msg = "test", severity = "error: ")(reporter.error(_, "test"))
    testHelper(posWithSource, msg = "test")(reporter.echo(_, "test"))
    testHelper(posWithSource, msg = "test", severity = "warning: ")(reporter.warning(_, "test"))
    testHelper(posWithSource, msg = "test", severity = "error: ")(reporter.error(_, "test"))
  }

  @Test
  def displayTest(): Unit = {
    val reporter = createConsoleReporter("r", writerOut)

    // Change maxerrs and maxwarns from default
    reporter.settings.maxerrs.value = 1
    reporter.settings.maxwarns.value = 1

    // counting happens in .error/.warning, doReport doesn't count
    testHelper(msg = "Testing display", severity = "warning: ")(reporter.doReport(_, "Testing display", reporter.WARNING))
    testHelper(msg = "Testing display", severity = "error: ")(reporter.doReport(_, "Testing display", reporter.ERROR))

    testHelper(msg = "Testing display")(reporter.echo(_, "Testing display"))
    testHelper(msg = "Testing display", severity = "warning: ")(reporter.warning(_, "Testing display"))
    testHelper(msg = "Testing display", severity = "error: ")(reporter.error(_, "Testing display"))

    testHelper(posWithSource, msg = "Testing display")(reporter.echo(_, "Testing display"))
    testHelper(msg = "")(reporter.warning(_, "Test maxerrs"))
    testHelper(msg = "")(reporter.error(_, "Test maxwarns"))

    // the filter happens in .error/.warning, doReport always reports
    testHelper(posWithSource, msg = "Testing display", severity = "warning: ")(reporter.doReport(_, "Testing display", reporter.WARNING))
    testHelper(posWithSource, msg = "Testing display", severity = "error: ")(reporter.doReport(_, "Testing display", reporter.ERROR))

    reporter.reset()

    testHelper(posWithSource, msg = "Testing display for maxerrs to pass", severity = "error: ")(reporter.error(_, "Testing display for maxerrs to pass"))
    incErrorCount(reporter)
    testHelper(msg = "")(reporter.error(_, "Testing display for maxerrs to fail"))

    testHelper(posWithSource, msg = "")(reporter.warning(_, "Position filter drops warning if there was an error at the same position"))
    testHelper(msg = "Warning passes at different location", severity = "warning: ")(reporter.warning(_, "Warning passes at different location"))

    reporter.reset()

    testHelper(posWithSource, msg = "Testing display for maxwarns to pass", severity = "warning: ")(reporter.warning(_, "Testing display for maxwarns to pass"))
    incWarningCount(reporter)
    testHelper(msg = "")(reporter.warning(_, "Testing display for maxwarns to fail"))
  }

  @Test
  def finishTest(): Unit = {
    val reporter = createConsoleReporter("", writerOut)

    reporter.reset()
    testHelper(msg = "")(_ => reporter.finish())

    for (i <- 1 to 10) incErrorCount(reporter)
    for (i <- 1 to 3) incWarningCount(reporter)
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
      val conf = new Settings
      conf.maxerrs.value  = 1
      conf.maxwarns.value = 1

      new FilteringReporter {
        def settings: Settings = conf
        def doReport(pos: Position, msg: String, severity: Severity): Unit = reporter.doReport(pos, msg, severity)
      }
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
    val reporter = new FilteringReporter {
      val settings: Settings = new Settings
      def doReport(pos: Position, msg: String, severity: Severity): Unit = ()
    }
    // test obsolete API, make sure it doesn't throw
    reporter.echo(NoPosition, "goodbye, cruel world")
  }

  @Test
  def adaptedReporterTest(): Unit = {
    val reporter = createConsoleReporter("r", writerOut)
    val adapted  = new FilteringReporter {
      def settings: Settings = reporter.settings
      def doReport(pos: Position, msg: String, severity: Severity): Unit = reporter.doReport(pos, msg, severity)
    }

    // pass one message
    testHelper(msg = "Testing display")(adapted.echo(_, "Testing display"))
    testHelper(msg = "Testing display", severity = "warning: ")(adapted.warning(_, "Testing display"))
    testHelper(msg = "Testing display", severity = "error: ")(adapted.error(_, "Testing display"))

    assertTrue(adapted.hasErrors)
    assertEquals(1, adapted.errorCount)
    assertTrue(adapted.hasWarnings)
    assertEquals(1, adapted.warningCount)
    adapted.reset()
    assertFalse(adapted.hasErrors)
    assertEquals(0, adapted.errorCount)
    assertFalse(adapted.hasWarnings)
    assertEquals(0, adapted.warningCount)
  }
}
