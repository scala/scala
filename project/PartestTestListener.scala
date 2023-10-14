package scala.build

import java.io.{File, PrintWriter, StringWriter}

import sbt.testing.{SuiteSelector, TestSelector}
import sbt.{JUnitXmlTestsListener, TestEvent, TestResult, TestsListener, _}
import sbt.internal.util.EscHelpers.removeEscapeSequences

// The default JUnitXMLListener doesn't play well with partest: we end up clobbering the one-and-only partest.xml
// file on group of tests run by `testAll`, and the test names in the XML file don't seem to show the path to the
// test for tests defined in a single file.
//
// Let's roll our own to try to enable the Jenkins JUnit test reports.
class PartestTestListener(target: File) extends TestsListener {
  val delegate = new JUnitXmlTestsListener(target.getAbsolutePath)
  import java.util.EnumSet

  import sbt.testing.{Status => TStatus}
  val errorStatus = EnumSet.of(TStatus.Error)
  val failStatus = EnumSet.of(TStatus.Failure)
  val skipStatus = EnumSet.of(TStatus.Skipped, TStatus.Ignored)

  override def doInit(): Unit = ()
  override def doComplete(finalResult: TestResult): Unit = ()
  override def endGroup(name: String, t: Throwable): Unit = ()
  override def endGroup(name: String, result: TestResult): Unit = ()
  override def testEvent(event: TestEvent): Unit = {
    // E.g "test.files.pos" or "test.scaladoc.run"
    def groupOf(e: sbt.testing.Event) = {
      val group = e.selector match {
        case sel: TestSelector =>
          e.fullyQualifiedName().replace('/', '.') + "." + sel.testName().takeWhile(_ != '/')
        case _: SuiteSelector =>
          // SBT emits this in the test event when a forked test failed unexpectedly: https://github.com/sbt/sbt/blob/684e2c369269e2aded5861c06aaad6f0b6b70a30/testing/agent/src/main/java/sbt/ForkMain.java#L337-L339
          "<unknown>"
      }
      // Don't even ask.
      // You really want to know? Okay.. https://issues.jenkins-ci.org/browse/JENKINS-49832
      group.replaceAll("""\brun\b""", "run_")
    }

    // "t1234.scala" or "t1235"
    def testOf(e: sbt.testing.Event) = e.selector match {
      case sel: TestSelector => sel.testName().dropWhile(_ != '/').drop(1)
      case _ =>
        e.fullyQualifiedName()
    }

    for ((group, events) <- event.detail.groupBy(groupOf)) {
      val statii = events.map(_.status())
      val errorCount = statii.count(errorStatus.contains)
      val failCount = statii.count(failStatus.contains)
      val skipCount = statii.count(skipStatus.contains)
      val testCount = statii.size
      val totalDurationMs = events.iterator.map(_.duration()).sum
      val xml = <testsuite hostname={delegate.hostname} name={group}
                           tests={"" + testCount} errors={"" + errorCount} failures={"" + failCount}
                           skipped={"" + skipCount} time={(1.0 * totalDurationMs / 1000).toString}>
        {delegate.properties}{for (e <- events) yield {
          val trace: String = if (e.throwable.isDefined) {
            val stringWriter = new StringWriter()
            val writer = new PrintWriter(stringWriter)
            e.throwable.get.printStackTrace(writer)
            writer.flush()
            removeEscapeSequences(stringWriter.toString)
          } else {
            ""
          }

          <testcase classname={group} name={testOf(e)} time={(1.0 * e.duration() / 1000).toString}>
            {e.status match {
            case TStatus.Error if e.throwable.isDefined =>
              <error message={removeEscapeSequences(e.throwable.get.getMessage)} type={e.throwable.get.getClass.getName}>
                {trace}
              </error>
            case TStatus.Error =>
                <error message={"No Exception or message provided"}/>
            case TStatus.Failure if e.throwable.isDefined =>
              <failure message={removeEscapeSequences(e.throwable.get.getMessage)} type={e.throwable.get.getClass.getName}>
                {trace}
              </failure>
            case TStatus.Failure =>
                <failure message={"No Exception or message provided"}/>
            case TStatus.Ignored | TStatus.Skipped | sbt.testing.Status.Pending =>
                <skipped/>
            case _ =>
          }}<system-out>
            <![CDATA[]]>
          </system-out>
            <system-err>
              <![CDATA[]]>
            </system-err>
          </testcase>
        }}
      </testsuite>
      val partestTestReports = target / "test-reports" / "partest"
      val xmlFile = partestTestReports / (group + ".xml")
      xmlFile.getParentFile.mkdirs()
      scala.xml.XML.save(xmlFile.getAbsolutePath, xml, "UTF-8", true, null)
    }
  }
  override def startGroup(name: String): Unit = ()
}
