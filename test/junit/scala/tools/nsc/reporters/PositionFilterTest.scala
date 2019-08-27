package scala
package tools.nsc
package reporters

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.reflect.internal.util._

@RunWith(classOf[JUnit4])
class PositionFilterTest {
  val source = "Test_PositionFilter"
  val batchFile = new BatchSourceFile(source, "For testing".toList)
  val pos = new OffsetPosition(batchFile, 4)

  val store = new StoreReporter(new Settings)

  def createFilter: FilteringReporter = new FilteringReporter {
    def settings: Settings = store.settings
    def doReport(pos: Position, msg: String, severity: Severity): Unit = store.doReport(pos, msg, severity)
  }

  @Test
  def `filters split messages`(): Unit = {
    val filter = createFilter
    val msg = "This is an important warning."
    val longMessage = s"""$msg
          |----
          |Here is some fine print.
          |Be sure to read it carefully.""".stripMargin
    filter.warning(pos, longMessage)
    filter.warning(pos, msg)
    assertEquals(1, store.infos.size)
    assertEquals(1, filter.warningCount)
    assertEquals(longMessage, store.infos.head.msg)
    filter.warning(pos, "hello, world")
    assertEquals(2, store.infos.size)
    assertEquals(2, filter.warningCount)
  }
}
