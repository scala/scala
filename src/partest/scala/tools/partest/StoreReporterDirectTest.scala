package scala.tools.partest

import scala.tools.nsc.Settings
import scala.tools.nsc.reporters.StoreReporter
import scala.collection.mutable

trait StoreReporterDirectTest extends DirectTest {
  lazy val storeReporter: StoreReporter = new scala.tools.nsc.reporters.StoreReporter()

  /** Discards all but the first message issued at a given position. */
  def filteredInfos: Seq[storeReporter.Info] = storeReporter.infos.groupBy(_.pos).map(_._2.head).toList

  /** Hook into [[scala.tools.partest.DirectTest]] to install the custom reporter */
  override def reporter(settings: Settings) = storeReporter
}
