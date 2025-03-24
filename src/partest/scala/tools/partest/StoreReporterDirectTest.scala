/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.partest

import scala.tools.nsc.Settings
import scala.tools.nsc.reporters.StoreReporter

trait StoreReporterDirectTest extends DirectTest {
  private[this] var _storeReporter: StoreReporter = _
  def storeReporter: StoreReporter = _storeReporter

  /** Discards all but the first message issued at a given position. */
  def filteredInfos: Seq[StoreReporter.Info] = storeReporter.infos.groupBy(_.pos).map(_._2.head).toList

  /** Hook into [[scala.tools.partest.DirectTest]] to install the custom reporter */
  override def reporter(settings: Settings) = {
    _storeReporter = new StoreReporter(settings)
    _storeReporter
  }
}
