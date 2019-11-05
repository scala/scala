/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools
package nsc
package incremental

import xsbti.{ AnalysisCallback, Logger, Problem, Reporter }
import xsbti.compile._
import scala.tools.nsc.Settings
import scala.collection.mutable
import Log.debug
import java.io.File
