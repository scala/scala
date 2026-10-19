/*
 * Zinc - The incremental compiler for Scala.
 * Copyright Scala Center, Lightbend dba Akka, and Mark Harrah
 *
 * Scala (https://www.scala-lang.org)
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools
package xsbt
package visitor

import xsbti.VirtualFile

/**
 * Implemented by an `AnalysisCallback` that wants the bridge to hash class APIs itself, standing
 * in for a callback method that the published compiler interface doesn't have yet.
 */
trait ApiHashSink {
  /** Whether to also report the API trees through `AnalysisCallback.api`. */
  def wantsTrees: Boolean
  def optimizedSealed: Boolean
  def apiHashes(source: VirtualFile, hashes: ApiHasher.ClassHashes): Unit
}
