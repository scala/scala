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

package scala.tools.nsc.transform.async

private[async] final class StateAssigner {
  private var current = StateAssigner.Initial

  def nextState(): Int = try current finally current += 1
}

object StateAssigner {
  final val Initial = 0
}
