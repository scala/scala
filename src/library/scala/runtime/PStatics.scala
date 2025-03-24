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

package scala.runtime

// things that should be in `Statics`, but can't be yet for bincompat reasons
// TODO 3.T: move to `Statics`
private[scala] object PStatics {
  // `Int.MaxValue - 8` traditional soft limit to maximize compatibility with diverse JVMs
  // See https://stackoverflow.com/a/8381338 for example
  final val VM_MaxArraySize = 2147483639
}
