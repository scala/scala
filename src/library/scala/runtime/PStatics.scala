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

package scala.runtime

// things that should be in `Statics`, but can't be yet for bincompat reasons
// TODO 3.T: move to `Statics`
private[scala] object PStatics {
  final val VM_MaxArraySize = 2147483645 // == `Int.MaxValue - 2`, hotspot limit
}
