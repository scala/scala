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

package scala.annotation.unchecked

import scala.annotation.meta.{field, getter}

/** An annotation for values that are assumed to be stable even though their
 *  types are volatile.
 */
@getter @field
final class uncheckedStable extends scala.annotation.StaticAnnotation {}
