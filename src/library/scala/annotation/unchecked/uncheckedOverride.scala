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

import scala.annotation.StaticAnnotation

/**
 * Marking a definition `@uncheckedOverride` is equivalent to the `override` keyword, except that overriding is not
 * enforced. A definition marked `@uncheckedOverride` is allowed to override nothing.
 */
final class uncheckedOverride extends StaticAnnotation
