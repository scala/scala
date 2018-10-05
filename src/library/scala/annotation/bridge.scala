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

package scala.annotation

/** If this annotation is present on a method, it will be treated as a bridge method.
 */
@deprecated("reconsider whether using this annotation will accomplish anything", "2.10.0")
private[scala] class bridge extends scala.annotation.StaticAnnotation
