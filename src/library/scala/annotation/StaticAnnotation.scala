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

/** A base class for static annotations. These are available
 *  to the Scala type checker, even across different compilation units.
 *
 *  @author  Martin Odersky
 *  @since   2.4
 */
trait StaticAnnotation extends Annotation
