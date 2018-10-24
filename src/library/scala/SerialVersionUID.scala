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

package scala

/**
 * Annotation for specifying the `static SerialVersionUID` field
 * of a serializable class.
 */
class SerialVersionUID(value: Long) extends scala.annotation.ClassfileAnnotation
