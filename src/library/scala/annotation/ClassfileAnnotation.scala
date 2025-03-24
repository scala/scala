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

package scala.annotation

/** A base class for classfile annotations. These are stored as
 *  [[https://docs.oracle.com/javase/8/docs/technotes/guides/language/annotations.html Java annotations]]
 *  in classfiles.
 */
@deprecated("Annotation classes need to be written in Java in order to be stored in classfiles in a Java-compatible manner", "2.13.0")
trait ClassfileAnnotation extends ConstantAnnotation
