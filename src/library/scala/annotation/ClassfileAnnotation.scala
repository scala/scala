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

/** A base class for classfile annotations. These are stored as
 *  [[http://docs.oracle.com/javase/8/docs/technotes/guides/language/annotations.html Java annotations]]]
 *  in classfiles.
 *
 *  @author  Martin Odersky
 *  @since 2.4
 */
trait ClassfileAnnotation extends StaticAnnotation
