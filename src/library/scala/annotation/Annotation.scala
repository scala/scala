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

/** A base class for annotations. Annotations extending this class directly
 *  are not preserved for the Scala type checker and are also not stored as
 *  Java annotations in classfiles. To enable either or both of these, one
 *  needs to inherit from [[scala.annotation.StaticAnnotation]] or/and
 *  [[scala.annotation.ClassfileAnnotation]].
 *
 *  @author  Martin Odersky
 *  @since 2.4
 */
abstract class Annotation {}
