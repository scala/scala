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

/**
 * To customize the error message that's emitted when an implicit of type
 * C[T1,..., TN] cannot be found, annotate the class C with @implicitNotFound.
 * Assuming C has type parameters X1,..., XN, the error message will be the
 * result of replacing all occurrences of ${Xi} in the string msg with the
 * string representation of the corresponding type argument Ti. *
 *
 * @author Adriaan Moors
 * @since 2.8.1
 */
final class implicitNotFound(msg: String) extends scala.annotation.StaticAnnotation {}
