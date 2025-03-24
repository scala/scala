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

/** A marker for annotations that, when applied to a type, should be treated
 *  as a constraint on the annotated type.
 *
 *  A proper constraint should restrict the type based only on information
 *  mentioned within the type.  A Scala compiler can use this assumption to
 *  rewrite the contents of the constraint as necessary.  To contrast, a type
 *  annotation whose meaning depends on the context where it is written
 *  down is not a proper constrained type, and this marker should not be
 *  applied.  A Scala compiler will drop such annotations in cases where it
 *  would rewrite a type constraint.
 */
trait TypeConstraint extends Annotation
