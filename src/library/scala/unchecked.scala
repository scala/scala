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

package scala

/** An annotation to designate that the annotated entity
 *  should not be considered for additional compiler checks.
 *  Specific applications include annotating the subject of
 *  a match expression to suppress exhaustiveness and reachability warnings, and
 *  annotating a type argument in a match case to suppress
 *  unchecked warnings.
 *
 *  Such suppression should be used with caution, without which
 *  one may encounter [[scala.MatchError]] or [[java.lang.ClassCastException]]
 *  at runtime.  In most cases one can and should address the
 *  warning instead of suppressing it.
 *
 *  {{{
 *    object Test extends App {
 *      // This would normally warn "match is not exhaustive"
 *      // because `None` is not covered.
 *      def f(x: Option[String]) = (x: @unchecked) match { case Some(y) => y }
 *      // This would normally warn "type pattern is unchecked"
 *      // but here will blindly cast the head element to String.
 *      def g(xs: Any) = xs match { case x: List[String @unchecked] => x.head }
 *    }
 * }}}
 */
final class unchecked extends scala.annotation.Annotation {}
