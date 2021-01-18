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

/** An annotation for local warning suppression.
  *
  * The optional `value` parameter allows selectively silencing messages, see `scalac -Wconf:help`
  * for help. Examples:
  *
  * {{{
  *   def f = {
  *     1: @nowarn // don't warn "a pure expression does nothing in statement position"
  *     2
  *   }
  *
  *   @nowarn def f = { 1; deprecated() } // don't warn
  *
  *   @nowarn("msg=pure expression does nothing")
  *   def f = { 1; deprecated() } // show deprecation warning
  * }}}
  *
  * To ensure that a `@nowarn` annotation actually suppresses a warning, enable `-Xlint:unused` or `-Wunused:nowarn`.
  * The unused annotation warning is emitted in category `unused-nowarn` and can be selectively managed
  * using `-Wconf:cat=unused-nowarn:s`.
  */
class nowarn(value: String = "") extends ConstantAnnotation
