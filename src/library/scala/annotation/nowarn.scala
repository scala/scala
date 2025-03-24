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

/** An annotation for local warning suppression.
  *
  * The optional `value` parameter allows selectively silencing messages. See `-Wconf:help` for help
  * writing a message filter expression, or use `@nowarn("verbose")` / `@nowarn("v")` to display message
  * filters applicable to a specific warning.
  *
  * Examples:
  *
  * {{{
  *   def f = {
  *     1: @nowarn // don't warn "a pure expression does nothing in statement position"
  *     2
  *   }
  *
  *   // show the warning, plus the applicable @nowarn / Wconf filters ("cat=other-pure-statement", ...)
  *   @nowarn("v") def f = { 1; 2 }
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
