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

import scala.annotation.meta._

/** An annotation to denote the API status, like [[scala.deprecated]] but more powerful.
 * While `@deprecated` is only able to discourage someone from using the some API,
 * `@apiStatus` can be more nuanced about the state (for instance `Category.ApiMayChange`),
 * and choose the default compile-time actions (`Action.Error`, `Action.Warning`, etc).
 *
 * In other words, this gives library authors the lever to trigger compilation warning or
 * compilation errors! Here's an example of displaying a migration message:
 *
 * {{{
 * import scala.annotation.apiStatus, apiStatus._
 * @apiStatus(
 *   "method <<= is removed; use := syntax instead",
 *   category = Category.ForRemoval,
 *   since = "foo-lib 1.0",
 *   defaultAction = Action.Error,
 * )
 * def <<=() = ???
 * }}}
 *
 * The compilation will fail and display the migration message if the method is called:
 *
 * {{{
 * example.scala:26: error: method <<= is removed; use := syntax instead (foo-lib 1.0)
 *   <<=()
 *   ^
 * }}}
 *
 * Here's another example of displaying a warning:
 *
 * {{{
 * import scala.annotation.apiStatus, apiStatus._
 * @apiStatus(
 *   "should DSL is incubating, and future compatibility is not guaranteed",
 *   category = Category.ApiMayChange,
 *   since = "foo-lib 1.0",
 * )
 * implicit class ShouldDSL(s: String) {
 *   def should(o: String): Unit = ()
 * }
 * }}}
 *
 * The compiler will emit warnings:
 *
 * {{{
 * example.scala:28: warning: should DSL is incubating, and future compatibility is not guaranteed (foo-lib 1.0)
 *   "bar" should "something"
 *   ^
 * }}}
 *
 * Using `-Wconf:cat=api-may-change&origin=foo\..*:silent` option, the user of the library can opt out of
 * the `api-may-change` warnings afterwards.
 *
 * @param  message the advisory to print during compilation
 * @param  category a string identifying the categorization of the restriction
 * @param  since a string identifying the first version in which the status is applied
 * @param  defaultAction the default severity of the restriction when the annotee is referenced
 * @since  2.13.2
 * @see    [[scala.annotation.apiStatus.Action]]
 * @see    [[scala.annotation.apiStatus.Category]]
 */
@getter @setter @beanGetter @beanSetter @companionClass @companionMethod
class apiStatus(
  message: String,
  category: String,
  since: String = "",
  defaultAction: String = apiStatus.Action.Warning) extends scala.annotation.StaticAnnotation

object apiStatus {
  object Action {
    final val Error          = "error"
    final val Warning        = "warning"
    final val WarningSummary = "warning-summary"
    final val Info           = "info"
    final val InfoSummary    = "info-summary"
    final val Silent         = "silent"
  }

  object Category {
    final val ForRemoval   = "for-removal"
    final val InternalOnly = "internal-only"
    final val ApiMayChange = "api-may-change"
    final val Mistake      = "mistake"
  }
}
