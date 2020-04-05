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
 * ==Defining a custom status annotation==
 * Instead of directly using `@apiStatus` we can create a specific status annotation by extending `apiStatus`.
 * However, due to the information available to the compiler, the default values for `message`, `category`, or
 * `defaultAction` can be specified through the corresponding meta-annotations.
 *
 * {{{
 * import scala.annotation.{ apiStatus, apiStatusCategory, apiStatusDefaultAction }
 * import scala.annotation.meta._
 *
 * @apiStatusCategory(apiStatus.Category.ApiMayChange)
 * @apiStatusDefaultAction(apiStatus.Action.Warning)
 * @companionClass @companionMethod
 * final class apiMayChange(
 *   message: String,
 *   since: String = "",
 * ) extends apiStatus(message, since = since)
 * }}}
 *
 * This can be used as follows:
 *
 * {{{
 * @apiMayChange("should DSL is incubating, and future compatibility is not guaranteed")
 * implicit class ShouldDSL(s: String) {
 *   def should(o: String): Unit = ()
 * }
 * }}}
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
  category: String = apiStatus.Category.Unspecified,
  since: String = "",
  defaultAction: String = apiStatus.Action.Warning,
) extends scala.annotation.StaticAnnotation

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
    final val Unspecified  = "unspecified"
  }
}

/**
 * Consult the documentation in [[scala.annotation.apiStatus]].
 */
final class apiStatusMessage(message: String) extends scala.annotation.StaticAnnotation

/**
 * Consult the documentation in [[scala.annotation.apiStatus]].
 */
final class apiStatusDefaultAction(defaultAction: String) extends scala.annotation.StaticAnnotation

/**
 * Consult the documentation in [[scala.annotation.apiStatus]].
 */
final class apiStatusCategory(category: String) extends scala.annotation.StaticAnnotation
