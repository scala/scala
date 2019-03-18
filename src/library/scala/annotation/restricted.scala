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

object RestrictedSeverity {
  final val info    = "info"
  final val warning = "warning"
  final val error   = "error"
  final val none    = "none"
}

object RestrictedLabel {
  final val deprecated   = "deprecated"
  final val internalOnly = "internalOnly"
  final val apiMayChange = "apiMayChange"
}

/**
 * An annotation to denote the API status.
 *
 * @param  message the advisory to print during compilation
 * @param  since   a string identifying the first version in which the restriction is applied
 * @param  label   a string identifying the categorization of the restriction
 * @param  defaultSeverity the default severity of the restriction when the annotee is referenced
 * @since  2.13.0
 * @see    [[scala.annotation.deprecatedError]]
 * @see    [[scala.annotation.RestrictedSeverity]]
 * @see    [[scala.annotation.RestrictedLabel]]
 */
@getter @setter @beanGetter @beanSetter @companionClass @companionMethod
sealed class restricted(
  message: String,
  since: String,
  label: String,
  defaultSeverity: String = RestrictedSeverity.warning) extends scala.annotation.StaticAnnotation

/** An annotation that designates that an annottee should no longer be used.
 *
 *  This can be used as a next step to deprecation. Instead of removing
 *  the methods, this can be used to display a more helpful migration message.
 *
 *  @param  message the error message to print during compilation if a reference remains in code
 *  @param  since   a string identifying the first version in which the definition was deprecated
 *  @since  2.13.0
 *  @see    [[scala.deprecated]]
 */
@getter @setter @beanGetter @beanSetter
final class deprecatedError(message: String, since: String) extends
  restricted(message, since, RestrictedLabel.deprecated, RestrictedSeverity.error)
