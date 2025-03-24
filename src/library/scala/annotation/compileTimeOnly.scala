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

import scala.annotation.meta._

/**
 * An annotation that designates that an annottee should not be referred to after
 * type checking (which includes macro expansion).
 *
 * Examples of potential use:
 *   1) The annottee can only appear in the arguments of some other macro
 *      that will eliminate it from the AST during expansion.
 *   2) The annottee is a macro and should have been expanded away,
 *      so if hasn't, something wrong has happened.
 *      (Comes in handy to provide better support for new macro flavors,
 *      e.g. macro annotations, that can't be expanded by the vanilla compiler).
 *
 * @param  message the error message to print during compilation if a reference remains
 *                 after type checking
 */
@getter @setter @beanGetter @beanSetter @companionClass @companionMethod
final class compileTimeOnly(message: String) extends scala.annotation.StaticAnnotation
