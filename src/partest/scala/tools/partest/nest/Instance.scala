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

package scala.tools.partest.nest

import scala.language.implicitConversions

/** The trait mixed into each instance of a specification.
 *
 *  @see    Reference
 */
trait Instance extends Spec {
  def parsed: CommandLine

  protected def help(str: => String): Unit = ()

  def isSet(s: String)    = parsed isSet toOpt(s)
  def originalArgs        = parsed.originalArgs     // the full original list
  def residualArgs        = parsed.residualArgs     // only args which were not options or args to options

  type OptionMagic = Opt.Instance
  protected implicit def optionMagicAdditions(name: String): Opt.Instance = new Opt.Instance(programInfo, parsed, name)
}
