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

package scala.tools.nsc
package plugins

/** A component that is part of a Plugin.
 *
 * @author Lex Spoon
 * Updated 2009/1/2 by Anders Bach Nielsen: Added features to implement SIP 00002
 */
abstract class PluginComponent extends SubComponent {

  /** By definition, plugin phases are externally provided. */
  final override val internal = false

  /** Only plugins are granted a reprieve from specifying whether they follow. */
  val runsRightAfter: Option[String] = None

  /** Useful for -Vphases. */
  def description: String = ""

}
