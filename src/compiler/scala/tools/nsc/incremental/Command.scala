/*
 * Zinc - The incremental compiler for Scala.
 * Copyright Lightbend, Inc. and Mark Harrah
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package xsbt

import scala.tools.nsc.{ CompilerCommand, Settings }

object Command {

  /**
   * Construct a CompilerCommand using reflection, to be compatible with Scalac before and after
   * <a href="https://lampsvn.epfl.ch/trac/scala/changeset/21274">r21274</a>
   */
  def apply(arguments: List[String], settings: Settings): CompilerCommand = {
    def constr(params: Class[_]*) = classOf[CompilerCommand].getConstructor(params: _*)
    try {
      constr(classOf[List[_]], classOf[Settings]).newInstance(arguments, settings)
    } catch {
      case _: NoSuchMethodException =>
        constr(classOf[List[_]], classOf[Settings], classOf[(_) => _], classOf[Boolean])
          .newInstance(
            arguments,
            settings,
            (s: String) => throw new RuntimeException(s),
            false.asInstanceOf[AnyRef]
          )
    }
  }

  def getWarnFatal(settings: Settings): Boolean =
    settings.fatalWarnings.value

  def getNoWarn(settings: Settings): Boolean =
    settings.nowarn.value
}
