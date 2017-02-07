/*
 * Zinc - The incremental compiler for Scala.
 * Copyright 2011 - 2017, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * This software is released under the terms written in LICENSE.
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
      case e: NoSuchMethodException =>
        constr(classOf[List[_]], classOf[Settings], classOf[Function1[_, _]], classOf[Boolean]).newInstance(arguments, settings, (s: String) => throw new RuntimeException(s), false.asInstanceOf[AnyRef])
    }
  }

  def getWarnFatal(settings: Settings): Boolean =
    settings.fatalWarnings.value

  def getNoWarn(settings: Settings): Boolean =
    settings.nowarn.value
}
