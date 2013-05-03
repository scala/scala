/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala
package tools
package cmd

/** Interpolation logic for generated files.  The idea is to be
 *  able to write in terms of @@THIS@@ and @@THAT@@ and the reference
 *  specification knows enough to perform the substitutions.  Warrants
 *  expansion.
 */
trait Interpolation {
  self: Spec =>

  private lazy val reference = referenceSpec
  import reference._

  object interpolate {
    def mapper: Map[String, () => String] = Map(
      "PROGRAM"       -> (() => programInfo.runner),
      "ALLOPTIONS"    -> (() => options.all mkString " "),
      "MAINCLASS"     -> (() => programInfo.mainClass)
    )

    private def mark(key: String) = "@@" + key + "@@"
    def apply(template: String) = mapper.foldLeft(template) { case (s, (key, f)) => s.replaceAll(mark(key), f()) }
  }
}

object Interpolation {
  /** A simple template for generating bash completion functions.
   */
  lazy val bashTemplate = """
    |_@@PROGRAM@@()
    |{
    |  local cur opts base
    |  COMPREPLY=()
    |  cur="${COMP_WORDS[COMP_CWORD]}"
    |  opts="@@ALLOPTIONS@@"
    |
    |  COMPREPLY=($(compgen -W "${opts}" -- ${cur}))
    |  _filedir
    |  return 0
    |} && complete -F _@@PROGRAM@@ @@PROGRAM@@
  """.stripMargin

  /** A simple template for generating a runner script.
   */
  val runnerTemplate = """
    |#!/bin/sh
    |#
    |
    |scala @@MAINCLASS@@ "$@"
    |""".stripMargin.trim + "\n"
}
