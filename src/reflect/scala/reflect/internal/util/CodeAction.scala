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

package scala
package reflect
package internal
package util

/**
 *  <span class="badge badge-red" style="float: right;">EXPERIMENTAL</span>
 *
 *  CodeAction is used to communicate code edit suggestion to tooling in
 *  a structured manner.
 *
 *  @see <a href=
 *     "https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#codeAction">`CodeAction`</a>
 *
 *  @groupname Common   Commonly used methods
 *  @group ReflectionAPI
 */
case class CodeAction(title: String, description: Option[String], edits: List[TextEdit])

object CodeAction {
  def apply(title: String, pos: Position, newText: String, desc: String, check: => Boolean = true): List[CodeAction] =
    if (check) List(CodeAction(title, Some(desc), List(TextEdit(pos, newText))))
    else Nil

  private lazy val parens = raw"\(.*\)".r
  def maybeWrapInParens(s: String) = if (s.contains(" ") && !parens.matches(s)) s"($s)" else s
  def wrapInParens(s: String) = if (!parens.matches(s)) s"($s)" else s
}

/**
 *  <span class="badge badge-red" style="float: right;">EXPERIMENTAL</span>
 *
 *
 *  @groupname Common   Commonly used methods
 *  @group ReflectionAPI
 */
case class TextEdit(position: Position, newText: String) {
  def delta: Int = newText.length - (position.end - position.start)
}
