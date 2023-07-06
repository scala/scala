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

/**
 *  <span class="badge badge-red" style="float: right;">EXPERIMENTAL</span>
 *
 *
 *  @groupname Common   Commonly used methods
 *  @group ReflectionAPI
 */
case class TextEdit(position: Position, newText: String)
