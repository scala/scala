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
package ast.parser

/** A patch that postulates that a brace needs to be inserted or deleted at a given position.
 *  @param off  The offset where the brace needs to be inserted or deleted
 *  @param inserted  If true, brace needs to be inserted, otherwise brace needs to be deleted.
 */
case class BracePatch(off: Int, inserted: Boolean)
extends Patch(off, if (inserted) Insertion("{") else Deletion(1))
