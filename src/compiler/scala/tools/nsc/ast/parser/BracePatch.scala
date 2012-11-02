/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */
package scala.tools.nsc
package ast.parser

/** A patch that postulates that a brace needs to be inserted or deleted at a given position.
 *  @param off  The offset where the brace needs to be inserted or deleted
 *  @param inserted  If true, brace needs to be inserted, otherwise brace needs to be deleted.
 */
case class BracePatch(off: Int, inserted: Boolean)
extends Patch(off, if (inserted) Insertion("{") else Deletion(1))