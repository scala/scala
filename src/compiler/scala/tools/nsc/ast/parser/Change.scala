/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */
package scala.tools.nsc.ast.parser

abstract class Change
case class Insertion(text: String) extends Change
case class Deletion(nchars: Int) extends Change

