/* NSC -- new Scala compiler
 * Copyright 2007-2013 LAMP/EPFL
 */

package scala.tools.nsc
package doc
package base

sealed trait LinkTo
final case class LinkToMember[Mbr, Tpl](mbr: Mbr, tpl: Tpl) extends LinkTo
final case class LinkToTpl[Tpl](tpl: Tpl) extends LinkTo
final case class LinkToExternal(name: String, url: String) extends LinkTo
final case class Tooltip(name: String) extends LinkTo
