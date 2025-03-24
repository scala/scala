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
package doc
package base

import model._

sealed trait LinkTo
final case class LinkToMember[Mbr, Tpl](mbr: Mbr, tpl: Tpl) extends LinkTo
final case class LinkToTpl[Tpl](tpl: Tpl) extends LinkTo
final case class LinkToExternalTpl(name: String, baseUrl: String, tpl: TemplateEntity) extends LinkTo
final case class Tooltip(name: String) extends LinkTo
