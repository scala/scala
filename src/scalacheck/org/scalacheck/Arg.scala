/*-------------------------------------------------------------------------*\
**  ScalaCheck                                                             **
**  Copyright (c) 2007-2011 Rickard Nilsson. All rights reserved.          **
**  http://www.scalacheck.org                                              **
**                                                                         **
**  This software is released under the terms of the Revised BSD License.  **
**  There is NO WARRANTY. See the file LICENSE for the full text.          **
\*-------------------------------------------------------------------------*/

package org.scalacheck

case class Arg[+T](
  label: String,
  arg: T,
  shrinks: Int,
  origArg: T
)(implicit prettyPrinter: T => Pretty) {
  lazy val prettyArg: Pretty = prettyPrinter(arg)
  lazy val prettyOrigArg: Pretty = prettyPrinter(origArg)
}
