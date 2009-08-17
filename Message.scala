/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package xsbt

import xsbti.F0

object Message
{
	def apply(s: => String) = new F0[String] { def apply() = s }
}