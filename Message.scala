/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package xsbt

object Message
{
	def apply(s: => String) = new xsbti.F0[String] { def apply() = s }
}