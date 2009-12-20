/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package xsbt

object Log
{
	def debug(log: xsbti.Logger, msg: => String) = log.debug(Message(msg))
}