/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package xsbt

import xsbti.Logger
import scala.tools.nsc.ObjectRunner

import java.net.URL

class RunInterface
{
	def run(classpathURLs: Array[URL], mainClass: String, options: Array[String], log: Logger)
	{
		log.info(Message("Running " + mainClass + " " + options.mkString(" ")))
		log.debug(Message("  Classpath:\n\t" + classpathURLs.mkString("\n\t")))
		try { ObjectRunner.run(classpathURLs.toList, mainClass, options.toList) }
		catch { case e: java.lang.reflect.InvocationTargetException => throw e.getCause }
	}
}