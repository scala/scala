/* sbt -- Simple Build Tool
 * Copyright 2010  Jason Zaugg
 */
package xsbt

	import scala.tools.nsc.{CompilerCommand, Settings}

object Command
{
	/**
		* Construct a CompilerCommand using reflection, to be compatible with Scalac before and after
		* <a href="https://lampsvn.epfl.ch/trac/scala/changeset/21274">r21274</a>
		*/
	def apply(arguments: List[String], settings: Settings): CompilerCommand = {
		def constr(params: Class[_]*) = classOf[CompilerCommand].getConstructor(params: _*)
		try {
			constr(classOf[List[_]], classOf[Settings]).newInstance(arguments, settings)
		} catch {
		case e: NoSuchMethodException =>
			constr(classOf[List[_]], classOf[Settings], classOf[Function1[_, _]], classOf[Boolean]).newInstance(arguments, settings, error _, false.asInstanceOf[AnyRef])
		}
	}
}