package xsbt

import java.io.File
import java.net.URLClassLoader
import xsbti.TestCallback
import FileUtilities.withTemporaryDirectory

object TestCompile
{
	/** Tests running the compiler interface with the analyzer plugin with a test callback.  The test callback saves all information
	* that the plugin sends it for post-compile analysis by the provided function.*/
	def apply[T](arguments: Seq[String], superclassNames: Seq[String])(f: (TestCallback, Logger) => T): T =
	{
		val testCallback = new TestCallback(superclassNames.toArray)
		val i = new CompilerInterface
		val log = new BufferedLogger(new ConsoleLogger)
		log.bufferQuietly {
			i.run(arguments.toArray, testCallback, 5, log)
			f(testCallback, log)
		}
	}
	/** Tests running the compiler interface with the analyzer plugin.  The provided function is given a ClassLoader that can
	* load the compiled classes..*/
	def apply[T](sources: Seq[File])(f: ClassLoader => T): T =
		CallbackTest.apply(sources, Nil){ case (callback, outputDir, log) => f(new URLClassLoader(Array(outputDir.toURI.toURL))) }
}
object CallbackTest
{
	def apply[T](sources: Iterable[File])(f: TestCallback => T): T =
		apply(sources.toSeq, Nil){ case (callback, outputDir, log) => f(callback) }
	def apply[T](sources: Seq[File], superclassNames: Seq[String])(f: (TestCallback, File, Logger) => T): T =
	{
		withTemporaryDirectory { outputDir =>
			val newArgs = "-d" :: outputDir.getAbsolutePath :: sources.map(_.getAbsolutePath).toList
			TestCompile(newArgs, superclassNames) { case (callback, log) => f(callback, outputDir, log) }
		}
	}
}