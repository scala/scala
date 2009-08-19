package xsbt

import java.io.File
import java.net.URLClassLoader
import xsbti.{Logger, TestCallback, TestLogger}
import FileUtilities.{classLocationFile, withTemporaryDirectory, write}

object TestCompile
{
	/** Tests running the compiler interface with the analyzer plugin with a test callback.  The test callback saves all information
	* that the plugin sends it for post-compile analysis by the provided function.*/
	def apply[T](arguments: Seq[String], superclassNames: Seq[String])(f: (TestCallback, Logger) => T): T =
	{
		val pluginLocation = classLocationFile[Analyzer]
		assert(pluginLocation.exists)
		val path = pluginLocation.getAbsolutePath
		val pluginArg = if(pluginLocation.getName.endsWith(".jar")) List("-Xplugin:" + path) else List("-Xpluginsdir", path)
		val testCallback = new TestCallback(superclassNames.toArray)
		val i = new CompilerInterface
		val newArgs = "-Xplugin-require:xsbt-analyze" :: pluginArg ::: arguments.toList
		TestLogger { log =>
			i.run(newArgs.toArray, testCallback, 5, log)
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
object WithFiles
{
	/** Takes the relative path -> content pairs and writes the content to a file in a temporary directory.  The written file
	* path is the relative path resolved against the temporary directory path.  The provided function is called with the resolved file paths
	* in the same order as the inputs. */
	def apply[T](sources: (File, String)*)(f: Seq[File] => T): T =
	{
		withTemporaryDirectory { dir =>
			val sourceFiles =
				for((file, content) <- sources) yield
				{
					assert(!file.isAbsolute)
					val to = new File(dir, file.getPath)
					write(to, content)
					to
				}
			f(sourceFiles)
		}
	}
}