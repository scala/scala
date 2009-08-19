package xsbt

import java.io.File
import java.net.URLClassLoader
import org.specs.Specification

/** Verifies that the analyzer plugin properly detects main methods.  The main method must be
* public with the right signature and be defined on a public, top-level module.*/
object ApplicationsTest extends Specification
{
	val sourceContent =
		"""
		object Main { def main(args: Array[String]) {} }
		""" :: """
		class Main2 { def main(args: Array[String]) {} }
		""" :: """
		object Main3 { private def main(args: Array[String]) {} }
		private object Main3b extends Main2
		object Main3c  { private def main(args: Array[String]) {} }
		protected object Main3d  { def main(args: Array[String]) {} }
		object Main3e {
			protected def main(args: Array[String]) {}
		}
		package a {
			object Main3f { private[a] def main(args: Array[String]) {} }
			object Main3g { protected[a] def main(args: Array[String]) {} }
		}
		""" ::"""
		object Main4 extends Main2
		""" :: """
		trait Main5 { def main(args: Array[String]) {} }; trait Main5b extends Main5; trait Main5c extends Main2; abstract class Main5d { def main(args: Array[String]) {} }
		""" :: """
		object Main6a { var main = () }
		object Main6b { var main = (args: Array[String]) => () }
		""" :: """
		object Main7 { object Main7b extends Main2 }
		""" :: """
		object Main8 extends Main2 { object Main7b extends Main2 }
		""" :: """
		object Main9 {
			def main() {}
			def main(i: Int) {}
			def main(args: Array[String]) {}
		}
		""" :: """
		object MainA {
			def main() {}
			def main(i: Int) {}
			def main(args: Array[String], other: String) {}
			def main(i: Array[Int]) {}
		}
		object MainA2 {
			def main[T](args: Array[T]) {}
		}
		""" :: """
		object MainB extends Main2 {
			def main() {}
			def main(i: Int) {}
		}
		""" :: """
		object MainC1 {
			def main(args: Array[String]) = 3
		}
		object MainC2 {
			def main1(args: Array[String]) {}
		}
		""" :: """
		object MainD1 {
			val main = ()
		}
		object MainD2 {
			val main = (args: Array[String]) => ()
		}
		""" :: """
		object MainE1 {
			type T = String
			def main(args: Array[T]) {}
		}
		object MainE2 {
			type AT = Array[String]
			def main(args: AT) {}
		}
		object MainE3 {
			type U = Unit
			type T = String
			def main(args: Array[T]): U = ()
		}
		object MainE4 {
			def main[T](args: Array[String]) {}
		}
		object MainE5 {
			type A[T] = Array[String]
			def main[T](args: A[T]) {}
		}
		""" ::
		Nil
	val sources = for((source, index) <- sourceContent.zipWithIndex) yield  new File("Main" + (index+1) + ".scala") -> source

	"Analysis plugin should detect applications" in {
		WithFiles(sources : _*) { case files @ Seq(main, main2, main3, main4, main5, main6, main7, main8, main9, mainA, mainB, mainC, mainD, mainE) =>
			CallbackTest(files, Nil) { (callback, file, log) =>
				val expected = Seq( main -> "Main", main4 -> "Main4", main8 -> "Main8", main9 -> "Main9", mainB -> "MainB",
					mainE -> "MainE1", mainE -> "MainE2", mainE -> "MainE3", mainE -> "MainE4", mainE -> "MainE5" )
				(callback.applications) must haveTheSameElementsAs(expected)
				val loader = new URLClassLoader(Array(file.toURI.toURL), getClass.getClassLoader)
				for( (_, className) <- expected) testRun(loader, className)
			}
		}
	}
	private def testRun(loader: ClassLoader, className: String)
	{
		val obj = Class.forName(className+"$", true, loader)
		val singletonField = obj.getField("MODULE$")
		val singleton = singletonField.get(null)
		singleton.asInstanceOf[{def main(args: Array[String]): Unit}].main(Array[String]())
	}
}