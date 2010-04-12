/* NEST (New Scala Test)
 * Copyright 2007-2010 LAMP/EPFL
 */

package scala.tools
package partest
package category

import java.lang.{ ClassLoader => JavaClassLoader }
import java.net.URL
import nsc.util.ScalaClassLoader
import nsc.io._

class PartestClassLoader(urls: Array[URL], parent: JavaClassLoader) extends ScalaClassLoader.URLClassLoader(urls, parent) {
  def this(urls: Array[URL])  = this(urls, null)
  def bytes(path: String)     = findBytesForClassName(path)
  def singleton(path: String) = tryToInitializeClass(path).get getField "MODULE$" get null

  /** Calls a method in an object via reflection.
   */
  def apply[T](className: String, methodName: String)(args: Any*): T = {
    def fail = error("Reflection failed on %s.%s".format(className, methodName))
    val clazz = tryToLoadClass(className) getOrElse fail
    val obj = singleton(className)
    val m = clazz.getMethods find (x => x.getName == methodName && x.getParameterTypes.size == args.size) getOrElse fail

    m.invoke(obj, args map (_.asInstanceOf[AnyRef]): _*).asInstanceOf[T]
  }
}

trait Analysis {
  self: Universe =>

  object Scalap extends DirBasedCategory("scalap") {
    val testSequence: TestSequence = List(isCheckPresent, compile, run, diff)
    override def denotesTest(p: Path) = p.isDirectory && (p.toDirectory.files exists (_.name == "result.test"))
    override def createTest(location: Path) = new ScalapTest(location)

    class ScalapTest(val location: Path) extends TestEntity {
      val category      = Scalap
      val scalapMain    = "scala.tools.scalap.Main$"
      val scalapMethod  = "decompileScala"

      override def classpathPaths = super.classpathPaths :+ build.scalap
      override def checkFile      = File(location / "result.test")

      private def runnerURLs        = build.classpathPaths ::: classpathPaths map (_.toURL)
      private def createClassLoader = new PartestClassLoader(runnerURLs.toArray, this.getClass.getClassLoader)

      val isPackageObject = containsString("package object")
      val suffix          = if (isPackageObject) ".package" else ""
      val className       = location.name.capitalize + suffix

      override def run() = loggingResult {
        def loader  = createClassLoader
        def bytes   = loader.bytes(className)

        trace("scalap %s".format(className))
        if (isDryRun) ""
        else loader[String](scalapMain, scalapMethod)(bytes, isPackageObject)
      }
    }
  }
}
