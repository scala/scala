import sbt._
import Keys._

/** Reflection helper that runs ScalaTool. 
 * TODO - When SBT is on 2.10.x try to use Dynamic + Reflection. COULD BE FUN.
 */
class ScalaToolRunner(classpath: Classpath) {
  // TODO - Don't use the ant task directly...
  lazy val classLoader        = new java.net.URLClassLoader(classpath.map(_.data.toURI.toURL).toArray, null)
  lazy val mainClass          = classLoader.loadClass("scala.tools.ant.ScalaTool")
  lazy val executeMethod      = mainClass.getMethod("execute")
  lazy val setFileMethod      = mainClass.getMethod("setFile", classOf[java.io.File])
  lazy val setClassMethod     = mainClass.getMethod("setClass", classOf[String])
  lazy val setClasspathMethod = mainClass.getMethod("setClassPath", classOf[String])
  lazy val instance           = mainClass.newInstance()
    
  def setClass(cls: String): Unit    = setClassMethod.invoke(instance, cls)
  def setFile(file: File): Unit      = setFileMethod.invoke(instance, file)
  def setClasspath(cp: String): Unit = setClasspathMethod.invoke(instance, cp)
  def execute(): Unit                = executeMethod.invoke(instance)
}
