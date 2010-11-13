package scala.tools.nsc
package io

import java.io.IOException
import Properties.{ javaHome }
import Path.isJarOrZip
import java.util.concurrent.ExecutionException
import java.util.jar._
import java.util.zip.ZipException
import scala.util.matching.Regex
import collection.JavaConverters._

class SourceJar(jarfile: File) {
  private def fail(entry: JarEntry) = throw new IOException("No such entry: " + entry)

  val jarFile                     = new JarFile(jarfile.jfile)
  def iterator: Iterator[Fileish] = jarFile.entries.asScala map (x => Fileish(x, () => getStream(x)))
  def toList: List[Fileish]       = iterator.toList
  def getStream(entry: JarEntry)  = Option(jarFile getInputStream entry) getOrElse fail(entry)
  def filesNamed(name: String)    = iterator filter (_.name == name)

  override def toString = jarfile.toString
}
