package scala.reflect
package runtime

class AbstractFile(val jfile: java.io.File) extends internal.AbstractFileApi {
  def path: String = jfile.getPath()
  def canonicalPath: String = jfile.getCanonicalPath()
}
