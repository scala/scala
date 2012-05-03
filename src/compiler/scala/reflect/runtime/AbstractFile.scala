package scala.reflect
package runtime

class AbstractFile(val jfile: java.io.File) extends api.RequiredFile {
  def path: String = jfile.getPath()
  def canonicalPath: String = jfile.getCanonicalPath()
}
