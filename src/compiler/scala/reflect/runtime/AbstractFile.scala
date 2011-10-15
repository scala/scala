package scala.reflect.runtime

class AbstractFile(val jfile: java.io.File) {
 def path: String = jfile.getPath()
 def canonicalPath: String = jfile.getCanonicalPath()
}