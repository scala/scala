package scala.reflect.io

import java.io.Closeable
import java.nio
import java.nio.file.{FileSystems, Files}


abstract class RootPath extends Closeable {
  def root: nio.file.Path
}

object RootPath {
  def apply(path: nio.file.Path, writable: Boolean): RootPath = {
    if (path.getFileName.toString.endsWith(".jar")) {
      import java.net.URI
      val zipFile = URI.create("jar:file:" + path.toUri.getPath)
      val env = new java.util.HashMap[String, String]()
      if (!Files.exists(path.getParent))
        Files.createDirectories(path.getParent)
      if (writable) {
        env.put("create", "true")
        if (Files.exists(path))
          Files.delete(path)
      }
      val zipfs = FileSystems.newFileSystem(zipFile, env)
      new RootPath {
        def root = zipfs.getRootDirectories.iterator().next()
        def close(): Unit = {
          zipfs.close()
        }
      }
    } else {
      new RootPath {
        override def root: nio.file.Path = path
        override def close(): Unit = ()
      }
    }
  }
}