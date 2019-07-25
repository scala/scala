package scala.tools.nsc.util

import java.io.{BufferedOutputStream, FileOutputStream}
import java.util.jar.{JarOutputStream, Manifest}

import scala.reflect.io.AbstractFile

trait JarFactory {
  def createJarOutputStream(file: AbstractFile, manifest: Manifest): JarOutputStream
}
final class DefaultJarFactory extends JarFactory{
  override def createJarOutputStream(file: AbstractFile, manifest: Manifest): JarOutputStream =
    new JarOutputStream(new BufferedOutputStream(new FileOutputStream(file.file), 64000), manifest)
}
