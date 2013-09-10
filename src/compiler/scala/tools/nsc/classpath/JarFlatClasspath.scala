package scala.tools.nsc.classpath

import java.util.jar.{JarEntry, JarFile}
import scala.collection.mutable.ArrayBuffer
import scala.reflect.io.FileZipArchive
import java.io.File
import scala.reflect.io.AbstractFile

class JarFlatClasspath(val jarFile: File) extends AbstractFileFlatClasspath(new FileZipArchive(jarFile))
