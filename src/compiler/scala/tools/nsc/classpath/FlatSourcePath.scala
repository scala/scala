/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package scala.tools.nsc.classpath

import scala.collection.immutable
import scala.tools.nsc.classpath.FileUtils._
import java.net.URL
import scala.tools.nsc.io.AbstractFile

/**
 * A Classpath containing source files
 */
// TODO basically copy paste from old implementations
// TODO it's mock, it won't work in this moment
case class FlatSourcePath(dir: AbstractFile) extends FlatClassPath {

  def validPackage(name: String) = (name != "META-INF") && (name != "") && (name.charAt(0) != '.')

  def validSourceFile(name: String) = endsScala(name) || endsJava(name)

  //  class SourcePath[T](dir: AbstractFile, val context: ClassPathContext[T]) extends ClassPath[T] {
  //    def name = dir.name
  //    override def origin = dir.underlyingSource map (_.path)
  override def asURLs: Seq[URL] = dir.toURLs()

  private def traverse() = {
    val classBuf = immutable.Vector.newBuilder[SourceFileEntry]
    val packageBuf = immutable.Vector.newBuilder[PackageEntry]
    dir foreach {
      f =>
        if (!f.isDirectory && validSourceFile(f.name))
          classBuf += SourceFileEntryImpl(f)
        else if (f.isDirectory && validPackage(f.name))
          packageBuf += PackageEntryImpl(f.name) // TODO what about nesting? and use proper value as name!
    }
    (packageBuf.result(), classBuf.result())
  }

  lazy val (packages, classes) = traverse()

  /** Empty string represents root package */
  // TODO jak directory?
  override def packages(inPackage: String): Seq[PackageEntry] = Seq.empty

  override def classes(inPackage: String): Seq[ClassFileEntry] = Seq.empty

  override def list(inPackage: String): (Seq[PackageEntry], Seq[ClassFileEntry]) = (Seq.empty, Seq.empty)

  override def findClassFile(name: String): Option[AbstractFile] = ???

  override def asClassPathStrings: Seq[String] = Seq(dir.path)

  override def asSourcePathString: String = asClassPathString
}