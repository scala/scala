/*
 * Copyright (c) 2014 Contributor. All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Scala License which accompanies this distribution, and
 * is available at http://www.scala-lang.org/license.html
 */
package scala.tools.nsc.classpath

import scala.tools.nsc.io._
import scala.collection.immutable
import scala.tools.nsc.util.ClassRepresentation
import scala.tools.nsc.classpath.FileUtils._
import java.net.URL

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

  //    def asClasspathString = dir.path
  //    val sourcepaths: IndexedSeq[AbstractFile] = IndexedSeq(dir)

  private def traverse() = {
    val classBuf = immutable.Vector.newBuilder[ClassRepresentation[AbstractFile]]
    val packageBuf = immutable.Vector.newBuilder[FlatSourcePath]
    dir foreach {
      f =>
        if (!f.isDirectory && validSourceFile(f.name))
          ()//classBuf += ClassRep(None, Some(f)) // TODO this is problem
        else if (f.isDirectory && validPackage(f.name))
          packageBuf += new FlatSourcePath(f)
    }
    (packageBuf.result(), classBuf.result())
  }

  lazy val (packages, classes) = traverse()

  /** Empty string represents root package */
  override def packages(inPackage: String): Seq[PackageEntry] = ???

  override def classes(inPackage: String): Seq[ClassFileEntry] = ???

  override def list(inPackage: String): (Seq[PackageEntry], Seq[ClassFileEntry]) = ???

  override def findClassFile(name: String): Option[AbstractFile] = ???
}
