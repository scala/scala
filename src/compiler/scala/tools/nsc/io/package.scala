/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc

import scala.language.implicitConversions

package object io {
  // Forwarders from scala.reflect.io
  type AbstractFile = scala.reflect.io.AbstractFile
  val AbstractFile = scala.reflect.io.AbstractFile
  type Directory = scala.reflect.io.Directory
  val Directory = scala.reflect.io.Directory
  type File = scala.reflect.io.File
  val File = scala.reflect.io.File
  type Path = scala.reflect.io.Path
  val Path = scala.reflect.io.Path
  type PlainFile = scala.reflect.io.PlainFile
  val Streamable = scala.reflect.io.Streamable
  type VirtualDirectory = scala.reflect.io.VirtualDirectory
  type VirtualFile = scala.reflect.io.VirtualFile
  type ZipArchive = scala.reflect.io.ZipArchive

  type JManifest = java.util.jar.Manifest
  type JFile = java.io.File

  implicit def enrichManifest(m: JManifest): Jar.WManifest = Jar.WManifest(m)
}
