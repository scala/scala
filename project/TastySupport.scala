package scala.build

import sbt._

/**
  * Settings to support validation of TastyUnpickler against the release of dotty with the matching TASTy version
  */
object TastySupport {
  val supportedTASTyRelease = "3.7.4" // TASTY: 28.7
  val scala3Compiler = "org.scala-lang" % "scala3-compiler_3" % supportedTASTyRelease
  val scala3Library = "org.scala-lang" % "scala3-library_3" % supportedTASTyRelease

  val CompilerClasspath = Configuration.of("TastySupport.CompilerClasspath", "TastySupport.CompilerClasspath")
  val LibraryClasspath = Configuration.of("TastySupport.LibraryClasspath", "TastySupport.LibraryClasspath")
}
