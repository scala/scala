package scala.build

import sbt._
import sbt.internal.inc.ScalaInstance

object Bootstrap {
  def makeScalaInstance(
    state: State,
    scalaVer: String,
    scalaLibraryJar: File,
    compilerJar: File,
    allJars: Seq[File]
  ): ScalaInstance = {
    val loader = state.classLoaderCache(allJars.toList)
    val loaderLibraryOnly = state.classLoaderCache(List(scalaLibraryJar))
    new ScalaInstance(
      scalaVer,
      loader,
      loaderLibraryOnly,
      scalaLibraryJar,
      compilerJar,
      allJars.toArray,
      None)
  }
}
