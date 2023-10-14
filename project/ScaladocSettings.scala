package scala.build

import sbt._
import sbt.Keys.{ artifact, externalDependencyClasspath, moduleID, resourceManaged }

object ScaladocSettings {

  // when this changes, the integrity check in HtmlFactory.scala also needs updating
  val webjarResources = Seq(
    "org.webjars" % "jquery" % "3.7.1"
  )

  def extractResourcesFromWebjar = Def.task {
    def isWebjar(s: Attributed[File]): Boolean =
      s.get(artifact.key).isDefined && s.get(moduleID.key).exists(_.organization == "org.webjars")
    val dest = (resourceManaged.value / "webjars").getAbsoluteFile
    IO.createDirectory(dest)
    // externalDependencyClasspath (not dependencyClasspath) to avoid compiling
    // upstream projects (library, reflect, compiler) on bsp `buildTarget/resources`
    val classpaths = (Compile / externalDependencyClasspath).value
    val files: Seq[File] = classpaths.filter(isWebjar).flatMap { classpathEntry =>
      val jarFile = classpathEntry.data
      IO.unzip(jarFile, dest)
    }
    (files ** "*.min.js").get()
  }

}
