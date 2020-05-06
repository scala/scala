package scala.build

import sbt._
import sbt.Keys.{ artifact, dependencyClasspath, moduleID, resourceManaged }

object ScaladocSettings {

  // when this changes, the integrity check in HtmlFactory.scala also needs updating
  val webjarResources = Seq(
    "org.webjars" % "jquery" % "3.5.1"
  )

  def extractResourcesFromWebjar = Def.task {
    def isWebjar(s: Attributed[File]): Boolean =
      s.get(artifact.key).isDefined && s.get(moduleID.key).exists(_.organization == "org.webjars")
    val dest = (resourceManaged.value / "webjars").getAbsoluteFile
    IO.createDirectory(dest)
    val classpathes = (dependencyClasspath in Compile).value
    val files: Seq[File] = classpathes.filter(isWebjar).flatMap { classpathEntry =>
      val jarFile = classpathEntry.data
      IO.unzip(jarFile, dest)
    }
    (files ** "*.min.js").get
  }

}
