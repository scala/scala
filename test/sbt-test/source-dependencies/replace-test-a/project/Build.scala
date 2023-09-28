import sbt._
import Keys._
import java.net.URLClassLoader

object B extends Build
{
	lazy val root = Project("root", file(".")) settings( ss : _*)

	def ss = Seq(
		TaskKey[Unit]("check-first") <<= checkTask("First"),
		TaskKey[Unit]("check-second") <<= checkTask("Second")
	)
	private def checkTask(className: String) =
		fullClasspath in Configurations.Runtime map { runClasspath =>
			val cp = runClasspath.map(_.data.toURI.toURL).toArray
			Class.forName(className, false, new URLClassLoader(cp))
			()
		}
}
