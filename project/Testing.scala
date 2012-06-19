import sbt._
import Keys._
import partest._
import SameTest._
import ScalaBuildKeys._

/** All settings/projects relating to testing. */
trait Testing { self: ScalaBuild.type =>

  lazy val testsuiteSettings: Seq[Setting[_]] = compilerDependentProjectSettings ++ partestTaskSettings ++ VerifyClassLoad.settings ++ Seq(
    unmanagedBase <<= baseDirectory / "test/files/lib",
    fullClasspath in VerifyClassLoad.checkClassLoad <<= (fullClasspath in scalaLibrary in Runtime).identity,
    autoScalaLibrary := false,
    checkSameLibrary <<= checkSameBinaryProjects(quickLib, strappLib),
    checkSameCompiler <<= checkSameBinaryProjects(quickComp, strappComp),
    checkSame <<= (checkSameLibrary, checkSameCompiler) map ((a,b) => ()),
    autoScalaLibrary := false
  )
  lazy val continuationsTestsuiteSettings: Seq[Setting[_]] = testsuiteSettings ++ Seq(
    scalacOptions in Test <++= (exportedProducts in Compile in continuationsPlugin) map { 
     case Seq(cpDir) => Seq("-Xplugin-require:continuations", "-P:continuations:enable", "-Xplugin:"+cpDir.data.getAbsolutePath)
    },
    partestDirs <<= baseDirectory apply { bd =>
      def mkFile(name: String) = bd / "test" / "files" / name
      def mkTestType(name: String) = name.drop("continuations-".length).toString
      Seq("continuations-neg", "continuations-run") map (t => mkTestType(t) -> mkFile(t)) toMap
    }
  )
  val testsuite = (
    Project("testsuite", file(".")) 
    settings (testsuiteSettings:_*)
    dependsOn (scalaLibrary, scalaCompiler, fjbg, partest, scalacheck, actorsMigration)
  )
  val continuationsTestsuite = (
    Project("continuations-testsuite", file("."))
    settings (continuationsTestsuiteSettings:_*) 
    dependsOn (partest, scalaLibrary, scalaCompiler, fjbg, actorsMigration)
  )

}

