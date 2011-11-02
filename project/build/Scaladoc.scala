import sbt._
import xsbt.AnalyzingCompiler

trait Scaladoc {
  self: BasicLayer with Packer =>

  lazy val documentationDestination = outputRootPath / "scaladoc"
  lazy val libraryDocumentationDestination = documentationDestination / "library"
  lazy val compilerDocumentationDestination = documentationDestination / "compiler"
  lazy val libraryDoc = {
    val reflect = librarySrcDir / "scala" / "reflect"
    val runtime = librarySrcDir / "scala" / "runtime"

    ((librarySrcDir +++ actorsSrcDir +++ swingSrcDir)**("*.scala")---
            reflect / "Code.scala"  ---
            reflect / "Manifest.scala" ---
            reflect / "Print.scala" ---
            reflect / "Symbol.scala" ---
            reflect / "Tree.scala" ---
            reflect / "Type.scala" ---
            reflect / "TypedCode.scala" ---
            runtime /"ScalaRunTime.scala" ---
            runtime / "StreamCons.scala" ---
            runtime / "StringAdd.scala" ---
            runtime * ("*$.scala") ---
            runtime *("*Array.scala")
            )

  }
  lazy val compilerDoc = {
          compilerSrcDir **("*.scala")
  }
  lazy val classpath ={
    (antJar +++ jlineJar +++ msilJar +++ fjbgJar +++ forkJoinJar +++ outputLibraryJar +++ outputCompilerJar  +++ outputPartestJar +++ outputScalapJar ).get
    
  }
  lazy val scaladoc = task(maybeFork(generateScaladoc, "Error generating scaladoc")) dependsOn pack

  lazy val generateScaladoc = task {
    instanceScope[Option[String]]{ scala =>
      lazy val compiler = new AnalyzingCompiler(scala, componentManager, xsbt.ClasspathOptions.manual, log)
      val docGenerator = new sbt.Scaladoc(50, compiler)
      docGenerator("Scala "+ versionNumber+" API", libraryDoc.get, classpath, libraryDocumentationDestination, Seq(), log) orElse
      docGenerator("Scala Compiler"+ versionNumber+" API", compilerDoc.get, classpath, compilerDocumentationDestination, Seq(), log)
    }
  }

}
