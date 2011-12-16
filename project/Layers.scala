import sbt._
import Keys._
import com.jsuereth.git.GitKeys.gitRunner

/** This trait stores all the helper methods to generate layers in Scala's layered build. */
trait Layers extends Build {
  // TODO - Clean this up or use a self-type.

  /** Default SBT overrides needed for layered compilation. */
  def settingOverrides: Seq[Setting[_]]
  /** Reference to the jline project */
  def jline: Project
  /** Reference to forkjoin library */
  def forkjoin: Project
  /** Reference to Fast-Java-Bytecode-Generator library */
  def fjbg: Project
  /** A setting that adds some external dependencies. */
  def externalDeps: Setting[_]

  /** Creates a reference Scala version that can be used to build other projects.   This takes in the raw
    * library, compiler and fjbg libraries as well as a string representing the layer name (used for compiling the compile-interface).
    */
  def makeScalaReference(layer : String, library: Project, compiler: Project, fjbg: Project) =
     scalaInstance <<= (appConfiguration in library,
                        version in library,
                        (exportedProducts in library in Compile),
                        (exportedProducts in compiler in Compile),
                        (exportedProducts in fjbg in Compile),
                        (fullClasspath in jline in Runtime)) map {
    (app, version: String, lib: Classpath, comp: Classpath, fjbg: Classpath, jline: Classpath) =>
      val launcher = app.provider.scalaProvider.launcher
      (lib,comp) match {
         case (Seq(libraryJar), Seq(compilerJar)) =>
           ScalaInstance(
             version + "-" + layer + "-",
             libraryJar.data,
             compilerJar.data,
             launcher,
             ((fjbg.files++jline.files):_*))
         case _ => error("Cannot build a ScalaReference with more than one classpath element")
      }
  }
  
  /** Creates a "layer" of Scala compilation.  That is, this will build the next version of Scala from a previous version.
   * Returns the library project and compiler project from the next layer.
   * Note:  The library and compiler are not *complete* in the sense that they are missing things like "actors" and "fjbg".
   */
  def makeLayer(layer: String, referenceScala: Setting[Task[ScalaInstance]]) : (Project, Project) = {
    val library = Project(layer + "-library", file("."))  settings(settingOverrides: _*) settings(
      version := layer,
      // TODO - use depends on.
      unmanagedClasspath in Compile <<= (exportedProducts in forkjoin in Compile).identity,
      managedClasspath in Compile := Seq(),
      scalaSource in Compile <<= (baseDirectory) apply (_ / "src" / "library"),
      resourceDirectory in Compile <<= baseDirectory apply (_ / "src" / "library"),   
      defaultExcludes in unmanagedResources := ("*.scala" | "*.java" | "*.disabled"),
      // TODO - Allow other scalac option settings.
      scalacOptions in Compile <++= (scalaSource in Compile) map (src => Seq("-sourcepath", src.getAbsolutePath)),
      classpathOptions := ClasspathOptions.manual,
      resourceGenerators in Compile <+= (baseDirectory, version, resourceManaged, gitRunner, streams) map Release.generatePropertiesFile("library.properties"),
      referenceScala
    )

    // Define the compiler
    val compiler = Project(layer + "-compiler", file(".")) settings(settingOverrides:_*) settings(
      version := layer,
      scalaSource in Compile <<= (baseDirectory) apply (_ / "src" / "compiler"),
      resourceDirectory in Compile <<= baseDirectory apply (_ / "src" / "compiler"),
      unmanagedSourceDirectories in Compile <+= (baseDirectory) apply (_ / "src" / "msil"),
      defaultExcludes := ("tests"),
      javacOptions ++= Seq("-source", "1.4"),
      defaultExcludes in unmanagedResources := "*.scala",
      resourceGenerators in Compile <+= (baseDirectory, version, resourceManaged, gitRunner, streams) map Release.generatePropertiesFile("compiler.properties"),
      // Note, we might be able to use the default task, but for some reason ant was filtering files out.  Not sure what's up, but we'll
      // stick with that for now.
      unmanagedResources in Compile <<= (baseDirectory) map {
        (bd) =>
          val dirs = Seq(bd / "src" / "compiler")
          dirs.descendentsExcept( ("*.xml" | "*.html" | "*.gif" | "*.png" | "*.js" | "*.css" | "*.tmpl" | "*.swf" | "*.properties" | "*.txt"),"*.scala").get
      },
      // TODO - Use depends on *and* SBT's magic dependency mechanisms...
      unmanagedClasspath in Compile <<= Seq(forkjoin, library, fjbg, jline).map(exportedProducts in Compile in _).join.map(_.flatten),
      classpathOptions := ClasspathOptions.manual,
      externalDeps,
      referenceScala
    )

    // Return the generated projects.
    (library, compiler)
  }

}
