import sbt._
import xsbt.{ScalaInstance}
import BasicLayer._
import scala.collection.immutable.{EmptyMap}

/**
 * Basic tasks and configuration shared by all layers. This class regroups the configuration and behaviour
 * shared by all layers.
 * @author Grégory Moix
 */
abstract class BasicLayer(val info:ProjectInfo,val versionNumber:String, previousLayer:Option[BasicLayer]) extends Project with ReflectiveProject
        with AdditionalResources with Compilation{
  override def dependencies = info.dependencies
  lazy val projectRoot = info.projectPath

  lazy val copyright = property[String]
  lazy val partestVersionNumber = property[Version]


  /**
   * Before compiling the layer, we need to check that the previous layer
   * was created correctly and compile it if necessary
   */
  lazy val startLayer = previousLayer match {
    case Some(previous) => task{
      None
    }.dependsOn(previous.finishLayer)
    case None => task{None}
  }

  lazy val build= task{
    None
  }.dependsOn(externalCompilation,copyAdditionalFiles,writeProperties)

  /**
   * Finish the compilation and ressources copy and generation
   * It does nothing in itself. As sbt doesn't support conditional dependencies,
   * it permit locker to override it in order to lock the layer when the compilation
   * is finished.
   */
  lazy val finishLayer = task{None}.dependsOn(build)

  def instanceScope[A](action: ScalaInstance => A):A={
    val instance = ScalaInstance(instantiationLibraryJar.asFile, instantiationCompilerJar.asFile, info.launcher, msilJar.asFile, fjbgJar.asFile)
    log.debug("Compiler will be instantiated by :" +instance.compilerJar +" and :" +instance.libraryJar )
    action(instance)
  }

  // All path values must be lazy in order to avoid initialization issues (sbt way of doing things)
  lazy val layerOutput = outputRootPath / name
  lazy val pathLayout = new PathLayout(projectRoot, layerOutput)
  lazy val manifestPath = projectRoot/"META-INF"/"MANIFEST.MF"

  // Utility methods (for quick access)
  def libraryOutput = libraryConfig.outputDirectory
  def actorsOutput = actorsConfig.outputDirectory
  def dbcOutput = dbcConfig.outputDirectory
  def swingOutput = swingConfig.outputDirectory
  def scalapOutput = scalapConfig.outputDirectory
  def librarySrcDir = libraryConfig.srcDir
  def compilerOutput = compilerConfig.outputDirectory
  def compilerSrcDir = compilerConfig.srcDir
  def actorsSrcDir = actorsConfig.srcDir
  def swingSrcDir = swingConfig.srcDir
  def outputLibraryJar = libraryWS.jarDestination
  def outputCompilerJar = compilerConfig.jarDestination
  def outputPartestJar = partestConfig.jarDestination
  def outputScalapJar = scalapConfig.jarDestination

  // CONFIGURATION OF THE COMPILTATION STEPS

 /**
   *  Configuration of the core library compilation
   */
  lazy val libraryConfig = new CompilationStep("library", pathLayout ,log) with ResourcesToCopy with PropertiesToWrite{
    def label = "["+name+"] library"
    def options: Seq[String] = Seq("-sourcepath", pathConfig.sources.absolutePath.toString)
    def dependencies = Nil
    override def classpath = super.classpath +++ forkJoinJar

    def copyDestination = outputDirectory
    def filesToCopy = getResources(srcDir)

    def propertyDestination = outputDirectory / "library.properties"
    def propertyList = ("version.number",versionNumber)::("copyright.string",copyright.value)::Nil
  }

  /**
   * Configuration of the compiler
   */
  lazy val compilerConfig = new CompilationStep("compiler", pathLayout, log) with ResourcesToCopy with PropertiesToWrite with Packaging{
    def label = "["+name+"] compiler"
    private def bootClassPath : String = {
      System.getProperty("sun.boot.class.path")
    }
    override def classpath: PathFinder = super.classpath +++ fjbgJar +++ msilJar +++ jlineJar +++ antJar +++ forkJoinJar
    def options  = Seq("-bootclasspath",bootClassPath)
    def dependencies = if (minimalCompilation) libraryConfig::Nil else libraryConfig::actorsConfig::dbcConfig::swingConfig::Nil

    def copyDestination = outputDirectory
    def filesToCopy = getResources(srcDir)

    def propertyDestination = outputDirectory / "compiler.properties"
    def propertyList = ("version.number",versionNumber)::("copyright.string",copyright.value)::Nil

    def packagingDestination:Path = packingDestination
    def jarName:String = compilerJarName
    override def jarsToInclude = compilerAdditionalJars
    override def manifest = {
      import java.util.jar.Manifest
      import java.io.FileInputStream
      new Manifest(new FileInputStream(manifestPath.asFile))
    }
    override def jarContent = List(outputDirectory ##)

  }

  /**
   * Config of the actors library
   */
  lazy val actorsConfig = new CompilationStep ("actors", pathLayout,log){
    def label = "["+name+"] actors library"
    override def classpath: PathFinder = super.classpath +++ forkJoinJar
    def options: Seq[String] = Seq()
    def dependencies = libraryConfig::Nil
  }

  /**
   * Config of the dbc library
   */
  lazy val dbcConfig = new CompilationStep("dbc", pathLayout, log) with Packaging{
    def label = "["+name+"] dbc library"
    def options: Seq[String] = Seq()
    def dependencies = libraryConfig::Nil

    def packagingDestination=packingDestination
    def jarName = dbcJarName
    def jarContent = List(outputDirectory ##)

  }

  /**
   * Config of the swing library
   */
  lazy val swingConfig = new CompilationStep("swing", pathLayout, log) with Packaging{
    def label = "["+name+"] swing library"
    def options: Seq[String] = Seq()
    def dependencies = libraryConfig::actorsConfig::Nil

    def packagingDestination=packingDestination
    def jarName = swingJarName
    def jarContent = List(outputDirectory ##)

  }


  /**
   *  Configuration of scalap tool
   */
  lazy val scalapConfig  = new CompilationStep("scalap", pathLayout,log) with Packaging{
    def label = "["+name+"] scalap"
    def options: Seq[String] = Seq()
    def dependencies = libraryConfig::compilerConfig::Nil

    def packagingDestination=packingDestination
    def jarName = scalapJarName
    def jarContent = {
      val decoderProperties = (srcDir ## )/ "decoder.properties"

      List(outputDirectory ##, decoderProperties)
   }
  }

  /**
   * Configuration of the partest tool
   */
  lazy val partestConfig = new CompilationStep("partest", pathLayout,log) with ResourcesToCopy with PropertiesToWrite with Packaging{
    def label = "["+name+"] partest"
    override def classpath: PathFinder = super.classpath +++ antJar +++ forkJoinJar
    def options: Seq[String] = Seq()
    def dependencies = libraryConfig::compilerConfig::scalapConfig::actorsConfig::Nil

    def copyDestination = outputDirectory
    def filesToCopy = getResources(srcDir)

    def propertyDestination = outputDirectory / "partest.properties"
    def propertyList = ("version.number",partestVersionNumber.value.toString)::("copyright.string",copyright.value)::Nil

    def packagingDestination=packingDestination
    def jarName = partestJarName
    def jarContent = List(outputDirectory ##)
  }

  // Grouping compilation steps
  def minimalCompilation = false // It must be true for locker because we do not nedd to compile everything

  def libraryWS:WrapperStep with Packaging
  def toolsWS:WrapperStep


  lazy val allSteps = new WrapperStep(libraryWS::compilerConfig::toolsWS::Nil)





  //Needed Libraries
  //TODO Check if not possible to manage some of them with the sbt dependency management (ivy)
  lazy val lib = projectRoot / "lib"
  lazy val forkJoinJar = lib / forkJoinJarName
  lazy val jlineJar = lib / jlineJarName
  lazy val antJar = lib / "ant" / "ant.jar"
  lazy val fjbgJar = lib / fjbgJarName
  lazy val msilJar = lib /  msilJarName




  //Paths location that must be defined layer by layer
  /*
   * We must define which are the libraries used to instantiate the compiler
   * that will be used to compile this layer.
   */
  def instantiationCompilerJar:Path
  def instantiationLibraryJar:Path
  def packingDestination :Path = layerOutput / "pack"
  def compilerAdditionalJars: List[Path] = Nil
  def libraryAdditionalJars: List[Path] = Nil


  /**
   * Environment for storing properties that
   * 1) need to be saved across sbt session
   * 2) Are local to a layer
   * Used to save the last version of the compiler used to build the layer (for discarding it's product if necessary)
   */
  lazy val layerEnvironment = new BasicEnvironment {
    // use the project's Logger for any properties-related logging
    def log = BasicLayer.this.log

    // the properties file will be read from/stored to project/extra.properties
    def envBackingPath = outputRootPath / ("build-"+name+".properties")

    // define some properties that will go in project/extra.properties
    lazy val lastCompilerVersion:Property[String] = propertyOptional[String]("")
  }
}

object BasicLayer{
  // Some path definitions related strings
  val compilerJarName = "scala-compiler.jar"
  val libraryJarName = "scala-library.jar"
  val scalapJarName = "scalap.jar"
  val dbcJarName = "scala-dbc.jar"
  val swingJarName = "scala-swing.jar"
  val partestJarName = "scala-partest.jar"
  val fjbgJarName = "fjbg.jar"
  val msilJarName = "msil.jar"
  val jlineJarName = "jline.jar"
  val forkJoinJarName = "forkjoin.jar"

  implicit def stringToGlob(s:String):NameFilter=GlobFilter(s)



}
