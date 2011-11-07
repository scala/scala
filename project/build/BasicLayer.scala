import sbt._
import xsbt.ScalaInstance
import ScalaBuildProject._

/**
 * Basic tasks and configuration shared by all layers. This class regroups the configuration and behaviour
 * shared by all layers.
 * @author Grégory Moix
 */
abstract class BasicLayer(val info: ProjectInfo, val versionNumber: String, previousLayer: Option[BasicLayer])
        extends ScalaBuildProject 
           with ReflectiveProject
           with AdditionalResources 
           with LayerCompilation
           with BuildInfoEnvironment 
           with ForkSBT {
  layer =>
  
  // All path values must be lazy in order to avoid initialization issues (sbt way of doing things)

  def buildInfoEnvironmentLocation: Path = outputRootPath / ("build-"+name+".properties")
  
  val forkProperty = "scala.sbt.forked"
  def isDebug  = info.logger atLevel Level.Debug
  def isForked = System.getProperty(forkProperty) != null

  // Support of triggered execution at project level
  override def watchPaths = info.projectPath / "src" ** ("*.scala" || "*.java" || AdditionalResources.basicFilter)
  override def dependencies = info.dependencies

  lazy val copyright = property[String]
  lazy val partestVersionNumber = property[Version]
  
  lazy val nextLayer: Option[BasicLayer] = None
  def packingDestination : Path = layerOutput / "pack"  
  lazy val libsDestination = packingDestination/ "lib"
  lazy val packedStarrOutput = outputRootPath / "pasta"
  lazy val requiredPluginsDirForCompilation = layerOutput / "misc" / "scala-devel" / "plugins"
  
  def compilerAdditionalJars: List[Path] = Nil
  def libraryAdditionalJars: List[Path] = Nil

  // TASKS

  /**
   * Before compiling the layer, we need to check that the previous layer
   * was created correctly and compile it if necessary
   */
  lazy val startLayer = previousLayer match {
    case Some(previous) => task(None) dependsOn previous.finishLayer
    case _              => task(None)
  }

  def buildLayer = externalCompilation orElse writeProperties

  lazy val build = compile
  
  lazy val compile = task(buildLayer) dependsOn startLayer

  /**
   * Finish the compilation and ressources copy and generation
   * It does nothing in itself. As sbt doesn't support conditional dependencies,
   * it permit locker to override it in order to lock the layer when the compilation
   * is finished.
   */
  lazy val finishLayer: ManagedTask = task(None) dependsOn compile
  
  def cleaningList = List(
    layerOutput,
    layerEnvironment.envBackingPath,
    packingDestination
  )

  def  cleanFiles = FileUtilities.clean(cleaningList, true, log)

  // We use super.task, so cleaning is done in every case, even when locked
  lazy val clean: Task = nextLayer match {
    case Some(next)   => super.task(cleanFiles) dependsOn next.clean
    case _            => super.task(cleanFiles)    
  }
  lazy val cleanBuild = task(cleanFiles orElse buildLayer) dependsOn startLayer

  // Utility methods (for quick access)
  def actorsOutput      = actorsConfig.outputDirectory
  def actorsSrcDir      = actorsConfig.srcDir
  def compilerOutput    = compilerConfig.outputDirectory
  def compilerSrcDir    = compilerConfig.srcDir
  def dbcOutput         = dbcConfig.outputDirectory
  def libraryOutput     = libraryConfig.outputDirectory
  def librarySrcDir     = libraryConfig.srcDir
  def outputCompilerJar = compilerConfig.packagingConfig.jarDestination
  def outputLibraryJar  = libraryWS.packagingConfig.jarDestination
  def outputPartestJar  = partestConfig.packagingConfig.jarDestination
  def outputScalapJar   = scalapConfig.packagingConfig.jarDestination 
  def scalapOutput      = scalapConfig.outputDirectory
  def swingOutput       = swingConfig.outputDirectory
  def swingSrcDir       = swingConfig.srcDir

  // CONFIGURATION OF THE COMPILATION STEPS
   
 /**
   *  Configuration of the core library compilation
   */
  lazy val libraryConfig = new CompilationStep("library", pathLayout , log) with ResourcesToCopy with PropertiesToWrite {
    def label = "["+layer.name+"] library"
    def options: Seq[String] = Seq("-sourcepath", pathConfig.sources.absolutePath.toString)
    def dependencies = Nil
    override def classpath = super.classpath +++ forkJoinJar

    def copyDestination = outputDirectory
    def filesToCopy = getResources(srcDir)
    
    def propertyDestination = outputDirectory / "library.properties"
    def propertyList = ("version.number",versionNumber) :: ("copyright.string", copyright.value) :: Nil
  }

  /**
   * Configuration of the compiler
   */
  lazy val compilerConfig = new CompilationStep("compiler", pathLayout, log) with ResourcesToCopy with PropertiesToWrite with Packaging {
    def label = "["+layer.name+"] compiler"
    private def bootClassPath : String = {
      System.getProperty("sun.boot.class.path")
    }
    override def classpath: PathFinder = super.classpath +++ fjbgJar +++ msilJar +++ jlineJar +++ antJar +++ forkJoinJar
    def options  = Seq("-bootclasspath", bootClassPath)
    def dependencies = if (minimalCompilation) libraryConfig :: Nil else libraryConfig :: actorsConfig :: dbcConfig :: swingConfig :: Nil

    def copyDestination = outputDirectory
    def filesToCopy = getResources(srcDir)

    def propertyDestination = outputDirectory / "compiler.properties"
    def propertyList = ("version.number",versionNumber) :: ("copyright.string", copyright.value) :: Nil

    lazy val packagingConfig = {
      import java.util.jar.Manifest
      import java.io.FileInputStream
      val manifest = new Manifest(new FileInputStream(manifestPath.asFile))
      new PackagingConfiguration(libsDestination / compilerJarName, List(outputDirectory ##), manifest , compilerAdditionalJars)
    }
    lazy val starrPackagingConfig = new PackagingConfiguration(packedStarrOutput/compilerJarName, List(outputDirectory ##))
    
  }

  //// ADDTIONNAL LIBRARIES ////

  /**
   * Config of the actors library
   */
  lazy val actorsConfig = new CompilationStep ("actors", pathLayout, log){
    def label = "["+layer.name+"] actors library"
    override def classpath: PathFinder = super.classpath +++ forkJoinJar
    def options: Seq[String] = Seq()
    def dependencies = libraryConfig :: Nil
  }

  /**
   * Config of the dbc library
   */
  lazy val dbcConfig = new CompilationStep("dbc", pathLayout, log) with Packaging {
    def label = "["+layer.name+"] dbc library"
    def options: Seq[String] = Seq()
    def dependencies = libraryConfig :: Nil
    
    lazy val packagingConfig = new PackagingConfiguration(
      libsDestination / dbcJarName,
      List(outputDirectory ##)
    )
  }

  /**
   * Config of the swing library
   */
  lazy val swingConfig = new CompilationStep("swing", pathLayout, log) with Packaging {
    def label = "["+layer.name+"] swing library"
    def options: Seq[String] = Seq()
    def dependencies = libraryConfig :: actorsConfig :: Nil

    lazy val packagingConfig = new PackagingConfiguration(
      libsDestination / swingJarName,
      List(outputDirectory ##)
    )
  }

  ///// TOOLS CONFIGURATION ////////

  /**
   *  Configuration of scalacheck
   */
  lazy val scalacheckConfig  = new CompilationStep("scalacheck", pathLayout, log) with Packaging {
    def label = "["+layer.name+"] scalacheck"
    def options: Seq[String] = Seq()
    def dependencies = libraryConfig :: compilerConfig :: actorsConfig :: Nil

    lazy val packagingConfig = new PackagingConfiguration(
      libsDestination / scalacheckJarName,
      List(outputDirectory ##)
    )
  }

  /**
   *  Configuration of scalap tool
   */
  lazy val scalapConfig  = new CompilationStep("scalap", pathLayout, log) with Packaging {
    def label = "["+layer.name+"] scalap"
    def options: Seq[String] = Seq()
    def dependencies = libraryConfig :: compilerConfig :: Nil

    val decoderProperties = (srcDir ## ) / "decoder.properties"
      
    lazy val packagingConfig = new PackagingConfiguration(
      libsDestination / scalapJarName,
      List(outputDirectory ##, decoderProperties)
    )
  }

  /**
   * Configuration of the partest tool
   */
  lazy val partestConfig = new CompilationStep("partest", pathLayout, log) with ResourcesToCopy with PropertiesToWrite with Packaging {
    def label = "["+layer.name+"] partest"
    override def classpath: PathFinder = super.classpath +++ antJar +++ forkJoinJar
    def options: Seq[String] = Seq()
    def dependencies = libraryConfig :: compilerConfig :: scalapConfig :: actorsConfig :: Nil

    def copyDestination = outputDirectory
    def filesToCopy = getResources(srcDir)

    def propertyDestination = outputDirectory / "partest.properties"
    def propertyList = List(
      ("version.number", partestVersionNumber.value.toString),
      ("copyright.string", copyright.value)
    )
    
    lazy val packagingConfig = new PackagingConfiguration(libsDestination / partestJarName, List(outputDirectory ##))    
    
  }

  ///// PLUGINS CONFIGURATION ////////

  lazy val continuationPluginConfig = {
    val config = new PathConfig {
      def projectRoot: Path = pathLayout.projectRoot
      def sources: Path     = pathLayout.srcDir / "continuations" / "plugin"
      def analysis: Path    = pathLayout.analysisOutput / "continuations" / "plugin"
      def output: Path      = pathLayout.classesOutput / "continuations" / "plugin"     
    }
        
    new CompilationStep("continuation-plugin", config, log) with ResourcesToCopy with EarlyPackaging {
      def label = "["+layer.name+"] continuation plugin"
      def dependencies = libraryConfig :: compilerConfig :: Nil
      def options = Seq()

      def filesToCopy     = (sourceRoots ##) / "scalac-plugin.xml"
      def copyDestination = outputDirectory
      def jarContent      = List(outputDirectory ##)
      lazy val packagingConfig = new PackagingConfiguration(
        requiredPluginsDirForCompilation/"continuations.jar",
        List(outputDirectory ##)
      )
      lazy val earlyPackagingConfig = new PackagingConfiguration(
        pathLayout.outputDir / "misc" / "scala-devel" / "plugins" / "continuations.jar",
        List(outputDirectory ##)
      )
    }
  }

  lazy val continuationLibraryConfig = {
    val config = new PathConfig {
       def projectRoot: Path = pathLayout.projectRoot
       def sources: Path     = pathLayout.srcDir / "continuations" / "library"
       def analysis: Path    = pathLayout.analysisOutput / "continuations" / "library"
       def output: Path      = pathLayout.classesOutput / "continuations" / "library"     
     }
    
    new CompilationStep("continuation-library", config, log) {
      def label = "["+layer.name+"] continuation library"
      def dependencies = libraryConfig :: compilerConfig :: continuationPluginConfig :: Nil
      def options = Seq(
        "-Xpluginsdir",
        requiredPluginsDirForCompilation.absolutePath,
        "-Xplugin-require:continuations",
        "-P:continuations:enable"
      )
    }
  }

  // Grouping compilation steps
  def minimalCompilation = false // It must be true for locker because we do not need to compile everything
  
  def libraryWS: WrapperStep with Packaging
  def toolsWS: WrapperStep

  lazy val pluginsWS = new WrapperStep(continuationPluginConfig :: continuationLibraryConfig :: Nil) 
  lazy val allSteps  = new WrapperStep(libraryWS :: compilerConfig :: pluginsWS :: toolsWS :: Nil)
}
