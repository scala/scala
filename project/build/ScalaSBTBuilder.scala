import sbt._
import ScalaBuildProject._
import ScalaSBTBuilder._

/**
 * This class is the entry point for building scala with SBT.
 * @author Grégory Moix
 */
class ScalaSBTBuilder(val info: ProjectInfo)
            extends Project
               with ReflectiveProject
               with BasicDependencyProject
               // with IdeaProject
               with MavenStyleScalaPaths {
  /** This secret system property turns off transitive dependencies during change
   *  detection.  It's a short term measure.  BE AWARE! That means you can no longer
   *  trust sbt to recompile everything: it's only recompiling changed files.
   *  (The alternative is that adding a space to TraversableLike incurs a 10+ minute
   *  incremental build, which means sbt doesn't get used at all, so this is better.)
   */
  System.setProperty("sbt.intransitive", "true")
  
  // Required by BasicDependencyProject
  def fullUnmanagedClasspath(config: Configuration) = unmanagedClasspath

  override def dependencies: Iterable[Project] = (
    info.dependencies ++ 
    locker.dependencies ++ 
    quick.dependencies ++ 
    strap.dependencies ++ 
    libs.dependencies
  )
  override def shouldCheckOutputDirectories = false

  // Support of triggered execution at top level
  override def watchPaths = info.projectPath / "src" ** ("*.scala" || "*.java" || AdditionalResources.basicFilter)

  // Top Level Tasks
  lazy val buildFjbg     = libs.buildFjbg.describedAs(buildFjbgTaskDescription)
  lazy val buildForkjoin = libs.buildForkjoin.describedAs(buildForkjoinTaskDescription)
  lazy val buildMsil     = libs.buildMsil.describedAs(buildMislTaskDescription)
  lazy val clean         = quick.clean.dependsOn(libs.clean).describedAs(cleanTaskDescription)
  lazy val cleanAll      = locker.clean.dependsOn(libs.clean).describedAs(cleanAllTaskDescription)
  lazy val compile       = task {None}.dependsOn(quick.binPack, quick.binQuick).describedAs(buildTaskDescription)
  lazy val docs          = quick.scaladoc.describedAs(docsTaskDescription)
  lazy val newFjbg       = libs.newFjbg.describedAs(newFjbgTaskDescription)
  lazy val newForkjoin   = libs.newForkjoin.describedAs(newForkjoinTaskDescription)
  lazy val newLocker     = locker.newLocker.describedAs(newLockerTaskDescription)
  lazy val newMsil       = libs.newMsil.describedAs(newMsilTaskDescription)
  lazy val newStarr      = quick.newStarr.describedAs(newStarrTaskDescription)
  lazy val palo          = locker.pack.describedAs(paloTaskDescription)
  lazy val pasta         = quick.pasta.describedAs(pastaTaskDescription)
  lazy val stabilityTest = strap.stabilityTest.describedAs(stabilityTestTaskDescription)
  lazy val test          = quick.externalPartest.describedAs(partestTaskDescription)
  
  // Non-standard names for tasks chosen earlier which I point at the standard ones.
  lazy val build = compile
  lazy val partest = test
  
  // Top level variables

  /**
   * The version number of the compiler that will be created by the run of sbt. It is initialised once
   * the first time it is needed, meaning that this number will be kept
   * until sbt quit.
   */
  lazy val versionNumber: String ={
    def getTimeString: String = {
      import java.util.Calendar;
      import java.text.SimpleDateFormat;
      val formatString = "yyyyMMddHHmmss"
      new SimpleDateFormat(formatString) format Calendar.getInstance.getTime
    }
    def getVersion: String = projectVersion.value.toString takeWhile (_ != '-') mkString
    def getRevision: Int   = new SVN(info.projectPath) getRevisionNumber
    
    getVersion+".r"+getRevision+"-b"+getTimeString
  }

  /* LAYER DEFINITIONS
   * We define here what's specific to each layer are they differ.
   * The common behavior is defined in the BasicLayer class
   * It is important that the class that extends BasicLayer are inner classes of ScalaSBTBuilder. If not, SBT will
   * not know what the main project definition is, as it will find many classes that extends Project
   */

  lazy val locker = project(info.projectPath,"locker", new LockerLayer(_))
  lazy val quick = project(info.projectPath,"quick", new QuickLayer(_, locker))
  lazy val strap = project(info.projectPath,"strap", new StrapLayer(_, quick))
  lazy val libs = project(info.projectPath,"libs", new LibsBuilder(_))


  /**
   * Definition of what is specific to the locker layer. It implements SimplePacker in order to
   * be able to create palo (packed locker)
   */
  class LockerLayer(info: ProjectInfo) extends BasicLayer(info, versionNumber, None)  with Packer {


    override lazy val nextLayer = Some(quick)
    lazy val instantiationCompilerJar = lib / compilerJarName
    lazy val instantiationLibraryJar = lib / libraryJarName
    lazy val lockFile = layerOutput / "locker.lock"

    /**
     * We override the definition of the task method in order to make the tasks of this layer
     * be executed only if the layer is not locked. Task of this layer that should be executed
     * whether the layer is locked or not should call super.task instead
     */
    override def task(action : => Option[String])=
      super.task {
        if (lockFile.exists) {
          log.info(name +" is locked")
          None
        }
        else action
      }
    
    def deleteLock = FileUtilities.clean(lockFile, log)
    def createLock = {
      log.info("locking "+name)
      FileUtilities.touch(lockFile, log)      
    }

    /**
     * Task for locking locker
     */
    lazy val lock = super.task {
      createLock
    }

    /**
     * Task for unlocking locker
     */
    lazy val unlock = super.task {
      deleteLock
    }

    lazy val newLocker = super.task {
      createNewLocker
    }
    def createNewLocker = {
      deleteLock orElse
      buildLayer orElse
      createLock
    }


    /**
     *  Making locker being locked when it has finished building
     */
    override lazy val finishLayer = lock.dependsOn(build)

    override lazy val pack = super.task {packF}.dependsOn(finishLayer)


    override lazy val packingDestination: Path = outputRootPath /"palo"

    override lazy val libraryWS = { 
        new WrapperStep(libraryConfig :: Nil) with WrapperPackaging {
          lazy val packagingConfig = new PackagingConfiguration(libsDestination/libraryJarName, jarContent)
         }
     }
     override val minimalCompilation = true
     override lazy val pluginsWS: WrapperStep = new WrapperStep(Nil)   
     override lazy val toolsWS = new WrapperStep(Nil)
  }


  /**
   * Definition of what is specific to the quick layer. It implements Packer in order to create pack, ScalaTools
   * for creating the binaries and Scaladoc to generate the documentation
   */
  class QuickLayer(info: ProjectInfo, previous: BasicLayer) extends BasicLayer(info, versionNumber, Some(previous)) with PartestRunner
          with Packer with ScalaTools with Scaladoc {

    override lazy val nextLayer = Some(strap)
            

    lazy val instantiationCompilerJar = previous.compilerOutput
    lazy val instantiationLibraryJar = previous.libraryOutput


    override lazy val packingDestination: Path = outputRootPath/ "pack"
    
    override def libraryToCopy = jlineJar :: Nil
    override def compilerAdditionalJars = msilJar :: fjbgJar :: Nil
    override def libraryAdditionalJars = forkJoinJar :: Nil
    
    override def cleaningList = packedStarrOutput :: super.cleaningList
    

    override lazy val libraryWS = new WrapperStep(libraryConfig :: actorsConfig :: dbcConfig :: swingConfig :: Nil) with Packaging {
        def jarContent = List(libraryConfig , actorsConfig, continuationLibraryConfig).map(_.outputDirectory ##)
        lazy val starrJarContent = List(libraryConfig , actorsConfig, dbcConfig, swingConfig, continuationLibraryConfig).map(_.outputDirectory ##)
        lazy val packagingConfig = new PackagingConfiguration(libsDestination/libraryJarName, jarContent, libraryAdditionalJars)
        lazy val starrPackagingConfig = new PackagingConfiguration(packedStarrOutput/libraryJarName, starrJarContent)

    }

    override lazy val toolsWS = new WrapperStep(scalacheckConfig :: scalapConfig :: partestConfig :: Nil)

    // An additional task for building only the library of quick
    // Used for compiling msil
    lazy val compileLibraryOnly = task {
      compile(libraryConfig, cleanCompilation _)
    }
    lazy val externalCompileLibraryOnly = task(maybeFork(compileLibraryOnly)) dependsOn startLayer

    def createNewStarrJar: Option[String] ={
      import Packer._
      createJar(libraryWS.starrPackagingConfig, log) orElse
      createJar(compilerConfig.starrPackagingConfig, log)
    }
    lazy val pasta = task {
      createNewStarrJar    
    }.dependsOn(build)
    
    lazy val newStarr = task {
      val files = (packedStarrOutput ##) * "*.jar"
      FileUtilities.copy(files.get, lib, true, log) match {
        case Right(_) => None
        case Left(_) => Some("Error occured when copying the new starr to its destination")
      }

    }.dependsOn(pasta)

    /*
     * Defining here the creation of the binaries for quick and pack
     */
    private lazy val quickBinClasspath = libraryOutput :: actorsOutput :: dbcOutput :: swingOutput :: compilerOutput :: scalapOutput :: forkJoinJar :: fjbgJar :: msilJar :: jlineJar :: Nil
    private lazy val packBinClasspath  = Nil
    lazy val binQuick = tools(layerOutput / "bin", quickBinClasspath).dependsOn(finishLayer)
    lazy val binPack = tools(packingDestination / "bin", packBinClasspath).dependsOn(pack)
  }
  

  /**
   * Definition of what is specific to the strap layer
   */
  class StrapLayer(info: ProjectInfo, previous: BasicLayer) extends BasicLayer(info, versionNumber, Some(previous)) {

    lazy val instantiationCompilerJar = previous.compilerOutput
    lazy val instantiationLibraryJar = previous.libraryOutput
    private val quick = previous

    override lazy val libraryWS = new WrapperStep(libraryConfig :: actorsConfig :: dbcConfig :: swingConfig :: Nil) with WrapperPackaging {
        lazy val packagingConfig = new PackagingConfiguration(libsDestination/libraryJarName, Set())
          
         }

    override lazy val toolsWS = new WrapperStep(scalacheckConfig :: scalapConfig :: partestConfig :: Nil)


    def compare = {
      import PathConfig.classes
      def filter(path: Path)= path.descendentsExcept(AllPassFilter, HiddenFileFilter || "*.properties")
      Comparator.compare(quick.pathLayout.outputDir/classes ##, this.pathLayout.outputDir/classes ##, filter _ , log)
    }

    lazy val stabilityTest = task {
      log.warn("Stability test must be run on a clean build in order to yield correct results.")
      compare   
    }.dependsOn(finishLayer)
  }

  /**
   * An additional subproject used to build new version of forkjoin, fjbg and msil
   */
  class LibsBuilder(val info: ProjectInfo) extends ScalaBuildProject with ReflectiveProject with Compilation with BuildInfoEnvironment {
    override def dependencies = info.dependencies
    override def watchPaths = info.projectPath / "src" ** ("*.scala" || "*.java" ||AdditionalResources.basicFilter) // Support of triggered execution at project level
    
    
    def buildInfoEnvironmentLocation: Path = outputRootPath / ("build-"+name+".properties")

    def instantiationCompilerJar: Path = locker.compilerOutput
    def instantiationLibraryJar: Path = locker.libraryOutput

    def libsDestination = layerOutput

    lazy val checkJavaVersion = task {
      val version = System.getProperty("java.version")
      log.debug("java.version ="+version)
      val required = "1.6"
      if (version.startsWith(required)) None else Some("Incompatible java version : required "+required)
    }
    
    
    private def simpleBuild(step: CompilationStep with Packaging)= task {
      import Packer._
      compile(step) orElse createJar(step, log)
    }.dependsOn(locker.finishLayer)
    
    private def copyJar(step: CompilationStep with Packaging, name: String) = task {
      FileUtilities.copyFile(step.packagingConfig.jarDestination, lib/name, log)
    }

    lazy val newForkjoin = copyJar(forkJoinConfig, forkjoinJarName).dependsOn(buildForkjoin)
    lazy val buildForkjoin = simpleBuild(forkJoinConfig).dependsOn(checkJavaVersion)
    lazy val newFjbg = copyJar(fjbgConfig, fjbgJarName).dependsOn(buildFjbg)
    lazy val buildFjbg = simpleBuild(fjbgConfig)
    lazy val newMsil = copyJar(msilConfig, msilJarName).dependsOn(buildMsil)
    // TODO As msil contains scala files, maybe needed compile it with an ExternalSBTRunner
    lazy val buildMsil = simpleBuild(msilConfig).dependsOn(quick.externalCompileLibraryOnly)

    lazy val forkJoinConfig = new CompilationStep("forkjoin", pathLayout, log) with Packaging {
      def label = "new forkjoin library"
      override def sources: PathFinder  = sourceRoots.descendentsExcept("*.java", ".svn")
      def dependencies = Seq()
      def options = Seq()
      override def javaOptions = Seq("-target","1.5","-source","1.5","-g")          
      lazy val packagingConfig = new PackagingConfiguration(libsDestination/forkjoinJarName, List(outputDirectory ##))
    }
  
    lazy val fjbgConfig =  new CompilationStep("fjbg", pathLayout, log) with Packaging {
      def label = "new fjbg library"
      override def sources: PathFinder  = sourceRoots.descendentsExcept("*.java", ".svn")      
      def dependencies = Seq()
      def options = Seq()
      override def javaOptions = Seq("-target","1.5","-source","1.4","-g")
      lazy val packagingConfig = new PackagingConfiguration(libsDestination/fjbgJarName, List(outputDirectory ##))
      
    }

    lazy val msilConfig =  new CompilationStep("msil", pathLayout, log) with Packaging {
      def label = "new msil library"
      override def sources: PathFinder  = sourceRoots.descendentsExcept("*.java" |"*.scala", ".svn" |"tests")
      def dependencies = Seq()
      override def classpath = super.classpath +++ quick.libraryOutput
      def options = Seq()
      override def javaOptions = Seq("-target","1.5","-source","1.4","-g")      
      lazy val packagingConfig = new PackagingConfiguration(libsDestination/msilJarName, List(outputDirectory ##))
      
    }

    def cleaningList = layerOutput :: layerEnvironment.envBackingPath :: Nil

    def  cleanFiles = FileUtilities.clean(cleaningList, true, log)

    lazy val clean: Task = task {cleanFiles}// We use super.task, so cleaning is done in every case, even when locked
    
  }
}
object ScalaSBTBuilder {
  val buildTaskDescription = "build locker, lock it, build quick and create pack. It is the equivalent command to 'ant build'."
  val cleanTaskDescription = "clean the outputs of quick and strap. locker remains untouched."
  val cleanAllTaskDescription = "same as clean, but in addition clean locker too."
  val docsTaskDescription = "generate the scaladoc"
  val partestTaskDescription = "run partest"
  val stabilityTestTaskDescription = "run stability testing. It is required to use a clean build (for example, execute the clean-all action) in order to ensure correctness of the result."
  val paloTaskDescription = "create palo"
  val pastaTaskDescription = "create all the jar needed to make a new starr from quick (pasta = packed starr). It does not replace the current library and compiler jars in the libs folder, but the products of the task are instead located in target/pasta"
  val newStarrTaskDescription = "create a new starr and replace the library and compiler jars in the libs folder. It will keep locker locker locked, meaning that if you want to update locker after updating starr, you must run the 'new-locker' command. It will not automatically run partest and stability testing before replacing."
  val newLockerTaskDescription = "replace locker. It will build a new locker. It does not automatically rebuild quick."
  val buildForkjoinTaskDescription = "create all the jar needed to make a new forkjoin. It does not replace the current library and compiler jars in the libs folder, but the products of the task are instead located in target/libs."
  val newForkjoinTaskDescription = "create a new forkjoin and replace the corresponding jar in the libs folder."
  val buildFjbgTaskDescription = "create all the jar needed to make a new fjbg. It does not replace the current library and compiler jars in the libs folder, but the products of the task are instead located in target/libs."
  val newFjbgTaskDescription = "create a new fjbg and replace the corresponding jar in the libs folder."
  val buildMislTaskDescription = "create all the jar needed to make a new msil. It does not replace the current library and compiler jars in the libs folder, but the products of the task are instead located in target/libs."
  val newMsilTaskDescription = "create a msil and replace the corresponding jar in the libs folder."
}
