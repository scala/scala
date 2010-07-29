import sbt._
import BasicLayer._

/**
 * This class is the entry point for building scala with SBT.
 * @author Grégory Moix
 */
class ScalaSBTBuilder(val info: ProjectInfo) extends Project with  ReflectiveProject {
  override def dependencies: Iterable[Project] = info.dependencies  ++ locker.dependencies ++ quick.dependencies ++ strap.dependencies
  override def shouldCheckOutputDirectories = false

  // Main Tasks

  lazy val replaceLocker=task{None}.dependsOn(locker.unlock)
  lazy val build = task{None}.dependsOn(quick.binPack,quick.binQuick)
  lazy val docs = quick.scaladoc

  // Top level variables

  /**
   * The version number of the compiler that will be created by the run of sbt. It is initialised once
   * the first time it is needed, meaning that this number will be kept
   * until sbt quit.
   */
  lazy val versionNumber:String ={
    def getTimeString:String ={
      import java.util.Calendar;
      import java.text.SimpleDateFormat;
      val formatString = "yyyyMMddHHmmss"
      new SimpleDateFormat(formatString) format( Calendar.getInstance.getTime)
    }
    def getVersion:String ={
      val version:String = projectVersion.value.toString
      val stopIndex = version.lastIndexOf('-')
      stopIndex match{
        case -1 => version
        case i => version substring(0,i)
      }
    }
    def getRevision:Int = {
      new SVN(info.projectPath).getRevisionNumber
    }

    getVersion+".r"+getRevision+"-b"+getTimeString
  }



  /* LAYER DEFINITIONS
   * We define here what's specific to each layer are they differ.
   * The common behavior is defined in the BasicLayer class
   * It is important that the class that extends BasicLayer are inner classes of ScalaSBTBuilder. If not, SBT will
   * not know what the main project definition is, as it will find many classes that extends Project
   */

  lazy val locker=project(info.projectPath,"locker", new LockerLayer(_))
  lazy val quick=project(info.projectPath,"quick",new QuickLayer(_,locker))
  lazy val strap=project(info.projectPath,"strap", new StrapLayer(_, quick))

  /**
   * Definition of what is specific to the locker layer. It implements SimplePacker in order to
   * be able to create palo (packed locker)
   */
  class LockerLayer(info:ProjectInfo) extends BasicLayer(info,versionNumber,None)  with Packer{

    import BasicLayer._

    lazy val instantiationCompilerJar = lib / compilerJarName
    lazy val instantiationLibraryJar = lib / libraryJarName
    lazy val lockFile = layerOutput / "locker.lock"

    /**
     * We override the definition of the task method in order to make the tasks of this layer
     * be executed only if the layer is not locked. Task of this layer that should be executed
     * whether the layer is locked or not should call super.task instead
     */
    override def task(action : => Option[String])=
      super.task{
        if (lockFile.exists) {
          log.info(name +" is locked")
          None
        }
        else action
      }

    /**
     * Task for locking locker
     */
    lazy val lock = super.task{
      log.info("locking "+name)
      FileUtilities.touch(lockFile,log)
    }

    /**
     * Task for unlocking locker
     */
    lazy val unlock = super.task{
      FileUtilities.clean(lockFile,log)
    }

    /**
     *  Making locker being locked when it has finished building
     */
    override lazy val finishLayer = lock.dependsOn(build)


    override lazy val packingDestination:Path = outputRootPath /"palo"

    /**
     *  We must override the compilation steps as we only want to compile
     * the core library (and not actors,dbc, scalap, partest)
     */
     override lazy val libraryWS = {
        new WrapperStep(libraryConfig::Nil) with WrapperPackaging{
          def jarName = libraryJarName
          def packagingDestination = packingDestination
         }
     }
     override val minimalCompilation = true
     override lazy val toolsWS = new WrapperStep(Nil)
  }


  /**
   * Definition of what is specific to the quick layer. It implements Packer in order to create pack, ScalaTools
   * for creating the binaries and Scaladoc to generate the documentation
   */
  class QuickLayer(info:ProjectInfo, previous:BasicLayer) extends BasicLayer(info,versionNumber,Some(previous))
          with Packer with ScalaTools with Scaladoc{

    lazy val instantiationCompilerJar = previous.compilerOutput
    lazy val instantiationLibraryJar = previous.libraryOutput


    override lazy val packingDestination:Path = outputRootPath/ "pack"
    override def libraryToCopy = jlineJar::Nil
    override def compilerAdditionalJars = msilJar::fjbgJar::Nil
    override def libraryAdditionalJars = forkJoinJar::Nil


    override lazy val libraryWS = new WrapperStep(libraryConfig::actorsConfig::dbcConfig::swingConfig::Nil)with Packaging{
        def jarName = libraryJarName
        def packagingDestination = packingDestination
        def jarContent = List(libraryConfig.outputDirectory ## , actorsConfig.outputDirectory ## )
        override def jarsToInclude = libraryAdditionalJars
      }
    override lazy val toolsWS = new WrapperStep(scalapConfig::partestConfig::Nil)



    /*
     * Defining here the creation of the binaries for quick and pack
     */
    private lazy val quickBinClasspath = libraryOutput::actorsOutput::dbcOutput::swingOutput::compilerOutput::scalapOutput::forkJoinJar::fjbgJar::msilJar::jlineJar::Nil
    private lazy val packBinClasspath  = Nil
    lazy val binQuick = tools(layerOutput / "bin", quickBinClasspath).dependsOn(finishLayer)
    lazy val binPack = tools(packingDestination / "bin", packBinClasspath).dependsOn(pack)


  }


  /**
   * Definition of what is specific to the strap layer
   */
  class StrapLayer(info:ProjectInfo, previous:BasicLayer) extends BasicLayer(info,versionNumber,Some(previous)) {

    lazy val instantiationCompilerJar = previous.compilerOutput
    lazy val instantiationLibraryJar = previous.libraryOutput

    override lazy val libraryWS = new WrapperStep(libraryConfig::actorsConfig::dbcConfig::swingConfig::Nil) with WrapperPackaging{
          def jarName = libraryJarName
          def packagingDestination = packingDestination
         }

    override lazy val toolsWS= new WrapperStep(scalapConfig::partestConfig::Nil)

  }
}
