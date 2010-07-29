import sbt._
import xsbt.{AnalyzingCompiler,ScalaInstance}

/**
 * This trait define the compilation task. It is possible to configure what it actually compiles by
 * overriding the compilationSteps methods (useful because locker does not compile everything)
 * @author Grégory Moix
 */
trait Compilation{
  self : BasicLayer =>


  def lastUsedCompilerVersion = layerEnvironment.lastCompilerVersion

  def cleanCompilation:Option[String]= {
    log.info("Cleaning the products of the compilation.")
    FileUtilities.clean(layerOutput::Nil,true,log)
  }

  /**
   * Execute the different compilation parts one after the others.
   */
  def compile:Option[String]={

    instanceScope[Option[String]]{  scala =>
      lazy val analyzing = new AnalyzingCompiler(scala,componentManager,xsbt.ClasspathOptions.manual,log)

      def compilerVersionHasChanged:Boolean={
        val lastVersion = lastUsedCompilerVersion.value
        !(lastVersion.compareTo(scala.actualVersion) == 0)

      }

      def checkAndClean:Option[String]={
        if (compilerVersionHasChanged){
          log.info("The compiler version used to build this layer has changed since last time.")
          lastUsedCompilerVersion.update(scala.actualVersion)
          layerEnvironment.saveEnvironment
          cleanCompilation
        }else{
          log.debug("The compiler version is unchanged. No need for cleaning.")
          None
        }
      }

      def compile0(steps:List[Step]):Option[String]= steps match{
        case x::xs => x match{
          case c:CompilationStep => {
            val conditional = new CompileConditional(c, analyzing)
            log.info("")
            conditional.run orElse compile0(xs)
          }
          case _ => compile0(xs)
        }
        case Nil => None
      }


      checkAndClean orElse compile0(allSteps.topologicalSort)
    }
  }

  /**
   * Run the actual compilation. Should not be called directly because it is executed on the same jvm and that
   * it could lead to memory issues. It is used only when launching a new sbt process to do the compilation.
   */
  lazy val compilation = task{compile}

  /**
   * Runs the compilation in another process in order to circumvent memory issues that
   * arises when compiling every layer on the same jvm.
   */
  lazy val externalCompilation:ManagedTask=task{
    val runner = new ExternalTaskRunner(projectRoot,this.name,compilation.name, log)
    runner.runTask
  }.dependsOn(startLayer)

  /**
   * This method permits to specify what are the different steps of the compilation process,
   * meaning that you can customize exactly what is compiled by overriding it. It is overriden by
   * the locker layer that doesn't compile actors, sbc, scalap, partest
   */
  def compilationSteps:List[CompileConfiguration] =
    libraryConfig::actorsConfig::dbcConfig::swingConfig::compilerConfig::scalapConfig::partestConfig::Nil

}
