import sbt._
import xsbt.{AnalyzingCompiler, ScalaInstance}
import FileUtilities._

/**
 * This trait define the compilation task.
* @author Grégory Moix
 */
trait Compilation {
  self : ScalaBuildProject with BuildInfoEnvironment =>

  def lastUsedCompilerVersion = layerEnvironment.lastCompilerVersion

  def instantiationCompilerJar: Path
  def instantiationLibraryJar: Path

  def instanceScope[A](action: ScalaInstance => A): A = {
    val instance = ScalaInstance(instantiationLibraryJar.asFile, instantiationCompilerJar.asFile, info.launcher, msilJar.asFile, fjbgJar.asFile)
    log.debug("Compiler will be instantiated by :" +instance.compilerJar +" and :" +instance.libraryJar )
    action(instance)
  }

  def compile(stepList: Step, clean:() => Option[String]): Option[String] = compile(stepList, Some(clean))
  def compile(stepList: Step): Option[String] = compile(stepList, None)
  /**
   * Execute the different compilation parts one after the others.
   */
  def compile(stepsList: Step, clean: Option[() => Option[String]]): Option[String] ={

    instanceScope[Option[String]]{  scala =>
      lazy val analyzing = new AnalyzingCompiler(scala, componentManager, xsbt.ClasspathOptions.manual, log)

      def compilerVersionHasChanged = lastUsedCompilerVersion.value != scala.actualVersion

      def checkAndClean(cleanFunction:() => Option[String]): Option[String] ={
        if (compilerVersionHasChanged) {
          log.info("The compiler version used to build this layer has changed since last time or this is a clean build.")
          lastUsedCompilerVersion.update(scala.actualVersion)
          layerEnvironment.saveEnvironment
          cleanFunction()
        } else {
          log.debug("The compiler version is unchanged. No need for cleaning.")
          None
        }
      }

      def compile0(steps: List[Step]): Option[String] = {
        steps foreach {
          case c: CompilationStep =>
            val conditional = new CompileConditional(c, analyzing)
            log.info("")
            val res = conditional.run orElse copy(c) orElse earlyPackaging(c)
            if (res.isDefined)
              return res
          case _ => ()
        }
        None
      }

      /**
       * When we finishe to compile a step we want to jar if necessary in order to 
       * be able to load plugins for the associated library
       */
      def earlyPackaging(step: CompilationStep): Option[String] = step match {
        case s: EarlyPackaging => {
          val c = s.earlyPackagingConfig
          log.debug("Creating jar for plugin")
          jar(c.content.flatMap(Packer.jarPattern(_)), c.jarDestination, c.manifest, false, log)
        }
        case _ => None
      }

      def copy(step: CompilationStep): Option[String] = step match {
        case s: ResourcesToCopy => s.copy
        case _ => None
      }

      def cleanIfNecessary: Option[String] = clean match {
        case None => None
        case Some(f) => checkAndClean(f)
      }
      cleanIfNecessary orElse compile0(stepsList.topologicalSort)
    }
  }


}

trait LayerCompilation extends Compilation {
  self : BasicLayer =>

  protected def cleanCompilation: Option[String] = {
    log.info("Cleaning the products of the compilation.")
    FileUtilities.clean(layerOutput :: Nil, true, log)
  }

  /**
   * Run the actual compilation. Should not be called directly because it is executed on the same jvm and that
   * it could lead to memory issues. It is used only when launching a new sbt process to do the compilation.
   */
  lazy val compilation = task {compile(allSteps, cleanCompilation _)}

  def externalCompilation: Option[String] = maybeFork(compilation)
}
