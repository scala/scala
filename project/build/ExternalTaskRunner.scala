import sbt._
import ExternalTaskRunner._

/**
 * Provide a way to launch a specific task in a new sbt instance.
 * As the compilation process need to be able to compile in a different process (due to memory related
 * performance issue) and that in order to keep incremental compilation, we allow to launch a task from a
 * specific project / sub-project in a different instance of sbt that disappear once the task has finished.
 */
class ExternalTaskRunner(root:Path,projectName:String, taskName :String, log: Logger ){

  def runTask:Option[String] ={

    val cmd:Seq[String] = Seq("project "+projectName,taskName) // TODO forward logging level (for example debug)
    val externalSbt = Process(sbtCommand ++ cmd)
    log.info("Launching task ["+taskName+"] of project ["+projectName+"] in new sbt instance")
    externalSbt.! match{
      case 0 => None
      case _ => Some("External Task Failed")
    }

  }

}

object ExternalTaskRunner{
  /**
   * parameters needed to launch another instance of sbt
   */
  val sbtCommand = Seq("sbt") // TODO remove dependency on sbt being on the path of the user



}