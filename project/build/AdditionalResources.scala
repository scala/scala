import sbt._
import java.util.jar.{Manifest}
import java.io.{FileInputStream}
import AdditionalResources._
/**
 * Additional tasks that are required to obtain a complete compiler and library pair, but that are not part of the
 * compilation task. It copies additional files and generates the properties files
 * @author Grégory Moix
 */
trait AdditionalResources {
  self : BasicLayer  =>
  
   def writeProperties: Option[String] = {
      def write0(steps: List[Step]): Option[String] = steps match {
        case x :: xs => x match {
        case c: PropertiesToWrite => {
            c.writeProperties orElse write0(xs)
          }
          case _ => write0(xs)
        }
        case Nil => None
      }
     write0(allSteps.topologicalSort)
  }
}

object AdditionalResources {
  /**
   * A FileFilter that defines what are the files that will be copied
   */
  lazy val basicFilter =  "*.tmpl" | "*.xml" | "*.js" | "*.css" | "*.properties" | "*.swf" | "*.png"
  implicit def stringToGlob(s: String): NameFilter = GlobFilter(s)  
}

trait ResourcesToCopy {
  self : CompilationStep =>
  
  def getResources(from: Path, filter: FileFilter): PathFinder = (from ##)** filter
  def getResources(from: Path): PathFinder = getResources(from, AdditionalResources.basicFilter)
  
  def copyDestination: Path
  def filesToCopy: PathFinder
  
  def copy = {
    log.info("Copying files for "+name)
    try   { FileUtilities.copy(filesToCopy.get, copyDestination, log) }
    catch { case e => Some(e.toString) }

    None
  }
}

trait PropertiesToWrite {
  self : CompilationStep =>

  def propertyList: List[(String, String)]
  def propertyDestination: Path

  def writeProperties: Option[String] ={
    import java.io._
    import java.util.Properties
    
    val properties = new Properties

    def insert(list: List[(String, String)]): Unit = 
     list foreach { case (k, v) => properties.setProperty(k, v) }
    
    try {
      insert(propertyList)
      val destFile = propertyDestination.asFile
      val stream = new FileOutputStream(destFile)
      properties.store(stream, null)
    }
    catch {
      case e: Exception => Some(e.toString)
    }
    None
  }

}

