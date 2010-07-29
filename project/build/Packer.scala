import sbt._
import java.io.{File,FileInputStream}
import java.util.jar.Manifest
import BasicLayer._
import FileUtilities._

/**
 * Create the jars of pack
 * @author Grégory Moix
 */
trait Packer {
  self: BasicLayer =>

  def libraryToCopy:List[Path] = Nil

  protected def jarPattern(path:PathFinder) = path.descendentsExcept(AllPassFilter, defaultExcludes || new ExactFilter("MANIFEST.MF")).get

  def createJar(j:Packaging):Option[String] = {
    def pack0(content:Iterable[Path])=jar(content.flatMap(jarPattern(_)),j.jarDestination, j.manifest, false, log)
    j.jarsToInclude match {
      case Nil => pack0(j.jarContent)
      case list => {
        withTemporaryDirectory(log) { tmp: File =>
           val tmpPath = Path.fromFile(tmp)
           log.debug("List of jars to be added : " +list)
           def unzip0(l:List[Path]):Option[String] = l match {
              case x::xs => {unzip(x,tmpPath,log);unzip0(xs)} //TODO properly handle failing of unzip
              case Nil => None
           }
           unzip0(list)
           log.debug("Content of temp folder"+ tmpPath.##.**( GlobFilter("*")))
           pack0(j.jarContent ++ Set(tmpPath ##))
       }
      }
    }
  }

  lazy val pack= task {

    def iterate(steps:List[Step]):Option[String]= steps match{
        case x::xs => x match{
          case c:Packaging => {
            createJar(c) orElse iterate(xs)
          }
          case _ => iterate(xs)
        }
        case Nil => None
      }

     def copy0 ={
       copyFile(manifestPath,packingDestination/"META-INF"/"MANIFEST.MF",log) orElse {
       copy(libraryToCopy,packingDestination , true,true,log) match {
          case Right(_) => None
          case Left(e) => Some(e)
          }
       }
     }
     iterate(allSteps.topologicalSort) orElse copy0
  }.dependsOn(finishLayer)



}

trait Packaging extends Step{
  def manifest = new Manifest
  def jarDestination:Path = packagingDestination /"lib" / jarName
  def packagingDestination:Path
  def jarName:String
  def jarsToInclude:List[Path] = Nil
  def jarContent:Iterable[Path]

}

trait WrapperPackaging extends Packaging {
  self : WrapperStep =>

  def jarContent = {
    def getContent(list:List[Step],acc:List[Path]):List[Path]= list match {
      case Nil => acc
      case x::xs => x match {
        case w:WrapperStep => getContent(xs,getContent(w.dependencies.toList,acc))
        case c:CompilationStep => getContent(xs,(c.outputDirectory ##)::acc)
      }
    }
    getContent(dependencies.toList,Nil)
  }

}
