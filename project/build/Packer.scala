import sbt._
import java.io.{File, FileInputStream}
import java.util.jar.Manifest
import AdditionalResources._
import FileUtilities._



object Packer {
 
  /**
   * A filter that exclude files that musn't be in a jar file.
   */
  // We must exclude the manifest because we generate it automatically, and when we add multiples other jars, they could have
  // also a manifest files each, resulting in conflicts for the FileUtilities.jar(..) method
  def jarPattern(path: PathFinder) = path.descendentsExcept(AllPassFilter, (".*" - ".") || HiddenFileFilter || new ExactFilter("MANIFEST.MF")).get
  
  def createJar(j: Packaging, log: Logger): Option[String] = createJar(j.packagingConfig, log, jarPattern _, true)
  def createJar(j: PackagingConfiguration, log: Logger): Option[String] = createJar(j, log, jarPattern _, true)


  /**
   * Create a jar from the packaging trait. Is able to add directly others jars to it
   */
  def createJar(j: PackagingConfiguration, log: Logger, filter:(PathFinder) => Iterable[Path], addIncludedLibs: Boolean): Option[String] = {
    def pack0(content: Iterable[Path])= jar(content.flatMap(filter(_)), j.jarDestination, j.manifest, false, log)

    j.jarsToInclude match {
      case Nil => pack0(j.content)      
      case list if addIncludedLibs => {
        withTemporaryDirectory(log) { tmp: File =>
           val tmpPath = Path.fromFile(tmp)
           log.debug("List of jars to be added : " +list)
           def unzip0(l: List[Path]): Option[String] = l match {
              case x :: xs => {unzip(x, tmpPath, log);unzip0(xs)} //TODO properly handle failing of unzip
              case Nil => None
           }
           unzip0(list)
           log.debug("Content of temp folder"+ tmpPath.##.**( GlobFilter("*")))
           pack0(j.content ++ Set(tmpPath ##))
       }
      }
      case _ => pack0(j.content)
      
    }
  }

}

/**
 * Create the jars of pack
 * @author Grégory Moix
 */
trait Packer {
  self: BasicLayer =>
  
  def libraryToCopy: List[Path] = Nil
   
  /**
   * The actual pack task.
   */
  def packF =  {
    import Packer._
    def iterate(steps: List[Step]): Option[String] = steps match {
        case x :: xs => x match {
          case c: Packaging => {
            createJar(c, log) orElse iterate(xs)
          }
          case _ => iterate(xs)
        }
        case Nil => None
      }

     def copy0 ={
       copyFile(manifestPath,packingDestination/"META-INF"/"MANIFEST.MF", log) orElse { 
       copy(libraryToCopy, packingDestination , true, true, log) match {
          case Right(_) => None
          case Left(e) => Some(e)
          }
       }
     }
     iterate(allSteps.topologicalSort) orElse copy0
  }
  lazy val pack = task {packF}.dependsOn(finishLayer)
}


class PackagingConfiguration(val jarDestination: Path, val content: Iterable[Path], val manifest: Manifest, val jarsToInclude: List[Path]){
  def this(jarDestination: Path, content: Iterable[Path])= this(jarDestination, content, new Manifest, Nil)
  def this(jarDestination: Path, content: Iterable[Path], jarsToInclude: List[Path])= this(jarDestination, content, new Manifest, jarsToInclude)
}

trait Packaging extends Step {
    def packagingConfig: PackagingConfiguration 
}

trait WrapperPackaging extends Packaging {
  self : WrapperStep =>

  def jarContent = {
    def getContent(list: List[Step], acc: List[Path]): List[Path] = list match {
      case Nil => acc
      case x :: xs => x match {
        case w: WrapperStep => getContent(xs, getContent(w.dependencies.toList, acc))
        case c: CompilationStep => getContent(xs, (c.outputDirectory ##) :: acc)
      }
    }
    getContent(dependencies.toList, Nil)
  }  
}

/**
 * This trait is here to add the possiblity to have a different packing destination that is used right after the
 * compilation of the step has finished. It permits to have use libraries that are build using a plugin. (The plugin must
 * be a jar in order to be recognised by the compiler.
 */
trait EarlyPackaging extends Packaging {
  self: CompilationStep  =>
  //def earlyPackagingDestination: Path
  //def earlyJarDestination = earlyPackagingDestination / jarName
  def earlyPackagingConfig: PackagingConfiguration
}
