package tools.test.osgi
 
import org.ops4j.pax.exam.CoreOptions._
import org.ops4j.pax.exam
import java.io.File

trait ScalaOsgiHelper {

  private def allBundleFiles = {
    def bundleLocation = new File(sys.props.getOrElse("scala.bundle.dir", "build/osgi"))
    bundleLocation.listFiles filter (_.getName endsWith ".jar")
  }

  private def filteredBundleFiles(names: String*): Array[exam.Option] =
     for(bundle <- allBundleFiles; if names exists (bundle.getName contains))
     yield makeBundle(bundle)

  private def makeBundle(file: File): exam.Option =
      bundle(file.toURI.toASCIIString)

  def standardOptions: Array[exam.Option]  = {
    val bundles = (allBundleFiles map makeBundle)
    bundles ++ Array[exam.Option](felix(), equinox(), junitBundles())
  }

  def justReflectionOptions: Array[exam.Option]  = {
    val bundles = filteredBundleFiles("scala-library", "scala-reflect")
    bundles ++ Array[exam.Option](felix(), equinox(), junitBundles())
  }

  def justCoreLibraryOptions: Array[exam.Option]  = {
    val bundles = filteredBundleFiles("scala-library")
    bundles ++ Array[exam.Option](felix(), equinox(), junitBundles())
  }
 
}
