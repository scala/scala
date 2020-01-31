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
     for(bundle <- allBundleFiles; if names exists (bundle.getName contains _))
     yield makeBundle(bundle)

  private def makeBundle(file: File): exam.Option =
      bundle(file.toURI.toASCIIString)

  def standardOptions: Array[exam.Option]  = {
    val bundles = (allBundleFiles map makeBundle)
    bundles ++ Array[exam.Option](junitBundles())
    // to change the local repo used (for some operations, but not all -- which is why I didn't bother):
    // systemProperty("org.ops4j.pax.url.mvn.localRepository").value(sys.props("maven.repo.local")))
  }

  def justReflectionOptions: Array[exam.Option]  = {
    val bundles = filteredBundleFiles("scala-library", "scala-reflect")
    bundles ++ Array[exam.Option](junitBundles())
  }

  def justCoreLibraryOptions: Array[exam.Option]  = {
    val bundles = filteredBundleFiles("scala-library")
    bundles ++ Array[exam.Option](junitBundles())
  }

}
