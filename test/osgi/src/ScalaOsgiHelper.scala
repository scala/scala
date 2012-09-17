package tools.test.osgi
 
import org.ops4j.pax.exam.CoreOptions._
import org.ops4j.pax.exam
import java.io.File

trait ScalaOsgiHelper {
  def scalaBundles: Array[exam.Option]  = {
    def bundleLocation = new File(sys.props.getOrElse("scala.bundle.dir", "build/osgi"))
    def bundleFiles = bundleLocation.listFiles filter (_.getName endsWith ".jar")
    def makeBundle(file: File): exam.Option =
      bundle(file.toURI.toASCIIString)
    val bundles = (bundleFiles map makeBundle)
    System.out.println(bundles)
    bundles ++ Array[exam.Option](felix(), equinox(), junitBundles())
  }
 
}
