package scala.tools.partest

import scala.tools.nsc.doc.Universe

/** A class for testing scaladoc model generation on java sources. */
abstract class ScaladocJavaModelTest extends ScaladocModelTest {

  // overridden to pass explicit files to newDocFactory.makeUniverse (rather than code strings)
  // since the .java file extension is required
  override def model: Option[Universe] = {
    val path = resourcePath + "/" + resourceFile
    newDocFactory.makeUniverse(Left(List(path)))
  }

}
