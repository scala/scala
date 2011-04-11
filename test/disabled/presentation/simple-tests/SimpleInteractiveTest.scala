import scala.tools.nsc.interactive.tests._

/** Simple test that shows how to use the InteractiveTest class. It uses the
 *  inherited runTest method that runs completion and typedAt tests on all
 *  sources found under src/
 */
object Test extends InteractiveTest {
  override val runRandomTests = false
//  settings.YpresentationDebug.value = true
//  override val synchronousRequests = false
}
