import org.junit._
import org.junit.runner._
import org.junit.runners._

@identity1 class C1
@pkg.identity2 class C2
@pkg.Module3.identity3 class C3
@Module4.identity4 class C4

@RunWith(classOf[JUnit4])
class NameResolution {
  import pkg._
  import Module3._
  import Module4._
  @identity1 class C1
  @identity2 class C2
  @identity3 class C3
  @identity4 class C4

  @Test
  def verifiedAtCompileTime: Unit = {
  }
}

package pkg {
  // @identity1 class C1
  @identity2 class C2
  @Module3.identity3 class C3
  // @Module4.identity4 class C4
}