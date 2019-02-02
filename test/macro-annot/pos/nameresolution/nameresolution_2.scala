@identity1 class C1
@pkg.identity2 class C2
@pkg.Module3.identity3 class C3
@Module4.identity4 class C4

object Test extends App {
  import pkg._
  import Module3._
  import Module4._
  @identity1 class C1
  @identity2 class C2
  @identity3 class C3
  @identity4 class C4

}

package pkg {
  // @identity1 class C1
  @identity2 class C2
  @Module3.identity3 class C3
  // @Module4.identity4 class C4
}
