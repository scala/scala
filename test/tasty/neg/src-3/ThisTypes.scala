package tastytest

object ThisTypes {

  abstract class Wrap3 {
    class Base
    final type Res = Option[Base]
    def doTest: Res
  }

  class Sub3 extends Wrap3 {
    def doTest: Res = Some(new Base())
  }

}
