package tastytest

import printing._

object TestJVMEnvLive extends Suite("TestJVMEnvLive") {

  final class TestClass {
    override def hashCode(): Int = 123
  }

  test {
    val JVMEnvLive: JVMEnvLive = new JVMEnvLive {}
    val str = JVMEnvLive.evalInPrinter(Printer.ObjectToString)(new TestClass)
    assert(str === s"${reflect.classTag[TestClass]}@123")
  }

}
