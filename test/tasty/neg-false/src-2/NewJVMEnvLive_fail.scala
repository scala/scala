package tastytest

import printing._

object NewJVMEnv {

  final class TestClass {
    override def hashCode(): Int = 123
  }

  val JVMEnvLive: JVMEnvLive = new JVMEnvLive {}
  assert(
    (JVMEnvLive)
      .evalInPrinter(Printer.ObjectToString)(new TestClass) == "tastytest.TestPrinter$TestClass@123"
  )

}
