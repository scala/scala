//> using jvm 16+
package tastytest

import lib.RawTypes2

/** Test definitions that only compile in Java 16+ */
object TestRawTypes2 extends scala.App {
  val rt: RawTypes2 = new RawTypes2()

  lazy val cd_is = new rt.C.DStatic[String]() // lazy because this fails at runtime even when reading from a classfile

  def foo1 = RawTypes2.mis_Raw_Raw(cd_is) // lazy because this fails at runtime even when reading from a classfile

  def foo2 = RawTypes2.mis_Raw_Gen(cd_is) // lazy because this fails at runtime even when reading from a classfile

}
