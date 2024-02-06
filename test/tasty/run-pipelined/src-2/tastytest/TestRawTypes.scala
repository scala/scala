package tastytest

import lib.RawTypes

object TestRawTypes extends scala.App {
  val rt: RawTypes = new RawTypes()
  val c: RawTypes#C[String] = new rt.C[String]()

  val cd_ii: RawTypes#C[String]#D[String] = new c.D[String]()
  val cde_iii: RawTypes#C[String]#D[String]#E[String] = new cd_ii.E[String]()

  lazy val cd_is = new rt.C.DStatic[String]() // lazy because this fails at runtime even when reading from a classfile

  val cd_s: RawTypes.CStatic[String] = new RawTypes.CStatic[String]()
  val cd_si: RawTypes.CStatic[String]#D[String] = new cd_s.D[String]()

  val cd_ss: RawTypes.CStatic.DStatic[String] = new RawTypes.CStatic.DStatic[String]()

  // RawTypes.mii_Raw_Raw(cd_ii) // fails due to wildcards, see neg-pipelined for error message
  // RawTypes.miii_Raw_Raw_Raw(cde_iii) // fails due to wildcards, see neg-pipelined for error message

  RawTypes.mss_Raw_Raw(cd_ss)
  def foo1 = RawTypes.mis_Raw_Raw(cd_is) // lazy because this fails at runtime even when reading from a classfile

  RawTypes.mss_Raw_Gen(cd_ss)
  def foo2 = RawTypes.mis_Raw_Gen(cd_is) // lazy because this fails at runtime even when reading from a classfile

  RawTypes.mii_Gen_Gen(cd_ii)
  RawTypes.msi_Gen_Gen(cd_si)
}
