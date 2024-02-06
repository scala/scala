package tastytest

import lib.RawTypes

object TestRawTypes extends scala.App {
  val rt: RawTypes = new RawTypes()
  val c: RawTypes#C[String] = new rt.C[String]()

  val cd_ii: RawTypes#C[String]#D[String] = new c.D[String]()
  val cde_iii: RawTypes#C[String]#D[String]#E[String] = new cd_ii.E[String]()

  val c_s: RawTypes.CStatic[String] = new RawTypes.CStatic[String]()
  val cd_si: RawTypes.CStatic[String]#D[String] = new c_s.D[String]()

  RawTypes.mii_Raw_Raw(cd_ii) // error
  RawTypes.miii_Raw_Raw_Raw(cde_iii) // error

  RawTypes.msi_Raw_Raw(cd_si) // error
}
