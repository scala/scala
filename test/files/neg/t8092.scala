
trait S {
  def k = f"${0x1234}%.2x"  // DNC
  //FlueTest.scala:15: Bad format: java.util.IllegalFormatPrecisionException: 2
}
