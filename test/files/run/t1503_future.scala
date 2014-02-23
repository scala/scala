object Whatever {
  override def equals(x: Any) = true
}

object Test extends App {
  // this should make it abundantly clear Any is the best return type we can guarantee
  def matchWhatever(x: Any): Any = x match { case n @ Whatever => n }
  // when left to its own devices, and not under -Xfuture, the return type is Whatever.type
  def matchWhateverCCE(x: Any) = x match { case n @ Whatever => n }

  // just to exercise it a bit
  assert(matchWhatever(1) == 1)
  assert(matchWhatever("1") == "1")

  assert(matchWhateverCCE(1) == 1)
  assert(matchWhateverCCE("1") == "1")
}