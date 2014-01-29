object Whatever {
  override def equals(x: Any) = true
}

class Test {
  // when left to its own devices, and not under -Xfuture, the return type is Whatever.type
  def matchWhateverCCE(x: Any) = x match { case n @ Whatever => n }
}