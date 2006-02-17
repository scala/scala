object cfcrash {
  final val zero = 0
  def blah = 3 / zero  // this should not crash the compiler
}
