object Test {
  type `-_` = Int
  type `+_` = Long

  val fnMinusPlus1: -_ => +_ = (_: Int).toLong // error -_/+_ won't parse without -Xsource:3
  val fnMinusPlus2: (-_) => +_ = fnMinusPlus1  // error -_/+_ won't parse without -Xsource:3
  val fnMinusPlus3: -_ => (+_) = fnMinusPlus2  // error -_/+_ won't parse without -Xsource:3
}
