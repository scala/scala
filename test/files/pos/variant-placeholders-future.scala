//> using options -Xsource:3
//
object Test {
  type `-_` = Int
  type `+_` = Long

  val fnMinusPlus1: -_ => +_ = (_: Int).toLong
  val fnMinusPlus2: (-_) => +_ = fnMinusPlus1
  val fnMinusPlus3: -_ => (+_) = fnMinusPlus2

  val fnTupMinusPlus2: (=> -_, -_) => +_ = (a, b) => ((a: Int) + (b: Int)).toLong
  def defMinusPlus2(byname: => -_, vararg: -_*): +_ = ((vararg.sum: Int) + (byname: -_)).toLong
  val infixMinusPlus2: -_ Either +_ = Right[-_, +_](1L)

  val optPlus: Option[+_] = Some[ + _ ](1L)  // spaces allowed
  optPlus match {
    case opt: Option[ + _ ] =>
      val opt1: + _ = opt.get
      val opt2: Long = opt1
  }

  val optMinus: Option[-_] = Some[ - _ ](1)  // spaces allowed
  optMinus match {
    case opt: Option[ - _ ] =>
      val opt1: `-_` = opt.get
      val optErr: - _ = opt.get
      val opt2: Int = opt1
  }

  locally {
    type `-_`[A] = A
    type `+_`[A] = Option[A]
    val optOpt: Option[ + _ [+_[-_[Int]]]] = Some(Some(Some(1)))
  }
}
