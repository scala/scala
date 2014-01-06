class SI_4715 {
  type :+:[X,Y] = Map[X,Y]
  val withType: Int :+: Double = sys.error("")

  trait :-:[X,Y]
  val withTrait: Int :-: Double = sys.error("")
}
