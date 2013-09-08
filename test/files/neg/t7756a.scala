object Test {
  def test: Unit = {
    trait TA[X <: CharSequence]
    0 match {
      case _ =>
        // the bounds violation isn't reported. RefChecks seems to be too broadly disabled under virtpatmat: see 65340ed4ad2e
        locally(null: TA[Object])
        ()
    }
  }
}
