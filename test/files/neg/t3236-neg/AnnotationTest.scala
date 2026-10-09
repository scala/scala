trait AnnotationTest {
  @IntAnnotation(Constants.ConstInt) // ok
  @IntAnnotation(Constants.ConstIdent) // ok
  @IntAnnotation(Constants.ConstSelect) // ok
  @IntAnnotation(Constants.NegatedInt)
  @IntAnnotation(Constants.ConstOpExpr1) // ok
  @IntAnnotation(Constants.ConstOpExpr2) // ok
  @BooleanAnnotation(Constants.ConstOpExpr3) // ok
  @IntAnnotation(Constants.ConstOpExpr4) // ok
  @IntAnnotation(Constants.NonFinalConst)
  @IntAnnotation(Constants.NonStaticConst)
  @IntAnnotation(Constants.NonConst)
  @ShortAnnotation(Constants.ConstCastExpr) // ok
  @StringAnnotation(Constants.ConstString) // ok
  @StringAnnotation(Constants.StringAdd) // ok
  def test: Unit
}
