trait AnnotationTest {
  @IntAnnotation(Constants.ConstInt) // ok
  @IntAnnotation(Constants.ConstIdent)
  @IntAnnotation(Constants.ConstSelect)
  @IntAnnotation(Constants.NegatedInt) // ok
  @IntAnnotation(Constants.ConstOpExpr1)
  @IntAnnotation(Constants.ConstOpExpr2)
  @BooleanAnnotation(Constants.ConstOpExpr3)
  @IntAnnotation(Constants.ConstOpExpr4)
  @IntAnnotation(Constants.NonFinalConst)
  @IntAnnotation(Constants.NonStaticConst)
  @IntAnnotation(Constants.NonConst)
  @ShortAnnotation(Constants.ConstCastExpr)
  @StringAnnotation(Constants.ConstString) // ok
  @StringAnnotation(Constants.StringAdd)
  def test: Unit
}