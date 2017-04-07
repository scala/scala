trait AnnotationTest {
  @BooleanAnnotation(Constants.BooleanTrue)
  @ByteAnnotation(Constants.Byte)
  @CharAnnotation(Constants.Char)
  @ShortAnnotation(Constants.Short)
  @IntAnnotation(Constants.Int)
  @LongAnnotation(Constants.Long)
  @FloatAnnotation(Constants.Float)
  @DoubleAnnotation(Constants.Double)
  @StringAnnotation(Constants.String)
  def test1: Unit

  @BooleanAnnotation(Constants.InvertedBoolean)
  @ByteAnnotation(Constants.NegativeByte)
  @ShortAnnotation(Constants.NegativeShort)
  @IntAnnotation(Constants.NegativeInt)
  @LongAnnotation(Constants.NegativeLong)
  @FloatAnnotation(Constants.NegativeFloat)
  @DoubleAnnotation(Constants.NegativeDouble)
  @StringAnnotation(Constants.NegativeString)
  def test2: Unit

  @BooleanAnnotation(Constants.BooleanFalse)
  @ByteAnnotation(Constants.LiteralCharAsByte)
  @CharAnnotation(Constants.LiteralChar)
  @ShortAnnotation(Constants.LiteralCharAsShort)
  @IntAnnotation(Constants.LiteralCharAsInt)
  @LongAnnotation(Constants.LiteralCharAsLong)
  def test3: Unit

  @LongAnnotation(Constants.LiteralIntAsLong)
  def test4: Unit
}
