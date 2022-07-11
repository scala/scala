package tastytest

object TestArrayAnnot {
  def test = {
    forceAnnots[
      Tagged, 
      SuppressWarnings,
      "new SuppressWarnings(Array.type.apply[String]((Array[String]{\"xyz\", \"foo\"}: String*))(reflect#ClassTag.type.apply[String](classOf[java.lang.String])))"
    ]
  }
}
