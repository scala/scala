import java.lang.reflect.Modifier.isFinal

object Test {
  object InnerObject
  final case class InnerCase()
  final class InnerNonCase()

  def main(args: Array[String]) {
    def checkFinal(clazz: Class[_]) =
      println(s"${clazz} isFinal = ${isFinal(clazz.getModifiers())}")

    checkFinal(InnerObject.getClass)
    checkFinal(classOf[InnerCase])
    checkFinal(classOf[InnerNonCase])

    checkFinal(TopLevelObject.getClass)
    checkFinal(classOf[TopLevelCase])
    checkFinal(classOf[TopLevelNonCase])
  }
}

object TopLevelObject
final case class TopLevelCase()
final case class TopLevelNonCase()
