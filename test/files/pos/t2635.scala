abstract class Base

object Test
{
  def run(c: Class[_ <: Base]): Unit = {
  }

  def main(args: Array[String]): Unit =
  {
    val sc: Option[Class[_ <: Base]] = Some(classOf[Base])
    sc match {
      case Some(c) => run(c)
      case None =>
    }
  }
}
