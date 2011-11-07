object Test
{
  val a = scala.collection.immutable.Set.empty ++ (0 to 100000)
  val b = scala.collection.immutable.Set.empty ++ (0 to 100000)
  
  def main(args: Array[String]): Unit = {
    a -- b
    a -- b
    a -- b
    a -- b
  }
}
