import scala.collection.immutable._
object Test {
  def main(args : Array[String]) : Unit = {
    var words = "a" :: "b" :: "cd" :: "de" :: "fg" :: "ef" ::
               "gh" :: "jk" :: "hj" :: "kl" :: "lm" :: "mn" :: Nil
    val q0:Set[String] =
      new HashSet[String]() ++ words
    val q1 = q0.filter(w => false)
    val q2 = q1.filter(w => false)
    Console.println("q0 = " + q0)
    Console.println("q1 = " + q1+" "+q1.size)
    Console.println("q2 = " + q2+" "+q2.size)
 }
}
