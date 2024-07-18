//> using options -Werror -Wperformance
//

import util.Try

trait Test {

  /*
   * Inspired by
   * https://stackoverflow.com/questions/50297478/why-scala-allows-return-from-recoverwith
   * "I actually saw the post Don't Use Return in Scala long ago never paid attention."
   */
  def result: Option[Int] = Try(0/0).recoverWith { case _ => return Some(-1) }.toOption
}
