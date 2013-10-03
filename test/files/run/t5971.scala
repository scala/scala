




/** When using `AbstractTransformed` abstract inner class in views in order
 *  to force generating bridges, one must take care to push the corresponding
 *  collection trait (such as `Iterable` or `Seq`) as far as possible to the
 *  left in the linearization order -- otherwise, overridden methods from these
 *  traits can override the already overridden methods in view. This was the
 *  case with `takeWhile`.
 *  Mind blowing, I know.
 */
object Test {

  def main(args: Array[String]) {
    println("bar".view.reverse.filter(_ > 'a').mkString(","))
    println("bar".view.reverse.take(1).mkString(","))
    println("bar".view.reverse.dropWhile(_ > 'a').mkString(","))
    println("bar".view.reverse.takeWhile(_ => true).mkString(","))
  }

}
