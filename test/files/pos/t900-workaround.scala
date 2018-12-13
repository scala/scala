
trait T {
  val xs = Seq(Some(1), None)

  def f() = for (x @ (_x: Some[_]) <- xs) yield x

  def g() = for (x @ (_: Some[_]) <- xs) yield x
}

/*
 * The second form was not translated to a partial function:
  val res1 = xs.withFilter(((check$ifrefutable$1) => check$ifrefutable$1: @scala.unchecked match {
    case (x @ (_: Some[(_ @ <empty>)])) => true
    case _ => false
  })).map(((x: Some[(_ @ <empty>)]) => x))
 *
 *        error: type mismatch;
 *         found   : Some[_] => Some[Any]
 *         required: Option[Int] => ?
 */
