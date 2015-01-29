class Unchecked[C] {
  def nowarn[T] = (null: Any) match { case _: Some[T]      => } // warn (did not warn due to SI-8597)

  // These warned before.
  def warn1[T]  = (null: Any) match { case _: T            => } // warn
  def warn2     = (null: Any) match { case _: Some[String] => } // warn
                  (null: Any) match { case _: Some[C]      => } // warn

  // These must remain without warnings. These are excerpts from
  // related tests that are more exhauative.
  class C; class D extends C
  def okay      = (List(new D) : Seq[D]) match { case _: List[C] => case _ => } // nowarn
  class B2[A, B]
  class A2[X] extends B2[X, String]
  def okay2(x: A2[Int]) = x match { case _: B2[Int, _] => true } // nowarn
  def okay3(x: A2[Int]) = x match { case _: B2[Int, typeVar] => true } // nowarn

  def warnArray[T] = (null: Any) match { case _: Array[T] => } // warn (did not warn due to SI-8597)
  def nowarnArrayC   = (null: Any) match { case _: Array[C] => } // nowarn

  def nowarnArrayTypeVar[T] = (null: Any) match { case _: Array[t] => } // nowarn

  def noWarnArrayErasure1 = (null: Any) match {case Some(_: Array[String]) => } // nowarn
  def noWarnArrayErasure2 = (null: Any) match {case Some(_: Array[List[_]]) => } // nowarn
  def noWarnArrayErasure3 = (null: Any) match {case Some(_: Array[Array[List[_]]]) => } // nowarn
  def warnArrayErasure2   = (null: Any) match {case Some(_: Array[Array[List[String]]]) => } // warn
}
