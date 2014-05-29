class Unchecked[C] {
  def nowarn[T] = (null: Any) match { case _: Some[T]      => }  // SI-8597 no unchecked warning here

  // These warned before.
  def warn1[T]  = (null: Any) match { case _: T            => }
  def warn2     = (null: Any) match { case _: Some[String] => }
                  (null: Any) match { case _: Some[C]      => }

  // These must remain without warnings. These are excerpts from
  // related tests that are more exhauative.
  class C; class D extends C
  def okay      = (List(new D) : Seq[D]) match { case _: List[C] => case _ => }
  class B2[A, B]
  class A2[X] extends B2[X, String]
  def okay2(x: A2[Int]) = x match { case _: B2[Int, _] => true }
  def okay3(x: A2[Int]) = x match { case _: B2[Int, typeVar] => true }

  def nowarnArray[T] = (null: Any) match { case _: Array[T] => } // did not warn
  def nowarnArrayC   = (null: Any) match { case _: Array[C] => } // did not warn

  def nowarnArrayTypeVar[T] = (null: Any) match { case _: Array[t] => } // should not warn

  def noWarnArrayErasure1 = (null: Any) match {case Some(_: Array[String]) => }
  def noWarnArrayErasure2 = (null: Any) match {case Some(_: Array[List[_]]) => }
  def noWarnArrayErasure3 = (null: Any) match {case Some(_: Array[Array[List[_]]]) => }
  def warnArrayErasure2   = (null: Any) match {case Some(_: Array[Array[List[String]]]) => }
}
