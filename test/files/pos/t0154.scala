package test
trait MyMatchers  {
  val StringMatch = new AnyRef {}
    trait Something {
      (null : AnyRef) match {
        case (StringMatch) =>
        case _ =>
      }
   }
}
