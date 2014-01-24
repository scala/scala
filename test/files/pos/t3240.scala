class A {
  val foo = new {
  type t
  def apply(a: Option[t], defVal: Any) = {
       a.getOrElse(defVal).asInstanceOf[t]
     }
  }
}