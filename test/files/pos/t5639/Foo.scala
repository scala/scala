package pack.age

class Baz 

object Implicits  {
  implicit def Baz(n: Int): Baz = new Baz
}
