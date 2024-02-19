package tastytest

// makes Null$.class, and Null$.tasty
class Null$ {
  def res: List[Int] = List(23)
}

// makes Nothing$.class, and Nothing$.tasty
class Nothing$ {
  def res: List[Int] = List(23)

}

// makes $$.class, $.class, and $.tasty
object $ {
  def res: List[Int] = List(23)
}

// makes StandaloneObject$.class, StandaloneObject.class, and StandaloneObject.tasty
object StandaloneObject {
  def res: List[Int] = List(23)
}
