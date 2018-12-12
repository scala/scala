trait Animal

class Dog

case class Owner(animals: Set[Animal])

// uncomment this to make the compile detect the error
//object Owner {
//  def apply(age: Int): Unit = Unit
//}

object Main {
  def main(args : Array[String]): Unit = {
    val a = new Dog with Animal

    // if the following two lines are used, the error is detected properly
    //val set = Set(a)
    //Owner(set)

    // expected error: Type mismatch, expected: Set[Animal], actual: Set[Dog with Animal]
    Owner(Set(a))
  }
}