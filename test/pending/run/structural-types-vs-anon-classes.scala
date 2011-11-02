object Test {
  class Arm
  class Leg
  class Tail
  class Monkey(arms: List[Arm], legs :List[Leg], tail: Tail)
  
  def makeAwesomeMonkey(arms: List[Arm], legs: List[Leg], tail: Tail) = {
    object m extends Monkey(arms, legs, tail) {
      def beAwesome () = "I can fly! I can fly!"
    }
    m
  }
  
  def main(args: Array[String]): Unit = {
    println(makeAwesomeMonkey(Nil, Nil, new Tail) beAwesome)
  }
}
