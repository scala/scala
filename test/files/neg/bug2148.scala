class A { 
  var i = 0
  trait A1 extends A { 
    i += 1
  }
}

object Bob {
  val b = new A with A#A1
}