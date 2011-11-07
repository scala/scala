trait C {
  def f: int
}

object Test {
  type T = C {
    def f: int
    def g: String
  }

  val x: T = new C {
    def f = 1
    def g = "hello"
  }

  val y = new C {
    def f = 2
    def g = " world"
  } 
    
  val z: T = y
  
  Console.println(x.f+z.f+", expected = 3")
  Console.println(x.g+z.g+", expected = hello world")
}
