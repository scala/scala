trait C {
  def f: Int
}

object Test {
  type T = C {
    def f: Int
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
  
  def main(args: Array[String]): Unit = {
    assert(x.f+z.f == 3)
    assert(x.g+z.g == "hello world")
  }
}
