object CompilerCrash {
  def main(args: Array[String]) {
    args match {
      case Array("a", a @ _*) => { } // The code compiles fine if this line is commented out or "@ _*" is deleted or this line is swapped for the next line
      case Array("b") => { } // The code compiles fine if this line is commented out 
      case Array("c", c) => {
        0 // The code compiles fine if this line is commented out
      }
    }
  }
}
