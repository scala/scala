object Test extends App {
  
  trait SpecialException {}

  try {
    throw new Exception
  } catch {
    case e : SpecialException => {
      println("matched SpecialException: "+e)
      assume(e.isInstanceOf[SpecialException])
    }
    case e : Exception => {
      assume(e.isInstanceOf[Exception])
    }
  }
}
