// checks that error doesn't crash the compiler
// (due to isFunctionType normalizing Type1 to a function type, 
//  but then the code that used that test not using the normalized type for further operations)
class Test {
  type Type1 = () => Unit
  
  def call(p: Int)(f: => Type1) = {
    f()
  }
  
  def run = {
    call(0,() => System.out.println("here we are"))
  }
}
