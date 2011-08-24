// that compiles
class Test(myValue:String) { println(myValue) }

// that compiles too
trait Other { val otherValue = "" }
class Test2(myValue:String) { self:Other => println(otherValue) }

// that does not compile saying that myValue is not found
class Test3(myValue:String) { self:Other => println(myValue) }
