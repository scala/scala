abstract class Obj { type S }
class ObjImpl extends Obj { type S = String }

abstract class A {
  type MyObj <: Obj
  type S = MyObj#S

  val any:  Any = 0
  val some: S = any   // compiles => type X is set to scala.Any
}

class B extends A {
  type MyObj = ObjImpl 
  val myString:   S = "hello"
  val realString: String = myString   // error: type missmatch
}
