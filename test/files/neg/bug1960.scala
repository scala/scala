object ClassFormatErrorExample extends App { new Aclass(1) }

trait TBase { var p:Int = 0; def f(p1: Int) {} }

class Aclass (p: Int) extends TBase { def g{ f(p) } }
