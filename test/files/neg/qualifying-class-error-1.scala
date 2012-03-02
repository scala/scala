class A(val i:Int)
class B extends A(this.getClass.getName.length)
