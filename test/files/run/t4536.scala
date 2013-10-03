





object dynamicObject extends Dynamic {
  def applyDynamic(m: String)() = println("obj: " + m);
  this.foo()
}


class dynamicClass extends Dynamic {
  def applyDynamic(m: String)() = println("cls: " + m);
  this.bar()
  dynamicObject.bar()
}


abstract class dynamicAbstractClass extends Dynamic {
  def applyDynamic(m: String)(args: Any*): Unit
  this.pili(1, new dynamicClass, "hello");
}


trait dynamicTrait extends Dynamic {
  def applyDynamic(m: String)(args: Any*) = println("trait: " + m);
  def two = 2
  this.mili(1,2,3)
  two
}


object dynamicMixin extends dynamicAbstractClass with dynamicTrait {
  this.foo(None)
}


object Test {

  def main(args: Array[String]) {
    val cls = new dynamicClass
    dynamicMixin
  }

}
