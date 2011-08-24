abstract class Module {}

abstract class T {
  type moduleType <: Module
  val module: moduleType
}

abstract class Base {
  type mType = Module
  type tType = T { type moduleType <: mType }
}

abstract class Derived extends Base {
  def f(inputs: List[tType]): Unit = {
    for (t <- inputs; m = t.module) { }
  }
}
