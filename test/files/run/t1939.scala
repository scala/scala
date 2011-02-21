class Module {}

abstract class T {
  type moduleType <: Module
  def module: moduleType
}

final class T1(val module: Module) extends T {
  type moduleType = Module
}

final class T2(_module: Module) extends T {
  type moduleType = Module

  def module = _module
}

object Test extends App {

  type mType = Module

  type tType = T { type moduleType <: mType }
  // type tType = T { type moduleType <: Module } // runs successfully
  // type tType = T // runs successfully

  def f(ts: List[tType]): Unit = {

    for (t <- ts; m = t.module) {}
    ts.map(t => t.module).foreach { _ => () }
    // ts.map(t => (t : T).module).foreach { _ => () } // runs successfully
  }

  f(new T1(new Module) :: new T2(new Module) :: Nil)
}
