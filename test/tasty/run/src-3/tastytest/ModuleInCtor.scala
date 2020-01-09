package tastytest

object ModuleInCtor {

  object Module {
    def foo: String = "Module"
  }

  class AClass(m: Module.type) {
    def foo: String = m.foo
  }


  class BClass extends AClass(Module)

}
