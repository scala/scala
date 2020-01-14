package tastytest

object TestModuleInCtor extends Suite("TestModuleInCtor") {

  class CClass extends ModuleInCtor.AClass(ModuleInCtor.Module)

  test(assert(new ModuleInCtor.BClass().foo === "Module"))
  test(assert(new CClass().foo === "Module"))

}
