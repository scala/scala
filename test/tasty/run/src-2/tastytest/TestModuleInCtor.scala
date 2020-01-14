package tastytest

object TestModuleInCtor extends Suite("TestModuleInCtor") {

  // class CClass extends ModuleInCtor.AClass(ModuleInCtor.Module) // suspended because the type arguments for AClass.<init> are not the singleton type

  test(assert(new ModuleInCtor.BClass().foo === "Module"))
  // test(assert(new CClass().foo === "Module"))

}
