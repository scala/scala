package tastytest

/** suspended because the signature for BClass.<init> references ModuleInCtor.Module.type before it is in the mirror */
object TestModuleInCtor extends Suite("TestModuleInCtor") {

  // class CClass extends ModuleInCtor.AClass(ModuleInCtor.Module) // suspended because the type arguments for AClass.<init> are ModuleInCtor.Module.type

  test(assert(new ModuleInCtor.BClass().foo === "Module"))
  // test(assert(new CClass().foo === "Module"))

}
