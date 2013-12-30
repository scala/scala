object Test extends App {
  DynMacro.foo(1, 2)         // prints "foo(1, 2)"
  DynMacro.foo(3).bar(4, 5)  // prints "bar(4, 5)", then "foo(3)"
  DynMacro(6).bar(7)         // prints "bar(7)", then "apply(6)"
  DynMacro.foo(8)(9)         // Fails!
}