object This {
     trait A {
         def foo(): unit;
     }
     class C: A {
         def bar() = foo();
     }
}
