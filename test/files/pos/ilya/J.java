package test;

class Foo {
}

class Boo<T extends Foo>{}

class Bar<BooT extends Boo<FooT>, FooT extends Foo>{
 private final int myInt;

 public Bar(int i) {
   myInt = i;
 }
}
