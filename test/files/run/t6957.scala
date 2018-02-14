object Test {
  def main(args: Array[String]): Unit = {
    class Foo
    class Parent(f:Foo)
    class Child extends Parent({val x=new Foo{}; x})
    new Child
  }
}
