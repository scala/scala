trait Bar {
  class Config {}
  var config: Config; // aha, traits can have variables?
}

object Foo extends Bar {

  class FooConfig extends Config;
  var config: Config = new FooConfig() // or not

}
