class Foo(val bar : () => String);

class IP extends {
  val baz = "bar";
} with Foo(() => baz);

object Test extends Application{
  (new IP).bar();
}
