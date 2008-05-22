class Foo(val bar : () => String);

class IP extends {
  val baz = "bar";
} with Foo(() => baz);

object Main extends Application{
  (new IP).bar();
}
