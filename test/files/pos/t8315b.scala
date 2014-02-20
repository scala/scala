object Test extends Object {
  def crash: Unit = {
    val key = ""
    try map(new F(key))
    catch { case _: Throwable => }
  };
  final def map(f: F): Any = f.apply("");
};
final class F(key: String) {
  final def apply(a: Any): Any = throw new RuntimeException(key);
}
