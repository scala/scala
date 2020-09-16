public class Test {
  public static void main(String... args) {
    scala.Predef.println(Lib.f(new AB() {}));
    scala.Predef.println(Lib.f(new AA() {}));
  }
}
