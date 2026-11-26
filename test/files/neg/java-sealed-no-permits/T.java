// a separate file, so that S does not collect these classes
public sealed class T {
  static non-sealed class A extends T {}
  static non-sealed class B extends T {}
}
