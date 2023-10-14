
package p;
public sealed interface X {
  public default int x() { return 27; }
}
final class Y implements X { }
final class O {
  final static class Z implements X { }
  final static class Inner {
    final static class Oz implements X { }
  }
}
