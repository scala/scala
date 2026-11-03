package p;

// sealed hierarchy in Java sources compiled in the same run, spanning compilation units and Scala
sealed interface Shape permits Circle, Poly, Shape.Point, S {
  record Point(int x, int y) implements Shape {}
}
