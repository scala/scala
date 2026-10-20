package p;

sealed interface Shape permits Circle, Poly, Shape.Point, Holder.Inner {
  record Point(int x, int y) implements Shape {}
}
