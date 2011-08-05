// no unchecked warnings
case class Bippy[T, -U, +V](x: T, z: V) { }
