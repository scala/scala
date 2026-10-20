//> using jvm 17+
package p

trait Tag[T]
trait Other

// javac records `S` in Shape.class's PermittedSubclasses
final class S extends Shape
