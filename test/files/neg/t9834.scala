
object x { def apply() = 42 ; def update(i: Int) = () }

trait Test {
  x() += "42"
}
