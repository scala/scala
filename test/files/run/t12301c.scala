
trait T {
  private final val HASH_SIZE  = 0x8000
}
class C {
  private final val HASH_SIZE  = 0x8000
}
object Test extends App {
  assert(new C().toString != null)
  assert(new T {}.toString != null)
}
