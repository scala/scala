// scala/bug#10361
final class AnyValNothing(val self: Nothing) extends AnyVal
final class AnyValNull   (val self: Null   ) extends AnyVal
// scala/bug#9240
final class AnyValUnit   (val self: Unit   ) extends AnyVal

object Test extends App {
  def avn = new AnyValNull(null)
  assert(avn == avn)
  /*this throws NPE right now b/c scala/bug#7396 */
  //assert(avn.hashCode() == 0)

  def avu = new AnyValUnit(())
  assert((avu.self: Any).equals(()))
  assert(avu equals avu)
  assert((avu: Any).## == 0)

  /* can't really test AnyValNothing, but summon it so it gets verified */
  AnyValNothing.toString
}