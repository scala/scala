object Test extends App {
  trait Assoc[K] { type V ; val v: V }

  def mkAssoc[K, V0](k: K, v0: V0): Assoc[k.type] { type V = V0 } = new Assoc[k.type] {type V = V0 ; val v = v0}
  def lookup[K](k: K)(implicit a: Assoc[k.type]): a.V = a.v

  implicit def firstAssoc = mkAssoc(1, "Panda!")
  implicit def secondAssoc = mkAssoc(2, "Kitty!")

  implicit def ageAssoc = mkAssoc("Age", 3)
  implicit def nmAssoc = mkAssoc("Name", "Jane")

  assert(lookup(1) == "Panda!")
  assert(lookup(2) == "Kitty!")
  assert(lookup("Age") == 3)
  assert(lookup("Name") == "Jane")
}
