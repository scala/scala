object Test extends App {
  def noisyIdentity(x: AnyRef): x.type = {
    println("got "+x)
    x
  }

  noisyIdentity("test")
  noisyIdentity(new java.lang.String("test"))

  trait Assoc[K] { type V; val value: V }
  def mkAssoc[V0](k: String, v: V0): Assoc[k.type] { type V = V0 } =
    new Assoc[k.type] { type V = V0 ; val value = v }

  def lookup(k: String)(implicit assoc: Assoc[k.type]): assoc.V = assoc.value

  implicit def nameAssoc = mkAssoc("Name", "Mary")
  implicit def ageAssoc = mkAssoc("Age", 23)

  assert((lookup("Name"): String) == "Mary")
  assert((lookup("Age"): Int) == 23)
}
