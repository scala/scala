trait Generic[g[x]] {
  def unit: g[Unit]
}

trait Rep[t] {
  def rep[m[x]](implicit gen: Generic[m]): m[t]
}

// testing that the return type is also transformed when checking overriding
// + that substitution (of types&symbols) preserves isHigherKinded when replacing a higher-kinded type with another one
object foo extends Rep[Unit] {
  def rep[g[x]](implicit gen: Generic[g]): g[Unit]= gen.unit
}
