//> using options -Werror
class E
class SE extends Serializable

object Test {
  val e = new E
  if (e == "") ??? // warn about comparing unrelated types

  val se = new SE
  if (se == "") ??? // types are still unrelated
}
