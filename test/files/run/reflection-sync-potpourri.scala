import scala.reflect.runtime.universe._

// this test checks that under heavily multithreaded conditions:
// 1) scala.reflect.runtime.universe, its rootMirror and definitions are initialized correctly
// 2) symbols are correctly materialized into PackageScopes (no dupes)
// 3) unpickling works okay even we unpickle the same symbol a lot of times

object Test extends App {
  def foo[T: TypeTag](x: T) = typeOf[T].toString
  val n = 1000
  val rng = new scala.util.Random()
  val types = List(
    () => typeOf[java.lang.reflect.Method],
    () => typeOf[java.lang.annotation.Annotation],
    () => typeOf[scala.io.BufferedSource],
    () => typeOf[scala.io.Codec])
  val perms = types.permutations.toList
  def force(lazytpe: () => Type): String = {
    lazytpe().typeSymbol.typeSignature
    lazytpe().toString
  }
  val diceRolls = List.fill(n)(rng.nextInt(perms.length))
  val threads = (1 to n) map (i => new Thread(s"Reflector-$i") {
    override def run(): Unit = {
      val s1 = foo("42")
      val s2 = perms(diceRolls(i - 1)).map(x => force(x)).sorted.mkString(", ")
      println(s"s1 = $s1, s2 = $s2")
    }
  })
  threads foreach (_.start)
}