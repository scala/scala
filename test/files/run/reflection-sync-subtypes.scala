import scala.reflect.runtime.universe._

object Test extends App {
  val n = 1000
  val rng = new scala.util.Random()
  val tasks = List(
    () => typeOf[List[Int]] <:< typeOf[List[T] forSome { type T }],
    () => typeOf[List[T] forSome { type T }] <:< typeOf[List[Any]],
    () => typeOf[Map[Int, Object]] <:< typeOf[Iterable[(Int, String)]],
    () => typeOf[Expr[Any] { val mirror: rootMirror.type }] <:< typeOf[Expr[List[List[List[Int]]]]{ val mirror: rootMirror.type }])
  val perms = tasks.permutations.toList
  val diceRolls = List.fill(n)(rng.nextInt(perms.length))
  val threads = (1 to n) map (i => new Thread(s"Reflector-$i") {
    override def run(): Unit = {
      val result = perms(diceRolls(i - 1)).map(_())
      assert(result.sorted == List(false, false, true, true))
    }
  })
  threads foreach (_.start)
}