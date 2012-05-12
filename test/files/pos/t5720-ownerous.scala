
/*
 * The block under qual$1 must be owned by it.
 * In the sample bug, the first default arg generates x$4,
 * the second default arg generates qual$1, hence the maximal
 * minimization.
 *
    <method> <triedcooking> def model: C.this.M = {
      val qual$1: C.this.M = scala.Option.apply[C.this.M]({
  val x$1: lang.this.String("foo") = "foo";
  val x$2: String = C.this.M.apply$default$2("foo");
  C.this.M.apply("foo")(x$2)
}).getOrElse[C.this.M]({
        val x$3: lang.this.String("bar") = "bar";
        val x$4: String = C.this.M.apply$default$2("bar");
        C.this.M.apply("bar")(x$4)
      });
      val x$5: lang.this.String("baz") = "baz";
      val x$6: String = qual$1.copy$default$2("baz");
      qual$1.copy("baz")(x$6)
    }
 */
class C {
  case class M(currentUser: String = "anon")(val message: String = "empty")
  val m = M("foo")()

  // reported
  //def model = Option(M("foo")()).getOrElse(M("bar")()).copy(currentUser = "")()

  // the bug
  def model = Option(m).getOrElse(M("bar")()).copy("baz")("empty")

  // style points for this version
  def modish = ((null: Option[M]) getOrElse new M()()).copy()("empty")

  // various simplifications are too simple
  case class N(currentUser: String = "anon")
  val n = N("fun")
  def nudel = Option(n).getOrElse(N()).copy()
}

object Test {
  def main(args: Array[String]) {
    val c = new C
    println(c.model.currentUser)
    println(c.model.message)
  }
}
/*
symbol value x$4$1 does not exist in badcopy.C.model
at scala.reflect.internal.SymbolTable.abort(SymbolTable.scala:45)
at scala.tools.nsc.Global.abort(Global.scala:202)
at scala.tools.nsc.backend.icode.GenICode$ICodePhase.liftedTree2$1(GenICode.scala:998)
at scala.tools.nsc.backend.icode.GenICode$ICodePhase.scala$tools$nsc$backend$icode$GenICode$ICodePhase$$genLoad(GenICode.scala:992)
*/

