object Test extends App {
  import reflect.runtime.universe._

  def transformer(rec_pf: (Tree => Tree) => PartialFunction[Tree, Tree]) =
    new Transformer {
      val pf: PartialFunction[Tree, Tree] = rec_pf(transform) // on Scala 2.12, this raises: java.lang.NoSuchMethodError: squid.utils.meta.UniverseHelpers$$anon$5.pf()Lscala/PartialFunction
      // ^ solved if we make it a def
      override def transform(x: Tree) = pf.applyOrElse(x, super.transform)
    } transform _

  val fn = transformer(_ => { case q"0" => q"1" })
  println(fn(q"Some(0).get"))
}