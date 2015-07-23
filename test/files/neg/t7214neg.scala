// pattern matcher crashes here trying to synthesize an unneeded outer test.
//     no-symbol does not have an owner
// at scala.reflect.internal.SymbolTable.abort(SymbolTable.scala:49)
// at scala.tools.nsc.Global.abort(Global.scala:253)
// at scala.reflect.internal.Symbols$NoSymbol.owner(Symbols.scala:3248)
// at scala.reflect.internal.Symbols$Symbol.effectiveOwner(Symbols.scala:678)
// at scala.reflect.internal.Symbols$Symbol.isDefinedInPackage(Symbols.scala:664)
// at scala.reflect.internal.TreeGen.mkAttributedSelect(TreeGen.scala:188)
// at scala.reflect.internal.TreeGen.mkAttributedRef(TreeGen.scala:124)
// at scala.tools.nsc.ast.TreeDSL$CODE$.REF(TreeDSL.scala:308)
// at scala.tools.nsc.typechecker.PatternMatching$TreeMakers$TypeTestTreeMaker$treeCondStrategy$.outerTest(PatternMatching.scala:1209)
class Crash {
  type Alias = C#T

  val c = new C
  val t = new c.T

  // Crash via a Typed Pattern...
  (t: Any) match {
    case e: Alias =>
  }

  // ... or via a Typed Extractor Pattern.
  object Extractor {
    def unapply(a: Alias): Option[Any] = None
  }
  (t: Any) match {
    case Extractor() =>
    case _ =>
  }

  // checking that correct outer tests are applied when
  // aliases for path dependent types are involved.
  val c2 = new C
  type CdotT = c.T
  type C2dotT = c2.T

  val outerField = t.getClass.getDeclaredFields.find(_.getName contains ("outer")).get
  outerField.setAccessible(true)

  (t: Any) match {
    case _: C2dotT =>
      println(s"!!! wrong match. t.outer=${outerField.get(t)} / c2 = $c2") // this matches on 2.10.0
    case _: CdotT =>
    case _ =>
      println(s"!!! wrong match. t.outer=${outerField.get(t)} / c = $c")
  }
}

class C {
  class T
}

object Test extends App {
  new Crash
}

