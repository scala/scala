import java.lang.annotation._

@Deprecated @Retention(RetentionPolicy.RUNTIME) class Foo

object Test extends App {
  classOf[Foo].getAnnotation(classOf[Deprecated])

  assert(classOf[Foo].getAnnotation(classOf[Retention]).value() == RetentionPolicy.RUNTIME)

  import reflect.runtime.universe._

  val List(d, r) = symbolOf[Foo].annotations

  d.tree match {
    case Apply(Select(New(tpt), _), Nil) =>
      assert (tpt.tpe.typeSymbol == symbolOf[Deprecated], tpt.tpe.typeSymbol)
  }

  val RetentionPolicy_RUNTIME = symbolOf[RetentionPolicy].companion.info.decl(TermName("RUNTIME"))
  r.tree match {
    case Apply(Select(New(tpt), _), List(NamedArg(Ident(TermName("value")), Literal(Constant(RetentionPolicy_RUNTIME))))) =>
      assert (tpt.tpe.typeSymbol == symbolOf[Retention], tpt.tpe.typeSymbol)
  }

}
