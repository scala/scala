object Test extends App {
  import scala.reflect.runtime.universe._
  //
  // x's owner is outer Test scope. Previously the quasiquote expansion
  // looked like:
  //
  //     object Test {
  //       build.withFreshTermName("doWhile")(n =>
  //         LabelDef(n, List(),
  //           Block(
  //             List({ val x = 1; x }),
  //             If(Literal(Constant(true)), Apply(Ident(n), List()), Literal(Constant(())))))
  //     }
  //
  // Here the proper owner is anonymous function, not the Test. Hence
  // symbol corruption. In new encoding this is represented as:
  //
  //     object Test {
  //       {
  //         val n = build.freshTermName("doWhile")
  //         LabelDef(n, List(),
  //           Block(
  //             List({ val x = 1; x }),
  //             If(Literal(Constant(true)), Apply(Ident(n), List()), Literal(Constant(()))))
  //       }
  //     }
  //
  // Owner stays the same and life is good again.
  //
  println(q"do ${ val x = 1; x } while(true)")
}
