import StaticReflect._

object Test extends App {
  //println(method[List[Int]]("distinct"))
  println(method[List[Int]]("map"))
  //val $u: scala.reflect.runtime.universe.type = scala.reflect.runtime.universe;
  //val $m: $u.Mirror = scala.reflect.runtime.universe.rootMirror;
  //import $u._, $m._, Flag._
  //val tpe = {
  //  val symdef$B2 = build.newNestedSymbol(build.selectTerm(staticClass("scala.collection.TraversableLike"), "map"), newTypeName("B"), NoPosition, DEFERRED | PARAM, false);
  //  val symdef$That2 = build.newNestedSymbol(build.selectTerm(staticClass("scala.collection.TraversableLike"), "map"), newTypeName("That"), NoPosition, DEFERRED | PARAM, false);
  //  val symdef$f2 = build.newNestedSymbol(build.selectTerm(staticClass("scala.collection.TraversableLike"), "map"), newTermName("f"), NoPosition, PARAM, false);
  //  val symdef$bf2 = build.newNestedSymbol(build.selectTerm(staticClass("scala.collection.TraversableLike"), "map"), newTermName("bf"), NoPosition, IMPLICIT | PARAM, false);
  //  build.setTypeSignature(symdef$B2, TypeBounds(staticClass("scala.Nothing").asType.toTypeConstructor, staticClass("scala.Any").asType.toTypeConstructor));
  //  build.setTypeSignature(symdef$That2, TypeBounds(staticClass("scala.Nothing").asType.toTypeConstructor, staticClass("scala.Any").asType.toTypeConstructor));
  //  build.setTypeSignature(symdef$f2, TypeRef(ThisType(staticPackage("scala").asModule.moduleClass), staticClass("scala.Function1"), List(staticClass("scala.Int").asType.toTypeConstructor, TypeRef(NoPrefix, symdef$B2, List()))));
  //  build.setTypeSignature(symdef$bf2, TypeRef(ThisType(staticPackage("scala.collection.generic").asModule.moduleClass), staticClass("scala.collection.generic.CanBuildFrom"), List(TypeRef(ThisType(staticPackage("scala.collection.immutable").asModule.moduleClass), staticClass("scala.collection.immutable.List"), List(staticClass("scala.Int").asType.toTypeConstructor)), TypeRef(NoPrefix, symdef$B2, List()), TypeRef(NoPrefix, symdef$That2, List()))));
  //  PolyType(List(symdef$B2, symdef$That2), MethodType(List(symdef$f2), MethodType(List(symdef$bf2), TypeRef(NoPrefix, symdef$That2, List()))))
  //}
  //println(tpe)
}