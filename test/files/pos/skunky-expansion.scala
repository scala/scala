//> using options -Werror -Wnonunit-statement
//
import scala.reflect.macros._
import scala.reflect.api.TypeCreator

abstract trait Encoder[A] extends scala.AnyRef;
object StringContextOps extends scala.AnyRef {
  class StringOpsMacros(c: scala.reflect.macros.whitebox.Context) extends scala.AnyRef {
    def sql_impl(argSeq: StringOpsMacros.this.c.universe.Tree*): AnyRef = {
      val EncoderType: StringOpsMacros.this.c.universe.Type = StringOpsMacros.this.c.universe.typeOf[Encoder[_]](({
        val $u: StringOpsMacros.this.c.universe.type = StringOpsMacros.this.c.universe;
        val $m: $u.Mirror = StringOpsMacros.this.c.universe.rootMirror;
        $u.TypeTag.apply[Encoder[_]]($m, {
          final class $typecreator1 extends TypeCreator {
            def apply[U <: scala.reflect.api.Universe with Singleton]($m$untyped: scala.reflect.api.Mirror[U]): U#Type = {
              val $u: U = $m$untyped.universe;
              val $m: $u.Mirror = $m$untyped.asInstanceOf[$u.Mirror];
              val symdef$EncoderType1: $u.Symbol = $u.internal.reificationSupport.newNestedSymbol($u.internal.reificationSupport.selectTerm($u.internal.reificationSupport.selectType($m.staticModule("StringContextOps").asModule.moduleClass, "StringOpsMacros"), "sql_impl"), $u.TermName.apply("EncoderType"), $u.NoPosition, $u.internal.reificationSupport.FlagsRepr.apply(549755813888L), false);
              val symdef$_$11: $u.Symbol = $u.internal.reificationSupport.newNestedSymbol(symdef$EncoderType1, $u.TypeName.apply("_$1"), $u.NoPosition, $u.internal.reificationSupport.FlagsRepr.apply(34359738384L), false);
              $u.internal.reificationSupport.setInfo[$u.Symbol](symdef$EncoderType1, $u.NoType);
              $u.internal.reificationSupport.setInfo[$u.Symbol](symdef$_$11, $u.internal.reificationSupport.TypeBounds($m.staticClass("scala.Nothing").asType.toTypeConstructor, $m.staticClass("scala.Any").asType.toTypeConstructor));
              $u.internal.reificationSupport.ExistentialType(scala.collection.immutable.List.apply[$u.Symbol](symdef$_$11), $u.internal.reificationSupport.TypeRef($u.internal.reificationSupport.thisPrefix($m.EmptyPackageClass), $m.staticClass("Encoder"), scala.collection.immutable.List.apply[$u.Type]($u.internal.reificationSupport.TypeRef($u.NoPrefix, symdef$_$11, scala.collection.immutable.Nil))))
            }
          };
          new $typecreator1()
        })
      }: StringOpsMacros.this.c.universe.TypeTag[Encoder[_]]));
      argSeq.head
    }
  }
}
