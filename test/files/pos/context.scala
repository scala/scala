class Context {
  object symswrap extends SymsWrapper {
    val context: Context.this.type = Context.this
  }
  object typswrap extends TypsWrapper {
    val context: Context.this.type = Context.this
  }
  object syms extends symswrap.Syms;
  object typs extends typswrap.Typs;
}

abstract class SymsWrapper {
  val context: Context;
  import context._;

  class Syms: context.syms.type {
    abstract class Sym: context.syms.Sym {
      def typ: typs.Typ;
      def sym: Sym = typ.sym;
    }
  }
}

abstract class TypsWrapper {
  val context: Context;
  import context._;

  class Typs: context.typs.type {
    abstract class Typ {
      def sym: syms.Sym;
      def typ: Typ = sym.typ;
    }
  }
}
