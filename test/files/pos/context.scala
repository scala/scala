class Context {
  object symwrap extends SymbolWrapper {
    val context: Context.this.type = Context.this
  }
  object typewrap extends TypeWrapper {
    val context: Context.this.type = Context.this
  }
  object symbols extends symwrap.Symbols;
  object types extends typewrap.Types;
}

abstract class SymbolWrapper {
  val context: Context;
  import context._;
  
  class Symbols {
    self: context.symbols.type =>
    
    abstract class Symbol {
      def typ: types.Type;
      def sym: Symbol = typ.sym;
    }
  }
}

abstract class TypeWrapper {
  val context: Context;
  import context._;

  class Types {
    self: context.types.type =>
    
    abstract class Type {
      def sym: symbols.Symbol;
      def typ: Type = sym.typ;
    }
  }
}
