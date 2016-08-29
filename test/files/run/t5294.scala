import scala.language.higherKinds

package p {
  trait T[+A, +CC] {
    def t: CC
  }
  class C {
    def test[CC[X] <: T[X,String] with T[X,Int]](from: CC[_]): Unit = ()
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    val symtab = reflect.runtime.universe.asInstanceOf[reflect.internal.SymbolTable]
    val CTpe = reflect.runtime.universe.typeOf[p.C].asInstanceOf[symtab.Type]
    val TClass = reflect.runtime.universe.symbolOf[p.T[_, _]].asInstanceOf[symtab.Symbol]
    import symtab._
    val from = CTpe.member(TermName("test")).paramss.head.head
    assert(from.baseClasses contains TClass)
    assert(from.info.baseTypeIndex(TClass) != -1) // was failing!
  }
}
