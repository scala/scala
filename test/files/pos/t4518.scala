
import language.existentials

class Broke {

  val tempval = new AnyRef {val roleA = new AnyRef with Bar}.roleA

  new AnyRef {} -: tempval // when not assigning to anything, no problem
  val broke_val = new AnyRef {} -: tempval // type mismatch error only when assigning

  trait Foo[AnyRef] {  }

  trait Bar extends Foo[AnyRef] {
    def -:(core: AnyRef): this.type with Foo[core.type] = throw new Exception()
  }
}
