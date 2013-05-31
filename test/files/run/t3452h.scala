class Mix___eFoo_I_wBar__f extends Foo_I_ with Bar__f { f; }
trait T
abstract class Foo_I_ { class I extends T    ; def f: I         ; f; }
trait Bar__f          { type  I>:Null<:T;      def f: I = {null}; f; def gobble: I = {null}}

object Test extends App {
  new Mix___eFoo_I_wBar__f
}
