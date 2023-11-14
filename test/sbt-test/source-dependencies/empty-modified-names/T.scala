trait T2
trait T1 extends T2 { def foo: String }
class C1 extends T1 { def foo = "test" }
