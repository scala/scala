trait T2 { def foo: String }
trait T1 extends T2
class C1 extends T1 { def foo = "test" }
