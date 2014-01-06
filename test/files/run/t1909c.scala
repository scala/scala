class Base(a: Any)

// java.lang.VerifyError: (class: Sub, method: <init> signature: ()V) Expecting to find object/array on stack
//  at Test$.<init>(t1909c.scala)
class Sub() extends Base({ def bippy = 5; bippy })

object Test extends App {
  new Sub()
}
