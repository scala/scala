object Test extends App {
case class IntOnly(i: Int, j: Int)

println("IntOnly: should return an unboxed int")
val a = IntOnly(1, 2)
val i: Int = a.productElement(0)
println(s"Int: ${a.productElement(0).getClass}")

case class IntAndDouble(i: Int, d: Double)

println("IntAndDouble: should just box and return Anyval")
val b = IntAndDouble(1, 2.0)
val j: AnyVal = b.productElement(0)
println(s"Double: ${b.productElement(1).getClass}")
println(s"Int: ${b.productElement(0).getClass}")
}
