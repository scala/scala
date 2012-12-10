trait T { type U }
// "abstract override" shouldn't be allowed on types
trait T1 extends T { abstract override type U = Int }
