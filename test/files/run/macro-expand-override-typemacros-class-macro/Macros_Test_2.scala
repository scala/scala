import Impls._

// I'm not very fond of this behavior, but it's consistent with how type aliases work right noe

trait T11 { class T }
trait T12 extends T11 { type T = macro impl }
object M1 extends T12
class Test1 { type T1 = M1.T }

class C11 { class T }
class C12 extends C11 { type T = macro impl }
object M2 extends C12
class Test2 { type T2 = M1.T }

trait T21 { class T }
trait T22 extends T21 { override type T = macro impl }
object M3 extends T22
class Test3 { type T3 = M1.T }

class C21 { class T }
class C22 extends C21 { override type T = macro impl }
object M4 extends C22
class Test4 { type T4 = M1.T }

trait T31 { final class T }
trait T32 extends T31 { type T = macro impl }
object M5 extends T32
class Test5 { type T5 = M5.T }

class C31 { final class T }
class C32 extends C31 { type T = macro impl }
object M6 extends C32
class Test6 { type T6 = M1.T }

trait T41 { final class T }
trait T42 extends T41 { override type T = macro impl }
object M7 extends T42
class Test7 { type T7 = M1.T }

class C51 { final class T }
class C52 extends C51 { override type T = macro impl }
object M8 extends C52
class Test8 { type T8 = M1.T }

object Test extends App