trait Fun[A, B] { def apply(a: A): B }
// can't do sam expansion until the sam body def is a static method in the sam class, and not a local method in a block'
class C(f: Fun[Int, String])
class Test extends C(s => "a")