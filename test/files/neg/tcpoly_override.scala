// bug1231: reported by Vladimir Reshetnikov on 19 July 2007
trait A {
 type T[_]
}

trait B // First-order type

class C extends A {
 type T = B // This compiles well (@M: ... but it shouldn't)
}
