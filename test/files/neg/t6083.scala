object conv {
  implicit def i2s(i: Int): String = ""
}
import conv._

class annot(value: String) extends annotation.ConstantAnnotation
@annot(101) class C
