class Yo[Unused] {
  def yo(hasDefault: Any = ""): String = ""
}

class MacroNotExpanded {
  implicit def toYo[Unused](a: Any)(implicit ct: reflect.ClassTag[Unused]): Yo[Unused] = new Yo[Unused]
   "".yo()  
}
