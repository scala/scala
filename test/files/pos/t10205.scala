package some
class ToArrayBug {
  val someArray:Array[_] = new java.util.ArrayList[ToArrayBug].toArray
}
