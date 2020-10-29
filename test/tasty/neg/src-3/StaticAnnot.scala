package tastytest

object StaticAnnot {

  @annotation.static def sum(x: Int, y: Int): Int = x + y

}

class StaticAnnot
