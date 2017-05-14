class Test {
  class MyExist extends ExistF[MyExist]
  // scala/bug#8197, scala/bug#6169: java infers the bounds of existentials, so we have to as well now that scala/bug#1786 is fixed...
  def stringy: Exist[_ <: String] = (new Exist[String]).foo
  def fbounded: (ExistF[t] forSome {type t <: ExistF[t] }) = (new MyExist).foo
  def indir: ExistIndir[_ <: String, _ <: String] = (new ExistIndir[String, String]).foo
}