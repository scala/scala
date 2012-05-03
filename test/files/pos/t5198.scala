package gaga





trait Sys[Self <: Sys[Self]] {
  type Tx
}


sealed trait AssocEntry[S <: Sys[S], @specialized(Int) A] {
  def value: A
  def value(implicit tx: S#Tx): A
}
