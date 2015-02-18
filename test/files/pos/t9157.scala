trait Flow[-In, +Out] {
  type Repr[+O] <: Flow[In, O]
  def map: Repr[String]
}

class Test {
  // typechecking was exponentially slow wrt the number of projections here.
  def slowFlow(
    f: Flow[String,String]#Repr[String]#Repr[String]#Repr[String]#Repr[String]#Repr[String]#Repr[String]#Repr[String]#Repr[String]#Repr[String]#Repr[String]#Repr[String]
  ) = {
    f.map
  }
}
