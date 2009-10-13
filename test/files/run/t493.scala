object Test {

 val y = new collection.mutable.HashMap[String,Any]
 val z = new collection.mutable.HashMap[String,Any]

 y("msg") = Array[String]("1","2")

 val array: Array[String] = Array[String]("1","2")
 z("msg") = array

 def main(args:Array[String]) = {

   assert(y("msg").isInstanceOf[Array[_]])
   assert(z("msg").isInstanceOf[Array[_]])

   // these work, without producing a match error

   (z.get("msg"): @unchecked) match {
     case Some(_:Array[String]) =>
   }
  }
}
