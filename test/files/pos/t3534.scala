object Test {
   List[Option[Int]]() match {
     case None :: bb :: cc => ()
     case x => throw new Exception(x.filter(_.isDefined).mkString)
   }
 }