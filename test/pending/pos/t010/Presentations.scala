package pkg1
trait Presentations {
   trait ProjectImpl {
     trait FileImpl
     protected def lockTyper[T](f : => T) = {
       if (this == null) None
       else Some(f)
     }
   }
}
