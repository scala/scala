/** Check protected accessors involving polymorphic methods. */

package pkg2 {

trait PresentationsX extends pkg1.Presentations {
   trait ProjectImpl extends super.ProjectImpl {
     trait FileImpl extends super.FileImpl {
       lockTyper(Console.println)
     }
   }
}

} // pkg2

package pkg1 {

trait Presentations {
   trait ProjectImpl {
     trait FileImpl
     protected def lockTyper[T](f : => T) = {
       if (this == null) None
       else Some(f)
     }
   }
}

} // pkg1
