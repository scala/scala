package pkg2
trait PresentationsX extends pkg1.Presentations {
   trait ProjectImpl extends super.ProjectImpl {
     trait FileImpl extends super.FileImpl {
       lockTyper(Console.println)
     }
   }
}
