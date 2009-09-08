import scala.collection.mutable._;
import scala.collection.script._;

class Members;

object subscriber extends Subscriber[Message[String] with Undoable, Members] {
 def notify(pub: Members, event: Message[String] with Undoable): Unit = 
  (event: Message[String]) match {
   case Include(l, elem) => Console.println("ADD: " + elem);
   case  Remove(l, elem) => Console.println("REM: " + elem);
   //case i : Include[HasTree] with Undoable  =>
   //case r : Remove [HasTree] with Undoable  =>
  }
 }
  
