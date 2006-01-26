import scala.collection.mutable._;

class Members;

object subscriber extends Subscriber[Message[String] with Undoable, Members] {
 def notify(pub: Members, event: Message[String] with Undoable): Unit =
  (event: Message[String]) match {
   case Include(elem) => System.err.println("ADD: " + elem);
   case  Remove(elem) => System.err.println("REM: " + elem);
   //case i : Include[HasTree] with Undoable  =>
   //case r : Remove [HasTree] with Undoable  =>
  }
 }

