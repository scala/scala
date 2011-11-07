/* The Computer Language Shootout 
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy (Scala novice)
*/

import concurrent.SyncVar;
import concurrent.ops._;

object prodcons {
   def main(args: Array[String]) = {
      val n = toPositiveInt(args);
      val buffer = new SharedBuffer();
      var p = 0;
      var c = 0;
      val cDone = new SyncVar[Boolean];

      spawn { 
         while(p<n) { p=p+1; buffer put(p); }
      }

      spawn { 
         var v: Int = _;
         while(c<n) { c=c+1; v = buffer.get; }
         cDone set true;
      }

      cDone.get;
      Console println(p + " " + c); 
   }


   private def toPositiveInt(s: Array[String]) = {
      val i = 
         try { Integer.parseInt(s(0)); } 
         catch { case _ => 1 }
      if (i>0) i; else 1;
   }
}


private class SharedBuffer() {
   var contents: Int = _;
   var available = false;

   def get = synchronized {
      while (available == false) wait();
      available = false;
         // Console println("\t" + "get " + contents);
      notifyAll();
      contents
   }

   def put(value: Int) = synchronized {
      while (available == true) wait();
      contents = value;
      available = true;
         // Console println("put " + value);
      notifyAll();
   }
}




