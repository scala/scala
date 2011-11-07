/* The Computer Language Shootout 
   http://shootout.alioth.debian.org/
   contributed by Isaac Gouy (Scala novice)
*/

object methcall {
   def main(args: Array[String]) = {
      var n = toPositiveInt(args);
      var v: Boolean = false

      val toggle = new Toggle(true);
      for (i <- Iterator.range(1,n)) v = toggle.activate.value;         

      Console println( toggle.activate.value );

      val ntoggle = new NToggle(true,3);
      for (i <- Iterator.range(1,n)) v = ntoggle.activate.value;    
     
      Console println( ntoggle.activate.value );
   }


   private def toPositiveInt(s: Array[String]) = {
      val i = 
         try { Integer.parseInt(s(0)); } 
         catch { case _ => 1 }
      if (i>0) i; else 1;
   }
}


private class Toggle(b: Boolean) {
   var state = b;

   def value = state;

   def activate = {
      state = !state;
      this 
   }
}


private class NToggle(b: Boolean, trigger: Int) 
extends Toggle(b) {

   val toggleTrigger = trigger;
   var count = 0;

   override def activate = {
      count = count + 1;
      if (count >= toggleTrigger) {
         state = !state;
         count = 0;
      }
      this
   }
}
