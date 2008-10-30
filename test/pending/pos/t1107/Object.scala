object O
{
   def d(t: Top) =
      t match
      {
         case s: Sub => true
         case _ => false
      }
}

