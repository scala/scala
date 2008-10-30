sealed trait Top
sealed trait Sub extends Top

trait C
{
   private object P extends Sub
}


