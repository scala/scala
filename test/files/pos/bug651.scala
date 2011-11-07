package test;

trait Test3 {
  trait MatchableImpl {
    trait MatchImpl;
  }
  
  trait BracePairImpl {
    trait BraceImpl extends MatchableImpl {
      private object MyMatch1 extends MatchImpl;
      protected def match0 : MatchImpl = MyMatch1;
      
    }
  }
}
