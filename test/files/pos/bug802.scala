package test;
trait Test {
  abstract class BracesImpl { 
    type Singleton;
    type Brace <: Singleton with BraceImpl;
    trait BraceImpl; 
    trait ForFile;
  }
  abstract class ParensImpl extends BracesImpl { 
    type Brace <: Singleton with BraceImpl;
    trait BraceImpl extends super.BraceImpl;
  }
  val parens : ParensImpl;
  abstract class BracksImpl extends BracesImpl { 
    type Brace <: Singleton with BraceImpl;
    trait BraceImpl extends super.BraceImpl;
  }
  val bracks : BracksImpl;
  trait File {
    def parens0 : parens.BraceImpl;
    def bracks0 : bracks.BraceImpl;
    def braces(b : BracesImpl) = b match {
    case b if b == parens => parens0;
    case b if b == bracks => bracks0;
    }
  }
}
