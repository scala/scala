package test

/* check that members defined in sub-block are not visible*/

class Completion1 {

  def f: Unit = {
    
    def ff: Unit = {
      
      def fff: Unit = {
        /*_*/
      }
      
      /*_*/
      
      class ffc {
        /*_*/
      }
      
      /*_*/
    }
    
    /*_*/
    
    class fc {
      
      def fcf: Unit = {
        /*_*/
      } 
      
      /*_*/
      
      class fcc {
        /*_*/
      }
      
      /*_*/
    }
    
    /*_*/
  }
  
  /*_*/
  
  class c {
    
    class cc {
      
      class ccc {
        /*_*/
      }
      
      /*_*/
      
      def ccf: Unit = {
        /*_*/
      }

      /*_*/
    }
    
    /*_*/
    
    def cf: Unit = {
      
      class cfc {
        /*_*/
      }
      
      /*_*/
      
      def cff: Unit = {
        /*_*/
      }
      
      /*_*/
    }
    
    /*_*/
  }

  /*_*/
}
