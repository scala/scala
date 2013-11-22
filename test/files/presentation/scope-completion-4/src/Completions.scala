package test

/* check that members defined in sub-block are not visible*/

class Completion1 {

  def f {
    
    def ff {
      
      def fff {
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
      
      def fcf {
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
      
      def ccf {
        /*_*/
      }

      /*_*/
    }
    
    /*_*/
    
    def cf {
      
      class cfc {
        /*_*/
      }
      
      /*_*/
      
      def cff {
        /*_*/
      }
      
      /*_*/
    }
    
    /*_*/
  }

  /*_*/
}
