object Test extends AnyRef with App {
  val x = 1;

  def try1 = {
    Console.print("1 + 1 = ");
    Console.println(1 + (
      try {
        x;
      } catch {
        case _: Error => 1;
      }
    ));
  }

  def try2 = {
    Console.print("1 + 1 = ");
    Console.println(
      (try { x } catch {
        case _: Error => 1;
      }) 
      +        
      (try { x } catch {
        case _: Error => 1;
      })
    );
  }

  var n = 0;

  def try3 = {
    Console.print("1 + 1 = ");
    val x = try { 1 } catch {
      case e: Error => 1;
    }
    this.n = try { 1 } catch {
      case e: Error => 1;
    }
    Console.println(x + n);
  }

  var instance: AnyRef = null;

  def try4 = {
    if (instance == null) {
      instance = try {
        "" //new String();
      } catch {
        case _ =>
          val cs = "aaa";
          if (cs.length() > 0) {
            "" //new String();
          } else {
            throw new Error("fatal error");
            null
          }
      }
    }
  }

  def try5 = try {
    Console.print("1 + 1 = ");
    try {
      if (true)
        error("exit");
      1+1;
      ()
    } catch {
      case _ =>
        Console.println("2");
        error("for good");
    }
    Console.println("a");
  } catch {
    case _ => ();
  }

  class A {
    private val result = {
      val y = try { x } catch {
          case _: Error => 1;
        };
      x + y
    }
    Console.print("1 + 1 = ");
    Console.println(result);
  }

  // ticket #981
  def try6 {
   class SekwencjaArray {
    def get = null
   }

   var sekw : SekwencjaArray =
     try {
       null
     } catch {
       case _ => null
     }

    new AnyRef {
      def getValueAt(row:Int, col:Int) = sekw.get
    }
  }

/*
  def finally1 = {
    Console.print("1 + 1 = ");
    Console.println(1 + (
      try {
        x
      } finally {
        ()
      }
    ));
  }

*/
    
  try1;
  try2;
  try3;
  try4;
  try5;
  try6;
  Console.println;
  new A();
  ()
}
