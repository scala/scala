

/* ================================================================================
         Automatically generated on 2011-05-11. Do Not Edit (unless you have to).
         (2-level nesting)
   ================================================================================ */ 



class Class2_1 {
  
  class Class1_2 {
    var ObjCounter = 0
    
    object Obj  { ObjCounter += 1}
    Obj // one

    def singleThreadedAccess(x: Any) = {
      x == Obj
    }

    def runTest {
      try {
        assert(singleThreadedAccess(Obj))
        assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
        println("ok")
      } catch {
        case e =>  print("failed "); e.printStackTrace()
      }
    }

    def run { runTest }
  }
  
  def run { (new Class1_2).run }
}


object Object3_1 {
  
  class Class1_2 {
    var ObjCounter = 0
    
    object Obj  { ObjCounter += 1}
    Obj // one

    def singleThreadedAccess(x: Any) = {
      x == Obj
    }

    def runTest {
      try {
        assert(singleThreadedAccess(Obj))
        assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
        println("ok")
      } catch {
        case e =>  print("failed "); e.printStackTrace()
      }
    }

    def run { runTest }
  }
  
  def run { (new Class1_2).run } // trigger
}


trait Trait4_1 {
  
  class Class1_2 {
    var ObjCounter = 0
    
    object Obj  { ObjCounter += 1}
    Obj // one

    def singleThreadedAccess(x: Any) = {
      x == Obj
    }

    def runTest {
      try {
        assert(singleThreadedAccess(Obj))
        assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
        println("ok")
      } catch {
        case e =>  print("failed "); e.printStackTrace()
      }
    }

    def run { runTest }
  }
  
  def run { (new Class1_2).run }
}


class Class6_1 {
  
  object Object5_2 {
    var ObjCounter = 0
    
    object Obj  { ObjCounter += 1}
    Obj // one

    def singleThreadedAccess(x: Any) = {
      x == Obj
    }

    def runTest {
      try {
        assert(singleThreadedAccess(Obj))
        assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
        println("ok")
      } catch {
        case e =>  print("failed "); e.printStackTrace()
      }
    }

    def run { runTest } // trigger
  }
  
  def run { Object5_2.run }
}


object Object7_1 {
  
  object Object5_2 {
    var ObjCounter = 0
    
    object Obj  { ObjCounter += 1}
    Obj // one

    def singleThreadedAccess(x: Any) = {
      x == Obj
    }

    def runTest {
      try {
        assert(singleThreadedAccess(Obj))
        assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
        println("ok")
      } catch {
        case e =>  print("failed "); e.printStackTrace()
      }
    }

    def run { runTest } // trigger
  }
  
  def run { Object5_2.run } // trigger
}


trait Trait8_1 {
  
  object Object5_2 {
    var ObjCounter = 0
    
    object Obj  { ObjCounter += 1}
    Obj // one

    def singleThreadedAccess(x: Any) = {
      x == Obj
    }

    def runTest {
      try {
        assert(singleThreadedAccess(Obj))
        assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
        println("ok")
      } catch {
        case e =>  print("failed "); e.printStackTrace()
      }
    }

    def run { runTest } // trigger
  }
  
  def run { Object5_2.run }
}


class Class10_1 {
  
  trait Trait9_2 {
    var ObjCounter = 0
    
    object Obj  { ObjCounter += 1}
    Obj // one

    def singleThreadedAccess(x: Any) = {
      x == Obj
    }

    def runTest {
      try {
        assert(singleThreadedAccess(Obj))
        assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
        println("ok")
      } catch {
        case e =>  print("failed "); e.printStackTrace()
      }
    }

    def run { runTest }
  }
  
  def run { (new Trait9_2 {}).run }
}


object Object11_1 {
  
  trait Trait9_2 {
    var ObjCounter = 0
    
    object Obj  { ObjCounter += 1}
    Obj // one

    def singleThreadedAccess(x: Any) = {
      x == Obj
    }

    def runTest {
      try {
        assert(singleThreadedAccess(Obj))
        assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
        println("ok")
      } catch {
        case e =>  print("failed "); e.printStackTrace()
      }
    }

    def run { runTest }
  }
  
  def run { (new Trait9_2 {}).run } // trigger
}


trait Trait12_1 {
  
  trait Trait9_2 {
    var ObjCounter = 0
    
    object Obj  { ObjCounter += 1}
    Obj // one

    def singleThreadedAccess(x: Any) = {
      x == Obj
    }

    def runTest {
      try {
        assert(singleThreadedAccess(Obj))
        assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
        println("ok")
      } catch {
        case e =>  print("failed "); e.printStackTrace()
      }
    }

    def run { runTest }
  }
  
  def run { (new Trait9_2 {}).run }
}


class Class14_1 {
  
  def method13_2 {
    var ObjCounter = 0
    
    object Obj  { ObjCounter += 1}
    Obj // one

    def singleThreadedAccess(x: Any) = {
      x == Obj
    }

    def runTest {
      try {
        assert(singleThreadedAccess(Obj))
        assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
        println("ok")
      } catch {
        case e =>  print("failed "); e.printStackTrace()
      }
    }

    runTest // trigger
  }
  
  def run { method13_2 }
}


object Object15_1 {
  
  def method13_2 {
    var ObjCounter = 0
    
    object Obj  { ObjCounter += 1}
    Obj // one

    def singleThreadedAccess(x: Any) = {
      x == Obj
    }

    def runTest {
      try {
        assert(singleThreadedAccess(Obj))
        assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
        println("ok")
      } catch {
        case e =>  print("failed "); e.printStackTrace()
      }
    }

    runTest // trigger
  }
  
  def run { method13_2 } // trigger
}


trait Trait16_1 {
  
  def method13_2 {
    var ObjCounter = 0
    
    object Obj  { ObjCounter += 1}
    Obj // one

    def singleThreadedAccess(x: Any) = {
      x == Obj
    }

    def runTest {
      try {
        assert(singleThreadedAccess(Obj))
        assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
        println("ok")
      } catch {
        case e =>  print("failed "); e.printStackTrace()
      }
    }

    runTest // trigger
  }
  
  def run { method13_2 }
}


class Class18_1 {
  
  private def method17_2 {
    var ObjCounter = 0
    
    object Obj  { ObjCounter += 1}
    Obj // one

    def singleThreadedAccess(x: Any) = {
      x == Obj
    }

    def runTest {
      try {
        assert(singleThreadedAccess(Obj))
        assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
        println("ok")
      } catch {
        case e =>  print("failed "); e.printStackTrace()
      }
    }

    runTest // trigger
  }
  
  def run { method17_2 }
}


object Object19_1 {
  
  private def method17_2 {
    var ObjCounter = 0
    
    object Obj  { ObjCounter += 1}
    Obj // one

    def singleThreadedAccess(x: Any) = {
      x == Obj
    }

    def runTest {
      try {
        assert(singleThreadedAccess(Obj))
        assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
        println("ok")
      } catch {
        case e =>  print("failed "); e.printStackTrace()
      }
    }

    runTest // trigger
  }
  
  def run { method17_2 } // trigger
}


trait Trait20_1 {
  
  private def method17_2 {
    var ObjCounter = 0
    
    object Obj  { ObjCounter += 1}
    Obj // one

    def singleThreadedAccess(x: Any) = {
      x == Obj
    }

    def runTest {
      try {
        assert(singleThreadedAccess(Obj))
        assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
        println("ok")
      } catch {
        case e =>  print("failed "); e.printStackTrace()
      }
    }

    runTest // trigger
  }
  
  def run { method17_2 }
}


class Class22_1 {
  
  val fun21_2 = () => {
    var ObjCounter = 0
    
    object Obj  { ObjCounter += 1}
    Obj // one

    def singleThreadedAccess(x: Any) = {
      x == Obj
    }

    def runTest {
      try {
        assert(singleThreadedAccess(Obj))
        assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
        println("ok")
      } catch {
        case e =>  print("failed "); e.printStackTrace()
      }
    }

    runTest // trigger
  }
  
  def run { fun21_2() }
}


object Object23_1 {
  
  val fun21_2 = () => {
    var ObjCounter = 0
    
    object Obj  { ObjCounter += 1}
    Obj // one

    def singleThreadedAccess(x: Any) = {
      x == Obj
    }

    def runTest {
      try {
        assert(singleThreadedAccess(Obj))
        assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
        println("ok")
      } catch {
        case e =>  print("failed "); e.printStackTrace()
      }
    }

    runTest // trigger
  }
  
  def run { fun21_2() } // trigger
}


trait Trait24_1 {
  
  val fun21_2 = () => {
    var ObjCounter = 0
    
    object Obj  { ObjCounter += 1}
    Obj // one

    def singleThreadedAccess(x: Any) = {
      x == Obj
    }

    def runTest {
      try {
        assert(singleThreadedAccess(Obj))
        assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
        println("ok")
      } catch {
        case e =>  print("failed "); e.printStackTrace()
      }
    }

    runTest // trigger
  }
  
  def run { fun21_2() }
}


class Class26_1 {
  
  class Class25_2 {
    { // in primary constructor 
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
          println("ok")
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
      }

      runTest // trigger
    } 
  }
  
  def run { (new Class25_2) }
}


object Object27_1 {
  
  class Class25_2 {
    { // in primary constructor 
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
          println("ok")
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
      }

      runTest // trigger
    } 
  }
  
  def run { (new Class25_2) } // trigger
}


trait Trait28_1 {
  
  class Class25_2 {
    { // in primary constructor 
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
          println("ok")
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
      }

      runTest // trigger
    } 
  }
  
  def run { (new Class25_2) }
}


class Class30_1 {
  
  trait Trait29_2 {
    { // in primary constructor 
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
          println("ok")
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
      }

      runTest // trigger
    } 
  }
  
  def run { (new Trait29_2 {}) }
}


object Object31_1 {
  
  trait Trait29_2 {
    { // in primary constructor 
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
          println("ok")
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
      }

      runTest // trigger
    } 
  }
  
  def run { (new Trait29_2 {}) } // trigger
}


trait Trait32_1 {
  
  trait Trait29_2 {
    { // in primary constructor 
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
          println("ok")
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
      }

      runTest // trigger
    } 
  }
  
  def run { (new Trait29_2 {}) }
}


class Class34_1 {
  
  lazy val lzvalue33_2 = {
    var ObjCounter = 0
    
    object Obj  { ObjCounter += 1}
    Obj // one

    def singleThreadedAccess(x: Any) = {
      x == Obj
    }

    def runTest {
      try {
        assert(singleThreadedAccess(Obj))
        assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
        println("ok")
      } catch {
        case e =>  print("failed "); e.printStackTrace()
      }
    }

    runTest // trigger
  }
  
  def run { lzvalue33_2 }
}


object Object35_1 {
  
  lazy val lzvalue33_2 = {
    var ObjCounter = 0
    
    object Obj  { ObjCounter += 1}
    Obj // one

    def singleThreadedAccess(x: Any) = {
      x == Obj
    }

    def runTest {
      try {
        assert(singleThreadedAccess(Obj))
        assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
        println("ok")
      } catch {
        case e =>  print("failed "); e.printStackTrace()
      }
    }

    runTest // trigger
  }
  
  def run { lzvalue33_2 } // trigger
}


trait Trait36_1 {
  
  lazy val lzvalue33_2 = {
    var ObjCounter = 0
    
    object Obj  { ObjCounter += 1}
    Obj // one

    def singleThreadedAccess(x: Any) = {
      x == Obj
    }

    def runTest {
      try {
        assert(singleThreadedAccess(Obj))
        assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
        println("ok")
      } catch {
        case e =>  print("failed "); e.printStackTrace()
      }
    }

    runTest // trigger
  }
  
  def run { lzvalue33_2 }
}


class Class38_1 {
  
  val value37_2 = {
    var ObjCounter = 0
    
    object Obj  { ObjCounter += 1}
    Obj // one

    def singleThreadedAccess(x: Any) = {
      x == Obj
    }

    def runTest {
      try {
        assert(singleThreadedAccess(Obj))
        assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
        println("ok")
      } catch {
        case e =>  print("failed "); e.printStackTrace()
      }
    }

    runTest // trigger
  }
  
  def run { value37_2 }
}


object Object39_1 {
  
  val value37_2 = {
    var ObjCounter = 0
    
    object Obj  { ObjCounter += 1}
    Obj // one

    def singleThreadedAccess(x: Any) = {
      x == Obj
    }

    def runTest {
      try {
        assert(singleThreadedAccess(Obj))
        assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
        println("ok")
      } catch {
        case e =>  print("failed "); e.printStackTrace()
      }
    }

    runTest // trigger
  }
  
  def run { value37_2 } // trigger
}


trait Trait40_1 {
  
  val value37_2 = {
    var ObjCounter = 0
    
    object Obj  { ObjCounter += 1}
    Obj // one

    def singleThreadedAccess(x: Any) = {
      x == Obj
    }

    def runTest {
      try {
        assert(singleThreadedAccess(Obj))
        assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
        println("ok")
      } catch {
        case e =>  print("failed "); e.printStackTrace()
      }
    }

    runTest // trigger
  }
  
  def run { value37_2 }
}


class Class42_1 {
  
  class Class41_2 {
    var ObjCounter = 0
    
    private object Obj  { ObjCounter += 1}
    Obj // one

    def singleThreadedAccess(x: Any) = {
      x == Obj
    }

    def runTest {
      try {
        assert(singleThreadedAccess(Obj))
        assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
        println("ok")
      } catch {
        case e =>  print("failed "); e.printStackTrace()
      }
    }

    def run { runTest }
  }
  
  def run { (new Class41_2).run }
}


object Object43_1 {
  
  class Class41_2 {
    var ObjCounter = 0
    
    private object Obj  { ObjCounter += 1}
    Obj // one

    def singleThreadedAccess(x: Any) = {
      x == Obj
    }

    def runTest {
      try {
        assert(singleThreadedAccess(Obj))
        assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
        println("ok")
      } catch {
        case e =>  print("failed "); e.printStackTrace()
      }
    }

    def run { runTest }
  }
  
  def run { (new Class41_2).run } // trigger
}


trait Trait44_1 {
  
  class Class41_2 {
    var ObjCounter = 0
    
    private object Obj  { ObjCounter += 1}
    Obj // one

    def singleThreadedAccess(x: Any) = {
      x == Obj
    }

    def runTest {
      try {
        assert(singleThreadedAccess(Obj))
        assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
        println("ok")
      } catch {
        case e =>  print("failed "); e.printStackTrace()
      }
    }

    def run { runTest }
  }
  
  def run { (new Class41_2).run }
}


class Class46_1 {
  
  object Object45_2 {
    var ObjCounter = 0
    
    private object Obj  { ObjCounter += 1}
    Obj // one

    def singleThreadedAccess(x: Any) = {
      x == Obj
    }

    def runTest {
      try {
        assert(singleThreadedAccess(Obj))
        assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
        println("ok")
      } catch {
        case e =>  print("failed "); e.printStackTrace()
      }
    }

    def run { runTest } // trigger
  }
  
  def run { Object45_2.run }
}


object Object47_1 {
  
  object Object45_2 {
    var ObjCounter = 0
    
    private object Obj  { ObjCounter += 1}
    Obj // one

    def singleThreadedAccess(x: Any) = {
      x == Obj
    }

    def runTest {
      try {
        assert(singleThreadedAccess(Obj))
        assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
        println("ok")
      } catch {
        case e =>  print("failed "); e.printStackTrace()
      }
    }

    def run { runTest } // trigger
  }
  
  def run { Object45_2.run } // trigger
}


trait Trait48_1 {
  
  object Object45_2 {
    var ObjCounter = 0
    
    private object Obj  { ObjCounter += 1}
    Obj // one

    def singleThreadedAccess(x: Any) = {
      x == Obj
    }

    def runTest {
      try {
        assert(singleThreadedAccess(Obj))
        assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
        println("ok")
      } catch {
        case e =>  print("failed "); e.printStackTrace()
      }
    }

    def run { runTest } // trigger
  }
  
  def run { Object45_2.run }
}


class Class50_1 {
  
  trait Trait49_2 {
    var ObjCounter = 0
    
    private object Obj  { ObjCounter += 1}
    Obj // one

    def singleThreadedAccess(x: Any) = {
      x == Obj
    }

    def runTest {
      try {
        assert(singleThreadedAccess(Obj))
        assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
        println("ok")
      } catch {
        case e =>  print("failed "); e.printStackTrace()
      }
    }

    def run { runTest }
  }
  
  def run { (new Trait49_2 {}).run }
}


object Object51_1 {
  
  trait Trait49_2 {
    var ObjCounter = 0
    
    private object Obj  { ObjCounter += 1}
    Obj // one

    def singleThreadedAccess(x: Any) = {
      x == Obj
    }

    def runTest {
      try {
        assert(singleThreadedAccess(Obj))
        assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
        println("ok")
      } catch {
        case e =>  print("failed "); e.printStackTrace()
      }
    }

    def run { runTest }
  }
  
  def run { (new Trait49_2 {}).run } // trigger
}


trait Trait52_1 {
  
  trait Trait49_2 {
    var ObjCounter = 0
    
    private object Obj  { ObjCounter += 1}
    Obj // one

    def singleThreadedAccess(x: Any) = {
      x == Obj
    }

    def runTest {
      try {
        assert(singleThreadedAccess(Obj))
        assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
        println("ok")
      } catch {
        case e =>  print("failed "); e.printStackTrace()
      }
    }

    def run { runTest }
  }
  
  def run { (new Trait49_2 {}).run }
}


class Class54_1 {
  
  class Class53_2 {
    @volatile var ObjCounter = 0
    
    object Obj  { ObjCounter += 1}

    def multiThreadedAccess() {
      val threads = for (i <- 1 to 5) yield new Thread(new Runnable {
        def run = Obj
      })

      threads foreach (_.start())
      threads foreach (_.join())
    }

    def runTest {
      try {
        multiThreadedAccess()
        assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
        println("ok")
      } catch {
        case e =>  print("multi-threaded failed "); e.printStackTrace()
      }
    }

    def run { runTest }
  }
  
  def run { (new Class53_2).run }
}


object Object55_1 {
  
  class Class53_2 {
    @volatile var ObjCounter = 0
    
    object Obj  { ObjCounter += 1}

    def multiThreadedAccess() {
      val threads = for (i <- 1 to 5) yield new Thread(new Runnable {
        def run = Obj
      })

      threads foreach (_.start())
      threads foreach (_.join())
    }

    def runTest {
      try {
        multiThreadedAccess()
        assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
        println("ok")
      } catch {
        case e =>  print("multi-threaded failed "); e.printStackTrace()
      }
    }

    def run { runTest }
  }
  
  def run { (new Class53_2).run } // trigger
}


trait Trait56_1 {
  
  class Class53_2 {
    @volatile var ObjCounter = 0
    
    object Obj  { ObjCounter += 1}

    def multiThreadedAccess() {
      val threads = for (i <- 1 to 5) yield new Thread(new Runnable {
        def run = Obj
      })

      threads foreach (_.start())
      threads foreach (_.join())
    }

    def runTest {
      try {
        multiThreadedAccess()
        assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
        println("ok")
      } catch {
        case e =>  print("multi-threaded failed "); e.printStackTrace()
      }
    }

    def run { runTest }
  }
  
  def run { (new Class53_2).run }
}


class Class58_1 {
  
  object Object57_2 {
    @volatile var ObjCounter = 0
    
    object Obj  { ObjCounter += 1}

    def multiThreadedAccess() {
      val threads = for (i <- 1 to 5) yield new Thread(new Runnable {
        def run = Obj
      })

      threads foreach (_.start())
      threads foreach (_.join())
    }

    def runTest {
      try {
        multiThreadedAccess()
        assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
        println("ok")
      } catch {
        case e =>  print("multi-threaded failed "); e.printStackTrace()
      }
    }

    def run { runTest } // trigger
  }
  
  def run { Object57_2.run }
}


object Object59_1 {
  
  object Object57_2 {
    @volatile var ObjCounter = 0
    
    object Obj  { ObjCounter += 1}

    def multiThreadedAccess() {
      val threads = for (i <- 1 to 5) yield new Thread(new Runnable {
        def run = Obj
      })

      threads foreach (_.start())
      threads foreach (_.join())
    }

    def runTest {
      try {
        multiThreadedAccess()
        assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
        println("ok")
      } catch {
        case e =>  print("multi-threaded failed "); e.printStackTrace()
      }
    }

    def run { runTest } // trigger
  }
  
  def run { Object57_2.run } // trigger
}


trait Trait60_1 {
  
  object Object57_2 {
    @volatile var ObjCounter = 0
    
    object Obj  { ObjCounter += 1}

    def multiThreadedAccess() {
      val threads = for (i <- 1 to 5) yield new Thread(new Runnable {
        def run = Obj
      })

      threads foreach (_.start())
      threads foreach (_.join())
    }

    def runTest {
      try {
        multiThreadedAccess()
        assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
        println("ok")
      } catch {
        case e =>  print("multi-threaded failed "); e.printStackTrace()
      }
    }

    def run { runTest } // trigger
  }
  
  def run { Object57_2.run }
}


class Class62_1 {
  
  trait Trait61_2 {
    @volatile var ObjCounter = 0
    
    object Obj  { ObjCounter += 1}

    def multiThreadedAccess() {
      val threads = for (i <- 1 to 5) yield new Thread(new Runnable {
        def run = Obj
      })

      threads foreach (_.start())
      threads foreach (_.join())
    }

    def runTest {
      try {
        multiThreadedAccess()
        assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
        println("ok")
      } catch {
        case e =>  print("multi-threaded failed "); e.printStackTrace()
      }
    }

    def run { runTest }
  }
  
  def run { (new Trait61_2 {}).run }
}


object Object63_1 {
  
  trait Trait61_2 {
    @volatile var ObjCounter = 0
    
    object Obj  { ObjCounter += 1}

    def multiThreadedAccess() {
      val threads = for (i <- 1 to 5) yield new Thread(new Runnable {
        def run = Obj
      })

      threads foreach (_.start())
      threads foreach (_.join())
    }

    def runTest {
      try {
        multiThreadedAccess()
        assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
        println("ok")
      } catch {
        case e =>  print("multi-threaded failed "); e.printStackTrace()
      }
    }

    def run { runTest }
  }
  
  def run { (new Trait61_2 {}).run } // trigger
}


trait Trait64_1 {
  
  trait Trait61_2 {
    @volatile var ObjCounter = 0
    
    object Obj  { ObjCounter += 1}

    def multiThreadedAccess() {
      val threads = for (i <- 1 to 5) yield new Thread(new Runnable {
        def run = Obj
      })

      threads foreach (_.start())
      threads foreach (_.join())
    }

    def runTest {
      try {
        multiThreadedAccess()
        assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
        println("ok")
      } catch {
        case e =>  print("multi-threaded failed "); e.printStackTrace()
      }
    }

    def run { runTest }
  }
  
  def run { (new Trait61_2 {}).run }
}


class Class66_1 {
  
  def method65_2 {
    @volatile var ObjCounter = 0
    
    object Obj  { ObjCounter += 1}

    def multiThreadedAccess() {
      val threads = for (i <- 1 to 5) yield new Thread(new Runnable {
        def run = Obj
      })

      threads foreach (_.start())
      threads foreach (_.join())
    }

    def runTest {
      try {
        multiThreadedAccess()
        assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
        println("ok")
      } catch {
        case e =>  print("multi-threaded failed "); e.printStackTrace()
      }
    }

    runTest // trigger
  }
  
  def run { method65_2 }
}


object Object67_1 {
  
  def method65_2 {
    @volatile var ObjCounter = 0
    
    object Obj  { ObjCounter += 1}

    def multiThreadedAccess() {
      val threads = for (i <- 1 to 5) yield new Thread(new Runnable {
        def run = Obj
      })

      threads foreach (_.start())
      threads foreach (_.join())
    }

    def runTest {
      try {
        multiThreadedAccess()
        assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
        println("ok")
      } catch {
        case e =>  print("multi-threaded failed "); e.printStackTrace()
      }
    }

    runTest // trigger
  }
  
  def run { method65_2 } // trigger
}


trait Trait68_1 {
  
  def method65_2 {
    @volatile var ObjCounter = 0
    
    object Obj  { ObjCounter += 1}

    def multiThreadedAccess() {
      val threads = for (i <- 1 to 5) yield new Thread(new Runnable {
        def run = Obj
      })

      threads foreach (_.start())
      threads foreach (_.join())
    }

    def runTest {
      try {
        multiThreadedAccess()
        assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
        println("ok")
      } catch {
        case e =>  print("multi-threaded failed "); e.printStackTrace()
      }
    }

    runTest // trigger
  }
  
  def run { method65_2 }
}


class Class70_1 {
  
  private def method69_2 {
    @volatile var ObjCounter = 0
    
    object Obj  { ObjCounter += 1}

    def multiThreadedAccess() {
      val threads = for (i <- 1 to 5) yield new Thread(new Runnable {
        def run = Obj
      })

      threads foreach (_.start())
      threads foreach (_.join())
    }

    def runTest {
      try {
        multiThreadedAccess()
        assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
        println("ok")
      } catch {
        case e =>  print("multi-threaded failed "); e.printStackTrace()
      }
    }

    runTest // trigger
  }
  
  def run { method69_2 }
}


object Object71_1 {
  
  private def method69_2 {
    @volatile var ObjCounter = 0
    
    object Obj  { ObjCounter += 1}

    def multiThreadedAccess() {
      val threads = for (i <- 1 to 5) yield new Thread(new Runnable {
        def run = Obj
      })

      threads foreach (_.start())
      threads foreach (_.join())
    }

    def runTest {
      try {
        multiThreadedAccess()
        assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
        println("ok")
      } catch {
        case e =>  print("multi-threaded failed "); e.printStackTrace()
      }
    }

    runTest // trigger
  }
  
  def run { method69_2 } // trigger
}


trait Trait72_1 {
  
  private def method69_2 {
    @volatile var ObjCounter = 0
    
    object Obj  { ObjCounter += 1}

    def multiThreadedAccess() {
      val threads = for (i <- 1 to 5) yield new Thread(new Runnable {
        def run = Obj
      })

      threads foreach (_.start())
      threads foreach (_.join())
    }

    def runTest {
      try {
        multiThreadedAccess()
        assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
        println("ok")
      } catch {
        case e =>  print("multi-threaded failed "); e.printStackTrace()
      }
    }

    runTest // trigger
  }
  
  def run { method69_2 }
}


class Class74_1 {
  
  val fun73_2 = () => {
    @volatile var ObjCounter = 0
    
    object Obj  { ObjCounter += 1}

    def multiThreadedAccess() {
      val threads = for (i <- 1 to 5) yield new Thread(new Runnable {
        def run = Obj
      })

      threads foreach (_.start())
      threads foreach (_.join())
    }

    def runTest {
      try {
        multiThreadedAccess()
        assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
        println("ok")
      } catch {
        case e =>  print("multi-threaded failed "); e.printStackTrace()
      }
    }

    runTest // trigger
  }
  
  def run { fun73_2() }
}


object Object75_1 {
  
  val fun73_2 = () => {
    @volatile var ObjCounter = 0
    
    object Obj  { ObjCounter += 1}

    def multiThreadedAccess() {
      val threads = for (i <- 1 to 5) yield new Thread(new Runnable {
        def run = Obj
      })

      threads foreach (_.start())
      threads foreach (_.join())
    }

    def runTest {
      try {
        multiThreadedAccess()
        assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
        println("ok")
      } catch {
        case e =>  print("multi-threaded failed "); e.printStackTrace()
      }
    }

    runTest // trigger
  }
  
  def run { fun73_2() } // trigger
}


trait Trait76_1 {
  
  val fun73_2 = () => {
    @volatile var ObjCounter = 0
    
    object Obj  { ObjCounter += 1}

    def multiThreadedAccess() {
      val threads = for (i <- 1 to 5) yield new Thread(new Runnable {
        def run = Obj
      })

      threads foreach (_.start())
      threads foreach (_.join())
    }

    def runTest {
      try {
        multiThreadedAccess()
        assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
        println("ok")
      } catch {
        case e =>  print("multi-threaded failed "); e.printStackTrace()
      }
    }

    runTest // trigger
  }
  
  def run { fun73_2() }
}


class Class78_1 {
  
  class Class77_2 {
    { // in primary constructor 
      @volatile var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}

      def multiThreadedAccess() {
        val threads = for (i <- 1 to 5) yield new Thread(new Runnable {
          def run = Obj
        })

        threads foreach (_.start())
        threads foreach (_.join())
      }

      def runTest {
        try {
          multiThreadedAccess()
          assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
          println("ok")
        } catch {
          case e =>  print("multi-threaded failed "); e.printStackTrace()
        }
      }

      runTest // trigger
    } 
  }
  
  def run { (new Class77_2) }
}


object Object79_1 {
  
  class Class77_2 {
    { // in primary constructor 
      @volatile var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}

      def multiThreadedAccess() {
        val threads = for (i <- 1 to 5) yield new Thread(new Runnable {
          def run = Obj
        })

        threads foreach (_.start())
        threads foreach (_.join())
      }

      def runTest {
        try {
          multiThreadedAccess()
          assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
          println("ok")
        } catch {
          case e =>  print("multi-threaded failed "); e.printStackTrace()
        }
      }

      runTest // trigger
    } 
  }
  
  def run { (new Class77_2) } // trigger
}


trait Trait80_1 {
  
  class Class77_2 {
    { // in primary constructor 
      @volatile var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}

      def multiThreadedAccess() {
        val threads = for (i <- 1 to 5) yield new Thread(new Runnable {
          def run = Obj
        })

        threads foreach (_.start())
        threads foreach (_.join())
      }

      def runTest {
        try {
          multiThreadedAccess()
          assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
          println("ok")
        } catch {
          case e =>  print("multi-threaded failed "); e.printStackTrace()
        }
      }

      runTest // trigger
    } 
  }
  
  def run { (new Class77_2) }
}


class Class82_1 {
  
  trait Trait81_2 {
    { // in primary constructor 
      @volatile var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}

      def multiThreadedAccess() {
        val threads = for (i <- 1 to 5) yield new Thread(new Runnable {
          def run = Obj
        })

        threads foreach (_.start())
        threads foreach (_.join())
      }

      def runTest {
        try {
          multiThreadedAccess()
          assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
          println("ok")
        } catch {
          case e =>  print("multi-threaded failed "); e.printStackTrace()
        }
      }

      runTest // trigger
    } 
  }
  
  def run { (new Trait81_2 {}) }
}


object Object83_1 {
  
  trait Trait81_2 {
    { // in primary constructor 
      @volatile var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}

      def multiThreadedAccess() {
        val threads = for (i <- 1 to 5) yield new Thread(new Runnable {
          def run = Obj
        })

        threads foreach (_.start())
        threads foreach (_.join())
      }

      def runTest {
        try {
          multiThreadedAccess()
          assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
          println("ok")
        } catch {
          case e =>  print("multi-threaded failed "); e.printStackTrace()
        }
      }

      runTest // trigger
    } 
  }
  
  def run { (new Trait81_2 {}) } // trigger
}


trait Trait84_1 {
  
  trait Trait81_2 {
    { // in primary constructor 
      @volatile var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}

      def multiThreadedAccess() {
        val threads = for (i <- 1 to 5) yield new Thread(new Runnable {
          def run = Obj
        })

        threads foreach (_.start())
        threads foreach (_.join())
      }

      def runTest {
        try {
          multiThreadedAccess()
          assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
          println("ok")
        } catch {
          case e =>  print("multi-threaded failed "); e.printStackTrace()
        }
      }

      runTest // trigger
    } 
  }
  
  def run { (new Trait81_2 {}) }
}


class Class90_1 {
  
  val value89_2 = {
    @volatile var ObjCounter = 0
    
    object Obj  { ObjCounter += 1}

    def multiThreadedAccess() {
      val threads = for (i <- 1 to 5) yield new Thread(new Runnable {
        def run = Obj
      })

      threads foreach (_.start())
      threads foreach (_.join())
    }

    def runTest {
      try {
        multiThreadedAccess()
        assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
        println("ok")
      } catch {
        case e =>  print("multi-threaded failed "); e.printStackTrace()
      }
    }

    runTest // trigger
  }
  
  def run { value89_2 }
}


trait Trait92_1 {
  
  val value89_2 = {
    @volatile var ObjCounter = 0
    
    object Obj  { ObjCounter += 1}

    def multiThreadedAccess() {
      val threads = for (i <- 1 to 5) yield new Thread(new Runnable {
        def run = Obj
      })

      threads foreach (_.start())
      threads foreach (_.join())
    }

    def runTest {
      try {
        multiThreadedAccess()
        assert(ObjCounter == 1, "multiple instances: " + ObjCounter)
        println("ok")
      } catch {
        case e =>  print("multi-threaded failed "); e.printStackTrace()
      }
    }

    runTest // trigger
  }
  
  def run { value89_2 }
}


object Test {
  def main(args: Array[String]) {
    (new Class2_1).run
    Object3_1.run
    (new Trait4_1 {}).run
    (new Class6_1).run
    Object7_1.run
    (new Trait8_1 {}).run
    (new Class10_1).run
    Object11_1.run
    (new Trait12_1 {}).run
    (new Class14_1).run
    Object15_1.run
    (new Trait16_1 {}).run
    (new Class18_1).run
    Object19_1.run
    (new Trait20_1 {}).run
    (new Class22_1).run
    Object23_1.run
    (new Trait24_1 {}).run
    (new Class26_1).run
    Object27_1.run
    (new Trait28_1 {}).run
    (new Class30_1).run
    Object31_1.run
    (new Trait32_1 {}).run
    (new Class34_1).run
    Object35_1.run
    (new Trait36_1 {}).run
    (new Class38_1).run
    Object39_1.run
    (new Trait40_1 {}).run
    (new Class42_1).run
    Object43_1.run
    (new Trait44_1 {}).run
    (new Class46_1).run
    Object47_1.run
    (new Trait48_1 {}).run
    (new Class50_1).run
    Object51_1.run
    (new Trait52_1 {}).run
    (new Class54_1).run
    Object55_1.run
    (new Trait56_1 {}).run
    (new Class58_1).run
    Object59_1.run
    (new Trait60_1 {}).run
    (new Class62_1).run
    Object63_1.run
    (new Trait64_1 {}).run
    (new Class66_1).run
    Object67_1.run
    (new Trait68_1 {}).run
    (new Class70_1).run
    Object71_1.run
    (new Trait72_1 {}).run
    (new Class74_1).run
    Object75_1.run
    (new Trait76_1 {}).run
    (new Class78_1).run
    Object79_1.run
    (new Trait80_1 {}).run
    (new Class82_1).run
    Object83_1.run
    (new Trait84_1 {}).run
    (new Class90_1).run
    (new Trait92_1 {}).run
  }
}

