

class Class3_1 {
  
  class Class2_2 {
    
    class Class1_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest }
    }
    
    def run { (new Class1_3).run }
  }
  
  def run { (new Class2_2).run }
}


object Object4_1 {
  
  class Class2_2 {
    
    class Class1_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest }
    }
    
    def run { (new Class1_3).run }
  }
  
  def run { (new Class2_2).run } // trigger
}


trait Trait5_1 {
  
  class Class2_2 {
    
    class Class1_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest }
    }
    
    def run { (new Class1_3).run }
  }
  
  def run { (new Class2_2).run }
}


class Class7_1 {
  
  object Object6_2 {
    
    class Class1_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest }
    }
    
    def run { (new Class1_3).run } // trigger
  }
  
  def run { Object6_2.run }
}


object Object8_1 {
  
  object Object6_2 {
    
    class Class1_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest }
    }
    
    def run { (new Class1_3).run } // trigger
  }
  
  def run { Object6_2.run } // trigger
}


trait Trait9_1 {
  
  object Object6_2 {
    
    class Class1_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest }
    }
    
    def run { (new Class1_3).run } // trigger
  }
  
  def run { Object6_2.run }
}


class Class11_1 {
  
  trait Trait10_2 {
    
    class Class1_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest }
    }
    
    def run { (new Class1_3).run }
  }
  
  def run { (new Trait10_2 {}).run }
}


object Object12_1 {
  
  trait Trait10_2 {
    
    class Class1_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest }
    }
    
    def run { (new Class1_3).run }
  }
  
  def run { (new Trait10_2 {}).run } // trigger
}


trait Trait13_1 {
  
  trait Trait10_2 {
    
    class Class1_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest }
    }
    
    def run { (new Class1_3).run }
  }
  
  def run { (new Trait10_2 {}).run }
}


class Class15_1 {
  
  def method14_2 {
    
    class Class1_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest }
    }
    
    (new Class1_3).run // trigger
  }
  
  def run { method14_2 }
}


object Object16_1 {
  
  def method14_2 {
    
    class Class1_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest }
    }
    
    (new Class1_3).run // trigger
  }
  
  def run { method14_2 } // trigger
}


trait Trait17_1 {
  
  def method14_2 {
    
    class Class1_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest }
    }
    
    (new Class1_3).run // trigger
  }
  
  def run { method14_2 }
}


class Class19_1 {
  
  val fun18_2 = () => {
    
    class Class1_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest }
    }
    
    (new Class1_3).run // trigger
  }
  
  def run { fun18_2() }
}


object Object20_1 {
  
  val fun18_2 = () => {
    
    class Class1_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest }
    }
    
    (new Class1_3).run // trigger
  }
  
  def run { fun18_2() } // trigger
}


trait Trait21_1 {
  
  val fun18_2 = () => {
    
    class Class1_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest }
    }
    
    (new Class1_3).run // trigger
  }
  
  def run { fun18_2() }
}


class Class23_1 {
  
  class Class22_2 {
    { // in primary constructor 
      
      class Class1_3 {
        var ObjCounter = 0
        
        object Obj  { ObjCounter += 1}
        Obj // one

        def singleThreadedAccess(x: Any) = {
          x == Obj
        }

        def runTest {
          try {
            assert(singleThreadedAccess(Obj))
            assert(ObjCounter == 1)
          } catch {
            case e =>  print("failed "); e.printStackTrace()
          }
          println("ok")
        }

        def run { runTest }
      }
      
      (new Class1_3).run // trigger
    } 
  }
  
  def run { (new Class22_2) }
}


object Object24_1 {
  
  class Class22_2 {
    { // in primary constructor 
      
      class Class1_3 {
        var ObjCounter = 0
        
        object Obj  { ObjCounter += 1}
        Obj // one

        def singleThreadedAccess(x: Any) = {
          x == Obj
        }

        def runTest {
          try {
            assert(singleThreadedAccess(Obj))
            assert(ObjCounter == 1)
          } catch {
            case e =>  print("failed "); e.printStackTrace()
          }
          println("ok")
        }

        def run { runTest }
      }
      
      (new Class1_3).run // trigger
    } 
  }
  
  def run { (new Class22_2) } // trigger
}


trait Trait25_1 {
  
  class Class22_2 {
    { // in primary constructor 
      
      class Class1_3 {
        var ObjCounter = 0
        
        object Obj  { ObjCounter += 1}
        Obj // one

        def singleThreadedAccess(x: Any) = {
          x == Obj
        }

        def runTest {
          try {
            assert(singleThreadedAccess(Obj))
            assert(ObjCounter == 1)
          } catch {
            case e =>  print("failed "); e.printStackTrace()
          }
          println("ok")
        }

        def run { runTest }
      }
      
      (new Class1_3).run // trigger
    } 
  }
  
  def run { (new Class22_2) }
}


class Class27_1 {
  
  lazy val lzvalue26_2 = {
    
    class Class1_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest }
    }
    
    (new Class1_3).run // trigger
  }
  
  def run { lzvalue26_2 }
}


object Object28_1 {
  
  lazy val lzvalue26_2 = {
    
    class Class1_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest }
    }
    
    (new Class1_3).run // trigger
  }
  
  def run { lzvalue26_2 } // trigger
}


trait Trait29_1 {
  
  lazy val lzvalue26_2 = {
    
    class Class1_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest }
    }
    
    (new Class1_3).run // trigger
  }
  
  def run { lzvalue26_2 }
}


class Class31_1 {
  
  val value30_2 = {
    
    class Class1_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest }
    }
    
    (new Class1_3).run // trigger
  }
  
  def run { value30_2 }
}


object Object32_1 {
  
  val value30_2 = {
    
    class Class1_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest }
    }
    
    (new Class1_3).run // trigger
  }
  
  def run { value30_2 } // trigger
}


trait Trait33_1 {
  
  val value30_2 = {
    
    class Class1_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest }
    }
    
    (new Class1_3).run // trigger
  }
  
  def run { value30_2 }
}


class Class36_1 {
  
  class Class35_2 {
    
    object Object34_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest } // trigger
    }
    
    def run { Object34_3.run }
  }
  
  def run { (new Class35_2).run }
}


object Object37_1 {
  
  class Class35_2 {
    
    object Object34_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest } // trigger
    }
    
    def run { Object34_3.run }
  }
  
  def run { (new Class35_2).run } // trigger
}


trait Trait38_1 {
  
  class Class35_2 {
    
    object Object34_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest } // trigger
    }
    
    def run { Object34_3.run }
  }
  
  def run { (new Class35_2).run }
}


class Class40_1 {
  
  object Object39_2 {
    
    object Object34_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest } // trigger
    }
    
    def run { Object34_3.run } // trigger
  }
  
  def run { Object39_2.run }
}


object Object41_1 {
  
  object Object39_2 {
    
    object Object34_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest } // trigger
    }
    
    def run { Object34_3.run } // trigger
  }
  
  def run { Object39_2.run } // trigger
}


trait Trait42_1 {
  
  object Object39_2 {
    
    object Object34_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest } // trigger
    }
    
    def run { Object34_3.run } // trigger
  }
  
  def run { Object39_2.run }
}


class Class44_1 {
  
  trait Trait43_2 {
    
    object Object34_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest } // trigger
    }
    
    def run { Object34_3.run }
  }
  
  def run { (new Trait43_2 {}).run }
}


object Object45_1 {
  
  trait Trait43_2 {
    
    object Object34_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest } // trigger
    }
    
    def run { Object34_3.run }
  }
  
  def run { (new Trait43_2 {}).run } // trigger
}


trait Trait46_1 {
  
  trait Trait43_2 {
    
    object Object34_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest } // trigger
    }
    
    def run { Object34_3.run }
  }
  
  def run { (new Trait43_2 {}).run }
}


class Class48_1 {
  
  def method47_2 {
    
    object Object34_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest } // trigger
    }
    
    Object34_3.run // trigger
  }
  
  def run { method47_2 }
}


object Object49_1 {
  
  def method47_2 {
    
    object Object34_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest } // trigger
    }
    
    Object34_3.run // trigger
  }
  
  def run { method47_2 } // trigger
}


trait Trait50_1 {
  
  def method47_2 {
    
    object Object34_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest } // trigger
    }
    
    Object34_3.run // trigger
  }
  
  def run { method47_2 }
}


class Class52_1 {
  
  val fun51_2 = () => {
    
    object Object34_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest } // trigger
    }
    
    Object34_3.run // trigger
  }
  
  def run { fun51_2() }
}


object Object53_1 {
  
  val fun51_2 = () => {
    
    object Object34_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest } // trigger
    }
    
    Object34_3.run // trigger
  }
  
  def run { fun51_2() } // trigger
}


trait Trait54_1 {
  
  val fun51_2 = () => {
    
    object Object34_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest } // trigger
    }
    
    Object34_3.run // trigger
  }
  
  def run { fun51_2() }
}


class Class56_1 {
  
  class Class55_2 {
    { // in primary constructor 
      
      object Object34_3 {
        var ObjCounter = 0
        
        object Obj  { ObjCounter += 1}
        Obj // one

        def singleThreadedAccess(x: Any) = {
          x == Obj
        }

        def runTest {
          try {
            assert(singleThreadedAccess(Obj))
            assert(ObjCounter == 1)
          } catch {
            case e =>  print("failed "); e.printStackTrace()
          }
          println("ok")
        }

        def run { runTest } // trigger
      }
      
      Object34_3.run // trigger
    } 
  }
  
  def run { (new Class55_2) }
}


object Object57_1 {
  
  class Class55_2 {
    { // in primary constructor 
      
      object Object34_3 {
        var ObjCounter = 0
        
        object Obj  { ObjCounter += 1}
        Obj // one

        def singleThreadedAccess(x: Any) = {
          x == Obj
        }

        def runTest {
          try {
            assert(singleThreadedAccess(Obj))
            assert(ObjCounter == 1)
          } catch {
            case e =>  print("failed "); e.printStackTrace()
          }
          println("ok")
        }

        def run { runTest } // trigger
      }
      
      Object34_3.run // trigger
    } 
  }
  
  def run { (new Class55_2) } // trigger
}


trait Trait58_1 {
  
  class Class55_2 {
    { // in primary constructor 
      
      object Object34_3 {
        var ObjCounter = 0
        
        object Obj  { ObjCounter += 1}
        Obj // one

        def singleThreadedAccess(x: Any) = {
          x == Obj
        }

        def runTest {
          try {
            assert(singleThreadedAccess(Obj))
            assert(ObjCounter == 1)
          } catch {
            case e =>  print("failed "); e.printStackTrace()
          }
          println("ok")
        }

        def run { runTest } // trigger
      }
      
      Object34_3.run // trigger
    } 
  }
  
  def run { (new Class55_2) }
}


class Class60_1 {
  
  lazy val lzvalue59_2 = {
    
    object Object34_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest } // trigger
    }
    
    Object34_3.run // trigger
  }
  
  def run { lzvalue59_2 }
}


object Object61_1 {
  
  lazy val lzvalue59_2 = {
    
    object Object34_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest } // trigger
    }
    
    Object34_3.run // trigger
  }
  
  def run { lzvalue59_2 } // trigger
}


trait Trait62_1 {
  
  lazy val lzvalue59_2 = {
    
    object Object34_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest } // trigger
    }
    
    Object34_3.run // trigger
  }
  
  def run { lzvalue59_2 }
}


class Class64_1 {
  
  val value63_2 = {
    
    object Object34_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest } // trigger
    }
    
    Object34_3.run // trigger
  }
  
  def run { value63_2 }
}


object Object65_1 {
  
  val value63_2 = {
    
    object Object34_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest } // trigger
    }
    
    Object34_3.run // trigger
  }
  
  def run { value63_2 } // trigger
}


trait Trait66_1 {
  
  val value63_2 = {
    
    object Object34_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest } // trigger
    }
    
    Object34_3.run // trigger
  }
  
  def run { value63_2 }
}


class Class69_1 {
  
  class Class68_2 {
    
    trait Trait67_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest }
    }
    
    def run { (new Trait67_3 {}).run }
  }
  
  def run { (new Class68_2).run }
}


object Object70_1 {
  
  class Class68_2 {
    
    trait Trait67_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest }
    }
    
    def run { (new Trait67_3 {}).run }
  }
  
  def run { (new Class68_2).run } // trigger
}


trait Trait71_1 {
  
  class Class68_2 {
    
    trait Trait67_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest }
    }
    
    def run { (new Trait67_3 {}).run }
  }
  
  def run { (new Class68_2).run }
}


class Class73_1 {
  
  object Object72_2 {
    
    trait Trait67_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest }
    }
    
    def run { (new Trait67_3 {}).run } // trigger
  }
  
  def run { Object72_2.run }
}


object Object74_1 {
  
  object Object72_2 {
    
    trait Trait67_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest }
    }
    
    def run { (new Trait67_3 {}).run } // trigger
  }
  
  def run { Object72_2.run } // trigger
}


trait Trait75_1 {
  
  object Object72_2 {
    
    trait Trait67_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest }
    }
    
    def run { (new Trait67_3 {}).run } // trigger
  }
  
  def run { Object72_2.run }
}


class Class77_1 {
  
  trait Trait76_2 {
    
    trait Trait67_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest }
    }
    
    def run { (new Trait67_3 {}).run }
  }
  
  def run { (new Trait76_2 {}).run }
}


object Object78_1 {
  
  trait Trait76_2 {
    
    trait Trait67_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest }
    }
    
    def run { (new Trait67_3 {}).run }
  }
  
  def run { (new Trait76_2 {}).run } // trigger
}


trait Trait79_1 {
  
  trait Trait76_2 {
    
    trait Trait67_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest }
    }
    
    def run { (new Trait67_3 {}).run }
  }
  
  def run { (new Trait76_2 {}).run }
}


class Class81_1 {
  
  def method80_2 {
    
    trait Trait67_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest }
    }
    
    (new Trait67_3 {}).run // trigger
  }
  
  def run { method80_2 }
}


object Object82_1 {
  
  def method80_2 {
    
    trait Trait67_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest }
    }
    
    (new Trait67_3 {}).run // trigger
  }
  
  def run { method80_2 } // trigger
}


trait Trait83_1 {
  
  def method80_2 {
    
    trait Trait67_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest }
    }
    
    (new Trait67_3 {}).run // trigger
  }
  
  def run { method80_2 }
}


class Class85_1 {
  
  val fun84_2 = () => {
    
    trait Trait67_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest }
    }
    
    (new Trait67_3 {}).run // trigger
  }
  
  def run { fun84_2() }
}


object Object86_1 {
  
  val fun84_2 = () => {
    
    trait Trait67_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest }
    }
    
    (new Trait67_3 {}).run // trigger
  }
  
  def run { fun84_2() } // trigger
}


trait Trait87_1 {
  
  val fun84_2 = () => {
    
    trait Trait67_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest }
    }
    
    (new Trait67_3 {}).run // trigger
  }
  
  def run { fun84_2() }
}


class Class89_1 {
  
  class Class88_2 {
    { // in primary constructor 
      
      trait Trait67_3 {
        var ObjCounter = 0
        
        object Obj  { ObjCounter += 1}
        Obj // one

        def singleThreadedAccess(x: Any) = {
          x == Obj
        }

        def runTest {
          try {
            assert(singleThreadedAccess(Obj))
            assert(ObjCounter == 1)
          } catch {
            case e =>  print("failed "); e.printStackTrace()
          }
          println("ok")
        }

        def run { runTest }
      }
      
      (new Trait67_3 {}).run // trigger
    } 
  }
  
  def run { (new Class88_2) }
}


object Object90_1 {
  
  class Class88_2 {
    { // in primary constructor 
      
      trait Trait67_3 {
        var ObjCounter = 0
        
        object Obj  { ObjCounter += 1}
        Obj // one

        def singleThreadedAccess(x: Any) = {
          x == Obj
        }

        def runTest {
          try {
            assert(singleThreadedAccess(Obj))
            assert(ObjCounter == 1)
          } catch {
            case e =>  print("failed "); e.printStackTrace()
          }
          println("ok")
        }

        def run { runTest }
      }
      
      (new Trait67_3 {}).run // trigger
    } 
  }
  
  def run { (new Class88_2) } // trigger
}


trait Trait91_1 {
  
  class Class88_2 {
    { // in primary constructor 
      
      trait Trait67_3 {
        var ObjCounter = 0
        
        object Obj  { ObjCounter += 1}
        Obj // one

        def singleThreadedAccess(x: Any) = {
          x == Obj
        }

        def runTest {
          try {
            assert(singleThreadedAccess(Obj))
            assert(ObjCounter == 1)
          } catch {
            case e =>  print("failed "); e.printStackTrace()
          }
          println("ok")
        }

        def run { runTest }
      }
      
      (new Trait67_3 {}).run // trigger
    } 
  }
  
  def run { (new Class88_2) }
}


class Class93_1 {
  
  lazy val lzvalue92_2 = {
    
    trait Trait67_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest }
    }
    
    (new Trait67_3 {}).run // trigger
  }
  
  def run { lzvalue92_2 }
}


object Object94_1 {
  
  lazy val lzvalue92_2 = {
    
    trait Trait67_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest }
    }
    
    (new Trait67_3 {}).run // trigger
  }
  
  def run { lzvalue92_2 } // trigger
}


trait Trait95_1 {
  
  lazy val lzvalue92_2 = {
    
    trait Trait67_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest }
    }
    
    (new Trait67_3 {}).run // trigger
  }
  
  def run { lzvalue92_2 }
}


class Class97_1 {
  
  val value96_2 = {
    
    trait Trait67_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest }
    }
    
    (new Trait67_3 {}).run // trigger
  }
  
  def run { value96_2 }
}


object Object98_1 {
  
  val value96_2 = {
    
    trait Trait67_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest }
    }
    
    (new Trait67_3 {}).run // trigger
  }
  
  def run { value96_2 } // trigger
}


trait Trait99_1 {
  
  val value96_2 = {
    
    trait Trait67_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      def run { runTest }
    }
    
    (new Trait67_3 {}).run // trigger
  }
  
  def run { value96_2 }
}


class Class102_1 {
  
  class Class101_2 {
    
    def method100_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    def run { method100_3 }
  }
  
  def run { (new Class101_2).run }
}


object Object103_1 {
  
  class Class101_2 {
    
    def method100_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    def run { method100_3 }
  }
  
  def run { (new Class101_2).run } // trigger
}


trait Trait104_1 {
  
  class Class101_2 {
    
    def method100_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    def run { method100_3 }
  }
  
  def run { (new Class101_2).run }
}


class Class106_1 {
  
  object Object105_2 {
    
    def method100_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    def run { method100_3 } // trigger
  }
  
  def run { Object105_2.run }
}


object Object107_1 {
  
  object Object105_2 {
    
    def method100_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    def run { method100_3 } // trigger
  }
  
  def run { Object105_2.run } // trigger
}


trait Trait108_1 {
  
  object Object105_2 {
    
    def method100_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    def run { method100_3 } // trigger
  }
  
  def run { Object105_2.run }
}


class Class110_1 {
  
  trait Trait109_2 {
    
    def method100_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    def run { method100_3 }
  }
  
  def run { (new Trait109_2 {}).run }
}


object Object111_1 {
  
  trait Trait109_2 {
    
    def method100_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    def run { method100_3 }
  }
  
  def run { (new Trait109_2 {}).run } // trigger
}


trait Trait112_1 {
  
  trait Trait109_2 {
    
    def method100_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    def run { method100_3 }
  }
  
  def run { (new Trait109_2 {}).run }
}


class Class114_1 {
  
  def method113_2 {
    
    def method100_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    method100_3 // trigger
  }
  
  def run { method113_2 }
}


object Object115_1 {
  
  def method113_2 {
    
    def method100_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    method100_3 // trigger
  }
  
  def run { method113_2 } // trigger
}


trait Trait116_1 {
  
  def method113_2 {
    
    def method100_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    method100_3 // trigger
  }
  
  def run { method113_2 }
}


class Class118_1 {
  
  val fun117_2 = () => {
    
    def method100_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    method100_3 // trigger
  }
  
  def run { fun117_2() }
}


object Object119_1 {
  
  val fun117_2 = () => {
    
    def method100_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    method100_3 // trigger
  }
  
  def run { fun117_2() } // trigger
}


trait Trait120_1 {
  
  val fun117_2 = () => {
    
    def method100_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    method100_3 // trigger
  }
  
  def run { fun117_2() }
}


class Class122_1 {
  
  class Class121_2 {
    { // in primary constructor 
      
      def method100_3 {
        var ObjCounter = 0
        
        object Obj  { ObjCounter += 1}
        Obj // one

        def singleThreadedAccess(x: Any) = {
          x == Obj
        }

        def runTest {
          try {
            assert(singleThreadedAccess(Obj))
            assert(ObjCounter == 1)
          } catch {
            case e =>  print("failed "); e.printStackTrace()
          }
          println("ok")
        }

        runTest // trigger
      }
      
      method100_3 // trigger
    } 
  }
  
  def run { (new Class121_2) }
}


object Object123_1 {
  
  class Class121_2 {
    { // in primary constructor 
      
      def method100_3 {
        var ObjCounter = 0
        
        object Obj  { ObjCounter += 1}
        Obj // one

        def singleThreadedAccess(x: Any) = {
          x == Obj
        }

        def runTest {
          try {
            assert(singleThreadedAccess(Obj))
            assert(ObjCounter == 1)
          } catch {
            case e =>  print("failed "); e.printStackTrace()
          }
          println("ok")
        }

        runTest // trigger
      }
      
      method100_3 // trigger
    } 
  }
  
  def run { (new Class121_2) } // trigger
}


trait Trait124_1 {
  
  class Class121_2 {
    { // in primary constructor 
      
      def method100_3 {
        var ObjCounter = 0
        
        object Obj  { ObjCounter += 1}
        Obj // one

        def singleThreadedAccess(x: Any) = {
          x == Obj
        }

        def runTest {
          try {
            assert(singleThreadedAccess(Obj))
            assert(ObjCounter == 1)
          } catch {
            case e =>  print("failed "); e.printStackTrace()
          }
          println("ok")
        }

        runTest // trigger
      }
      
      method100_3 // trigger
    } 
  }
  
  def run { (new Class121_2) }
}


class Class126_1 {
  
  lazy val lzvalue125_2 = {
    
    def method100_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    method100_3 // trigger
  }
  
  def run { lzvalue125_2 }
}


object Object127_1 {
  
  lazy val lzvalue125_2 = {
    
    def method100_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    method100_3 // trigger
  }
  
  def run { lzvalue125_2 } // trigger
}


trait Trait128_1 {
  
  lazy val lzvalue125_2 = {
    
    def method100_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    method100_3 // trigger
  }
  
  def run { lzvalue125_2 }
}


class Class130_1 {
  
  val value129_2 = {
    
    def method100_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    method100_3 // trigger
  }
  
  def run { value129_2 }
}


object Object131_1 {
  
  val value129_2 = {
    
    def method100_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    method100_3 // trigger
  }
  
  def run { value129_2 } // trigger
}


trait Trait132_1 {
  
  val value129_2 = {
    
    def method100_3 {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    method100_3 // trigger
  }
  
  def run { value129_2 }
}


class Class135_1 {
  
  class Class134_2 {
    
    val fun133_3 = () => {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    def run { fun133_3() }
  }
  
  def run { (new Class134_2).run }
}


object Object136_1 {
  
  class Class134_2 {
    
    val fun133_3 = () => {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    def run { fun133_3() }
  }
  
  def run { (new Class134_2).run } // trigger
}


trait Trait137_1 {
  
  class Class134_2 {
    
    val fun133_3 = () => {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    def run { fun133_3() }
  }
  
  def run { (new Class134_2).run }
}


class Class139_1 {
  
  object Object138_2 {
    
    val fun133_3 = () => {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    def run { fun133_3() } // trigger
  }
  
  def run { Object138_2.run }
}


object Object140_1 {
  
  object Object138_2 {
    
    val fun133_3 = () => {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    def run { fun133_3() } // trigger
  }
  
  def run { Object138_2.run } // trigger
}


trait Trait141_1 {
  
  object Object138_2 {
    
    val fun133_3 = () => {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    def run { fun133_3() } // trigger
  }
  
  def run { Object138_2.run }
}


class Class143_1 {
  
  trait Trait142_2 {
    
    val fun133_3 = () => {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    def run { fun133_3() }
  }
  
  def run { (new Trait142_2 {}).run }
}


object Object144_1 {
  
  trait Trait142_2 {
    
    val fun133_3 = () => {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    def run { fun133_3() }
  }
  
  def run { (new Trait142_2 {}).run } // trigger
}


trait Trait145_1 {
  
  trait Trait142_2 {
    
    val fun133_3 = () => {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    def run { fun133_3() }
  }
  
  def run { (new Trait142_2 {}).run }
}


class Class147_1 {
  
  def method146_2 {
    
    val fun133_3 = () => {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    fun133_3() // trigger
  }
  
  def run { method146_2 }
}


object Object148_1 {
  
  def method146_2 {
    
    val fun133_3 = () => {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    fun133_3() // trigger
  }
  
  def run { method146_2 } // trigger
}


trait Trait149_1 {
  
  def method146_2 {
    
    val fun133_3 = () => {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    fun133_3() // trigger
  }
  
  def run { method146_2 }
}


class Class151_1 {
  
  val fun150_2 = () => {
    
    val fun133_3 = () => {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    fun133_3() // trigger
  }
  
  def run { fun150_2() }
}


object Object152_1 {
  
  val fun150_2 = () => {
    
    val fun133_3 = () => {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    fun133_3() // trigger
  }
  
  def run { fun150_2() } // trigger
}


trait Trait153_1 {
  
  val fun150_2 = () => {
    
    val fun133_3 = () => {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    fun133_3() // trigger
  }
  
  def run { fun150_2() }
}


class Class155_1 {
  
  class Class154_2 {
    { // in primary constructor 
      
      val fun133_3 = () => {
        var ObjCounter = 0
        
        object Obj  { ObjCounter += 1}
        Obj // one

        def singleThreadedAccess(x: Any) = {
          x == Obj
        }

        def runTest {
          try {
            assert(singleThreadedAccess(Obj))
            assert(ObjCounter == 1)
          } catch {
            case e =>  print("failed "); e.printStackTrace()
          }
          println("ok")
        }

        runTest // trigger
      }
      
      fun133_3() // trigger
    } 
  }
  
  def run { (new Class154_2) }
}


object Object156_1 {
  
  class Class154_2 {
    { // in primary constructor 
      
      val fun133_3 = () => {
        var ObjCounter = 0
        
        object Obj  { ObjCounter += 1}
        Obj // one

        def singleThreadedAccess(x: Any) = {
          x == Obj
        }

        def runTest {
          try {
            assert(singleThreadedAccess(Obj))
            assert(ObjCounter == 1)
          } catch {
            case e =>  print("failed "); e.printStackTrace()
          }
          println("ok")
        }

        runTest // trigger
      }
      
      fun133_3() // trigger
    } 
  }
  
  def run { (new Class154_2) } // trigger
}


trait Trait157_1 {
  
  class Class154_2 {
    { // in primary constructor 
      
      val fun133_3 = () => {
        var ObjCounter = 0
        
        object Obj  { ObjCounter += 1}
        Obj // one

        def singleThreadedAccess(x: Any) = {
          x == Obj
        }

        def runTest {
          try {
            assert(singleThreadedAccess(Obj))
            assert(ObjCounter == 1)
          } catch {
            case e =>  print("failed "); e.printStackTrace()
          }
          println("ok")
        }

        runTest // trigger
      }
      
      fun133_3() // trigger
    } 
  }
  
  def run { (new Class154_2) }
}


class Class159_1 {
  
  lazy val lzvalue158_2 = {
    
    val fun133_3 = () => {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    fun133_3() // trigger
  }
  
  def run { lzvalue158_2 }
}


object Object160_1 {
  
  lazy val lzvalue158_2 = {
    
    val fun133_3 = () => {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    fun133_3() // trigger
  }
  
  def run { lzvalue158_2 } // trigger
}


trait Trait161_1 {
  
  lazy val lzvalue158_2 = {
    
    val fun133_3 = () => {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    fun133_3() // trigger
  }
  
  def run { lzvalue158_2 }
}


class Class163_1 {
  
  val value162_2 = {
    
    val fun133_3 = () => {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    fun133_3() // trigger
  }
  
  def run { value162_2 }
}


object Object164_1 {
  
  val value162_2 = {
    
    val fun133_3 = () => {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    fun133_3() // trigger
  }
  
  def run { value162_2 } // trigger
}


trait Trait165_1 {
  
  val value162_2 = {
    
    val fun133_3 = () => {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    fun133_3() // trigger
  }
  
  def run { value162_2 }
}


class Class168_1 {
  
  class Class167_2 {
    
    class Class166_3 {
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
            assert(ObjCounter == 1)
          } catch {
            case e =>  print("failed "); e.printStackTrace()
          }
          println("ok")
        }

        runTest // trigger
      } 
    }
    
    def run { (new Class166_3) }
  }
  
  def run { (new Class167_2).run }
}


object Object169_1 {
  
  class Class167_2 {
    
    class Class166_3 {
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
            assert(ObjCounter == 1)
          } catch {
            case e =>  print("failed "); e.printStackTrace()
          }
          println("ok")
        }

        runTest // trigger
      } 
    }
    
    def run { (new Class166_3) }
  }
  
  def run { (new Class167_2).run } // trigger
}


trait Trait170_1 {
  
  class Class167_2 {
    
    class Class166_3 {
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
            assert(ObjCounter == 1)
          } catch {
            case e =>  print("failed "); e.printStackTrace()
          }
          println("ok")
        }

        runTest // trigger
      } 
    }
    
    def run { (new Class166_3) }
  }
  
  def run { (new Class167_2).run }
}


class Class172_1 {
  
  object Object171_2 {
    
    class Class166_3 {
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
            assert(ObjCounter == 1)
          } catch {
            case e =>  print("failed "); e.printStackTrace()
          }
          println("ok")
        }

        runTest // trigger
      } 
    }
    
    def run { (new Class166_3) } // trigger
  }
  
  def run { Object171_2.run }
}


object Object173_1 {
  
  object Object171_2 {
    
    class Class166_3 {
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
            assert(ObjCounter == 1)
          } catch {
            case e =>  print("failed "); e.printStackTrace()
          }
          println("ok")
        }

        runTest // trigger
      } 
    }
    
    def run { (new Class166_3) } // trigger
  }
  
  def run { Object171_2.run } // trigger
}


trait Trait174_1 {
  
  object Object171_2 {
    
    class Class166_3 {
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
            assert(ObjCounter == 1)
          } catch {
            case e =>  print("failed "); e.printStackTrace()
          }
          println("ok")
        }

        runTest // trigger
      } 
    }
    
    def run { (new Class166_3) } // trigger
  }
  
  def run { Object171_2.run }
}


class Class176_1 {
  
  trait Trait175_2 {
    
    class Class166_3 {
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
            assert(ObjCounter == 1)
          } catch {
            case e =>  print("failed "); e.printStackTrace()
          }
          println("ok")
        }

        runTest // trigger
      } 
    }
    
    def run { (new Class166_3) }
  }
  
  def run { (new Trait175_2 {}).run }
}


object Object177_1 {
  
  trait Trait175_2 {
    
    class Class166_3 {
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
            assert(ObjCounter == 1)
          } catch {
            case e =>  print("failed "); e.printStackTrace()
          }
          println("ok")
        }

        runTest // trigger
      } 
    }
    
    def run { (new Class166_3) }
  }
  
  def run { (new Trait175_2 {}).run } // trigger
}


trait Trait178_1 {
  
  trait Trait175_2 {
    
    class Class166_3 {
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
            assert(ObjCounter == 1)
          } catch {
            case e =>  print("failed "); e.printStackTrace()
          }
          println("ok")
        }

        runTest // trigger
      } 
    }
    
    def run { (new Class166_3) }
  }
  
  def run { (new Trait175_2 {}).run }
}


class Class180_1 {
  
  def method179_2 {
    
    class Class166_3 {
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
            assert(ObjCounter == 1)
          } catch {
            case e =>  print("failed "); e.printStackTrace()
          }
          println("ok")
        }

        runTest // trigger
      } 
    }
    
    (new Class166_3) // trigger
  }
  
  def run { method179_2 }
}


object Object181_1 {
  
  def method179_2 {
    
    class Class166_3 {
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
            assert(ObjCounter == 1)
          } catch {
            case e =>  print("failed "); e.printStackTrace()
          }
          println("ok")
        }

        runTest // trigger
      } 
    }
    
    (new Class166_3) // trigger
  }
  
  def run { method179_2 } // trigger
}


trait Trait182_1 {
  
  def method179_2 {
    
    class Class166_3 {
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
            assert(ObjCounter == 1)
          } catch {
            case e =>  print("failed "); e.printStackTrace()
          }
          println("ok")
        }

        runTest // trigger
      } 
    }
    
    (new Class166_3) // trigger
  }
  
  def run { method179_2 }
}


class Class184_1 {
  
  val fun183_2 = () => {
    
    class Class166_3 {
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
            assert(ObjCounter == 1)
          } catch {
            case e =>  print("failed "); e.printStackTrace()
          }
          println("ok")
        }

        runTest // trigger
      } 
    }
    
    (new Class166_3) // trigger
  }
  
  def run { fun183_2() }
}


object Object185_1 {
  
  val fun183_2 = () => {
    
    class Class166_3 {
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
            assert(ObjCounter == 1)
          } catch {
            case e =>  print("failed "); e.printStackTrace()
          }
          println("ok")
        }

        runTest // trigger
      } 
    }
    
    (new Class166_3) // trigger
  }
  
  def run { fun183_2() } // trigger
}


trait Trait186_1 {
  
  val fun183_2 = () => {
    
    class Class166_3 {
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
            assert(ObjCounter == 1)
          } catch {
            case e =>  print("failed "); e.printStackTrace()
          }
          println("ok")
        }

        runTest // trigger
      } 
    }
    
    (new Class166_3) // trigger
  }
  
  def run { fun183_2() }
}


class Class188_1 {
  
  class Class187_2 {
    { // in primary constructor 
      
      class Class166_3 {
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
              assert(ObjCounter == 1)
            } catch {
              case e =>  print("failed "); e.printStackTrace()
            }
            println("ok")
          }

          runTest // trigger
        } 
      }
      
      (new Class166_3) // trigger
    } 
  }
  
  def run { (new Class187_2) }
}


object Object189_1 {
  
  class Class187_2 {
    { // in primary constructor 
      
      class Class166_3 {
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
              assert(ObjCounter == 1)
            } catch {
              case e =>  print("failed "); e.printStackTrace()
            }
            println("ok")
          }

          runTest // trigger
        } 
      }
      
      (new Class166_3) // trigger
    } 
  }
  
  def run { (new Class187_2) } // trigger
}


trait Trait190_1 {
  
  class Class187_2 {
    { // in primary constructor 
      
      class Class166_3 {
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
              assert(ObjCounter == 1)
            } catch {
              case e =>  print("failed "); e.printStackTrace()
            }
            println("ok")
          }

          runTest // trigger
        } 
      }
      
      (new Class166_3) // trigger
    } 
  }
  
  def run { (new Class187_2) }
}


class Class192_1 {
  
  lazy val lzvalue191_2 = {
    
    class Class166_3 {
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
            assert(ObjCounter == 1)
          } catch {
            case e =>  print("failed "); e.printStackTrace()
          }
          println("ok")
        }

        runTest // trigger
      } 
    }
    
    (new Class166_3) // trigger
  }
  
  def run { lzvalue191_2 }
}


object Object193_1 {
  
  lazy val lzvalue191_2 = {
    
    class Class166_3 {
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
            assert(ObjCounter == 1)
          } catch {
            case e =>  print("failed "); e.printStackTrace()
          }
          println("ok")
        }

        runTest // trigger
      } 
    }
    
    (new Class166_3) // trigger
  }
  
  def run { lzvalue191_2 } // trigger
}


trait Trait194_1 {
  
  lazy val lzvalue191_2 = {
    
    class Class166_3 {
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
            assert(ObjCounter == 1)
          } catch {
            case e =>  print("failed "); e.printStackTrace()
          }
          println("ok")
        }

        runTest // trigger
      } 
    }
    
    (new Class166_3) // trigger
  }
  
  def run { lzvalue191_2 }
}


class Class196_1 {
  
  val value195_2 = {
    
    class Class166_3 {
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
            assert(ObjCounter == 1)
          } catch {
            case e =>  print("failed "); e.printStackTrace()
          }
          println("ok")
        }

        runTest // trigger
      } 
    }
    
    (new Class166_3) // trigger
  }
  
  def run { value195_2 }
}


object Object197_1 {
  
  val value195_2 = {
    
    class Class166_3 {
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
            assert(ObjCounter == 1)
          } catch {
            case e =>  print("failed "); e.printStackTrace()
          }
          println("ok")
        }

        runTest // trigger
      } 
    }
    
    (new Class166_3) // trigger
  }
  
  def run { value195_2 } // trigger
}


trait Trait198_1 {
  
  val value195_2 = {
    
    class Class166_3 {
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
            assert(ObjCounter == 1)
          } catch {
            case e =>  print("failed "); e.printStackTrace()
          }
          println("ok")
        }

        runTest // trigger
      } 
    }
    
    (new Class166_3) // trigger
  }
  
  def run { value195_2 }
}


class Class201_1 {
  
  class Class200_2 {
    
    lazy val lzvalue199_3 = {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    def run { lzvalue199_3 }
  }
  
  def run { (new Class200_2).run }
}


object Object202_1 {
  
  class Class200_2 {
    
    lazy val lzvalue199_3 = {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    def run { lzvalue199_3 }
  }
  
  def run { (new Class200_2).run } // trigger
}


trait Trait203_1 {
  
  class Class200_2 {
    
    lazy val lzvalue199_3 = {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    def run { lzvalue199_3 }
  }
  
  def run { (new Class200_2).run }
}


class Class205_1 {
  
  object Object204_2 {
    
    lazy val lzvalue199_3 = {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    def run { lzvalue199_3 } // trigger
  }
  
  def run { Object204_2.run }
}


object Object206_1 {
  
  object Object204_2 {
    
    lazy val lzvalue199_3 = {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    def run { lzvalue199_3 } // trigger
  }
  
  def run { Object204_2.run } // trigger
}


trait Trait207_1 {
  
  object Object204_2 {
    
    lazy val lzvalue199_3 = {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    def run { lzvalue199_3 } // trigger
  }
  
  def run { Object204_2.run }
}


class Class209_1 {
  
  trait Trait208_2 {
    
    lazy val lzvalue199_3 = {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    def run { lzvalue199_3 }
  }
  
  def run { (new Trait208_2 {}).run }
}


object Object210_1 {
  
  trait Trait208_2 {
    
    lazy val lzvalue199_3 = {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    def run { lzvalue199_3 }
  }
  
  def run { (new Trait208_2 {}).run } // trigger
}


trait Trait211_1 {
  
  trait Trait208_2 {
    
    lazy val lzvalue199_3 = {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    def run { lzvalue199_3 }
  }
  
  def run { (new Trait208_2 {}).run }
}


class Class213_1 {
  
  def method212_2 {
    
    lazy val lzvalue199_3 = {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    lzvalue199_3 // trigger
  }
  
  def run { method212_2 }
}


object Object214_1 {
  
  def method212_2 {
    
    lazy val lzvalue199_3 = {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    lzvalue199_3 // trigger
  }
  
  def run { method212_2 } // trigger
}


trait Trait215_1 {
  
  def method212_2 {
    
    lazy val lzvalue199_3 = {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    lzvalue199_3 // trigger
  }
  
  def run { method212_2 }
}


class Class217_1 {
  
  val fun216_2 = () => {
    
    lazy val lzvalue199_3 = {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    lzvalue199_3 // trigger
  }
  
  def run { fun216_2() }
}


object Object218_1 {
  
  val fun216_2 = () => {
    
    lazy val lzvalue199_3 = {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    lzvalue199_3 // trigger
  }
  
  def run { fun216_2() } // trigger
}


trait Trait219_1 {
  
  val fun216_2 = () => {
    
    lazy val lzvalue199_3 = {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    lzvalue199_3 // trigger
  }
  
  def run { fun216_2() }
}


class Class221_1 {
  
  class Class220_2 {
    { // in primary constructor 
      
      lazy val lzvalue199_3 = {
        var ObjCounter = 0
        
        object Obj  { ObjCounter += 1}
        Obj // one

        def singleThreadedAccess(x: Any) = {
          x == Obj
        }

        def runTest {
          try {
            assert(singleThreadedAccess(Obj))
            assert(ObjCounter == 1)
          } catch {
            case e =>  print("failed "); e.printStackTrace()
          }
          println("ok")
        }

        runTest // trigger
      }
      
      lzvalue199_3 // trigger
    } 
  }
  
  def run { (new Class220_2) }
}


object Object222_1 {
  
  class Class220_2 {
    { // in primary constructor 
      
      lazy val lzvalue199_3 = {
        var ObjCounter = 0
        
        object Obj  { ObjCounter += 1}
        Obj // one

        def singleThreadedAccess(x: Any) = {
          x == Obj
        }

        def runTest {
          try {
            assert(singleThreadedAccess(Obj))
            assert(ObjCounter == 1)
          } catch {
            case e =>  print("failed "); e.printStackTrace()
          }
          println("ok")
        }

        runTest // trigger
      }
      
      lzvalue199_3 // trigger
    } 
  }
  
  def run { (new Class220_2) } // trigger
}


trait Trait223_1 {
  
  class Class220_2 {
    { // in primary constructor 
      
      lazy val lzvalue199_3 = {
        var ObjCounter = 0
        
        object Obj  { ObjCounter += 1}
        Obj // one

        def singleThreadedAccess(x: Any) = {
          x == Obj
        }

        def runTest {
          try {
            assert(singleThreadedAccess(Obj))
            assert(ObjCounter == 1)
          } catch {
            case e =>  print("failed "); e.printStackTrace()
          }
          println("ok")
        }

        runTest // trigger
      }
      
      lzvalue199_3 // trigger
    } 
  }
  
  def run { (new Class220_2) }
}


class Class225_1 {
  
  lazy val lzvalue224_2 = {
    
    lazy val lzvalue199_3 = {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    lzvalue199_3 // trigger
  }
  
  def run { lzvalue224_2 }
}


object Object226_1 {
  
  lazy val lzvalue224_2 = {
    
    lazy val lzvalue199_3 = {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    lzvalue199_3 // trigger
  }
  
  def run { lzvalue224_2 } // trigger
}


trait Trait227_1 {
  
  lazy val lzvalue224_2 = {
    
    lazy val lzvalue199_3 = {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    lzvalue199_3 // trigger
  }
  
  def run { lzvalue224_2 }
}



class Class234_1 {
  
  class Class233_2 {
    
    val value232_3 = {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    def run { value232_3 }
  }
  
  def run { (new Class233_2).run }
}


object Object235_1 {
  
  class Class233_2 {
    
    val value232_3 = {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    def run { value232_3 }
  }
  
  def run { (new Class233_2).run } // trigger
}


trait Trait236_1 {
  
  class Class233_2 {
    
    val value232_3 = {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    def run { value232_3 }
  }
  
  def run { (new Class233_2).run }
}


class Class238_1 {
  
  object Object237_2 {
    
    val value232_3 = {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    def run { value232_3 } // trigger
  }
  
  def run { Object237_2.run }
}


object Object239_1 {
  
  object Object237_2 {
    
    val value232_3 = {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    def run { value232_3 } // trigger
  }
  
  def run { Object237_2.run } // trigger
}


trait Trait240_1 {
  
  object Object237_2 {
    
    val value232_3 = {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    def run { value232_3 } // trigger
  }
  
  def run { Object237_2.run }
}


class Class242_1 {
  
  trait Trait241_2 {
    
    val value232_3 = {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    def run { value232_3 }
  }
  
  def run { (new Trait241_2 {}).run }
}


object Object243_1 {
  
  trait Trait241_2 {
    
    val value232_3 = {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    def run { value232_3 }
  }
  
  def run { (new Trait241_2 {}).run } // trigger
}


trait Trait244_1 {
  
  trait Trait241_2 {
    
    val value232_3 = {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    def run { value232_3 }
  }
  
  def run { (new Trait241_2 {}).run }
}


class Class246_1 {
  
  def method245_2 {
    
    val value232_3 = {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    value232_3 // trigger
  }
  
  def run { method245_2 }
}


object Object247_1 {
  
  def method245_2 {
    
    val value232_3 = {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    value232_3 // trigger
  }
  
  def run { method245_2 } // trigger
}


trait Trait248_1 {
  
  def method245_2 {
    
    val value232_3 = {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    value232_3 // trigger
  }
  
  def run { method245_2 }
}


class Class250_1 {
  
  val fun249_2 = () => {
    
    val value232_3 = {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    value232_3 // trigger
  }
  
  def run { fun249_2() }
}


object Object251_1 {
  
  val fun249_2 = () => {
    
    val value232_3 = {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    value232_3 // trigger
  }
  
  def run { fun249_2() } // trigger
}


trait Trait252_1 {
  
  val fun249_2 = () => {
    
    val value232_3 = {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    value232_3 // trigger
  }
  
  def run { fun249_2() }
}


class Class254_1 {
  
  class Class253_2 {
    { // in primary constructor 
      
      val value232_3 = {
        var ObjCounter = 0
        
        object Obj  { ObjCounter += 1}
        Obj // one

        def singleThreadedAccess(x: Any) = {
          x == Obj
        }

        def runTest {
          try {
            assert(singleThreadedAccess(Obj))
            assert(ObjCounter == 1)
          } catch {
            case e =>  print("failed "); e.printStackTrace()
          }
          println("ok")
        }

        runTest // trigger
      }
      
      value232_3 // trigger
    } 
  }
  
  def run { (new Class253_2) }
}


object Object255_1 {
  
  class Class253_2 {
    { // in primary constructor 
      
      val value232_3 = {
        var ObjCounter = 0
        
        object Obj  { ObjCounter += 1}
        Obj // one

        def singleThreadedAccess(x: Any) = {
          x == Obj
        }

        def runTest {
          try {
            assert(singleThreadedAccess(Obj))
            assert(ObjCounter == 1)
          } catch {
            case e =>  print("failed "); e.printStackTrace()
          }
          println("ok")
        }

        runTest // trigger
      }
      
      value232_3 // trigger
    } 
  }
  
  def run { (new Class253_2) } // trigger
}


trait Trait256_1 {
  
  class Class253_2 {
    { // in primary constructor 
      
      val value232_3 = {
        var ObjCounter = 0
        
        object Obj  { ObjCounter += 1}
        Obj // one

        def singleThreadedAccess(x: Any) = {
          x == Obj
        }

        def runTest {
          try {
            assert(singleThreadedAccess(Obj))
            assert(ObjCounter == 1)
          } catch {
            case e =>  print("failed "); e.printStackTrace()
          }
          println("ok")
        }

        runTest // trigger
      }
      
      value232_3 // trigger
    } 
  }
  
  def run { (new Class253_2) }
}


class Class258_1 {
  
  lazy val lzvalue257_2 = {
    
    val value232_3 = {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    value232_3 // trigger
  }
  
  def run { lzvalue257_2 }
}


object Object259_1 {
  
  lazy val lzvalue257_2 = {
    
    val value232_3 = {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    value232_3 // trigger
  }
  
  def run { lzvalue257_2 } // trigger
}


trait Trait260_1 {
  
  lazy val lzvalue257_2 = {
    
    val value232_3 = {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    value232_3 // trigger
  }
  
  def run { lzvalue257_2 }
}


class Class262_1 {
  
  val value261_2 = {
    
    val value232_3 = {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    value232_3 // trigger
  }
  
  def run { value261_2 }
}


object Object263_1 {
  
  val value261_2 = {
    
    val value232_3 = {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    value232_3 // trigger
  }
  
  def run { value261_2 } // trigger
}


trait Trait264_1 {
  
  val value261_2 = {
    
    val value232_3 = {
      var ObjCounter = 0
      
      object Obj  { ObjCounter += 1}
      Obj // one

      def singleThreadedAccess(x: Any) = {
        x == Obj
      }

      def runTest {
        try {
          assert(singleThreadedAccess(Obj))
          assert(ObjCounter == 1)
        } catch {
          case e =>  print("failed "); e.printStackTrace()
        }
        println("ok")
      }

      runTest // trigger
    }
    
    value232_3 // trigger
  }
  
  def run { value261_2 }
}


object Test extends App {
  (new Class3_1).run
  Object4_1.run
  (new Trait5_1 {}).run
  (new Class7_1).run
  Object8_1.run
  (new Trait9_1 {}).run
  (new Class11_1).run
  Object12_1.run
  (new Trait13_1 {}).run
  (new Class15_1).run
  Object16_1.run
  (new Trait17_1 {}).run
  (new Class19_1).run
  Object20_1.run
  (new Trait21_1 {}).run
  (new Class23_1).run
  Object24_1.run
  (new Trait25_1 {}).run
  (new Class27_1).run
  Object28_1.run
  (new Trait29_1 {}).run
  (new Class31_1).run
  Object32_1.run
  (new Trait33_1 {}).run
  (new Class36_1).run
  Object37_1.run
  (new Trait38_1 {}).run
  (new Class40_1).run
  Object41_1.run
  (new Trait42_1 {}).run
  (new Class44_1).run
  Object45_1.run
  (new Trait46_1 {}).run
  (new Class48_1).run
  Object49_1.run
  (new Trait50_1 {}).run
  (new Class52_1).run
  Object53_1.run
  (new Trait54_1 {}).run
  (new Class56_1).run
  Object57_1.run
  (new Trait58_1 {}).run
  (new Class60_1).run
  Object61_1.run
  (new Trait62_1 {}).run
  (new Class64_1).run
  Object65_1.run
  (new Trait66_1 {}).run
  (new Class69_1).run
  Object70_1.run
  (new Trait71_1 {}).run
  (new Class73_1).run
  Object74_1.run
  (new Trait75_1 {}).run
  (new Class77_1).run
  Object78_1.run
  (new Trait79_1 {}).run
  (new Class81_1).run
  Object82_1.run
  (new Trait83_1 {}).run
  (new Class85_1).run
  Object86_1.run
  (new Trait87_1 {}).run
  (new Class89_1).run
  Object90_1.run
  (new Trait91_1 {}).run
  (new Class93_1).run
  Object94_1.run
  (new Trait95_1 {}).run
  (new Class97_1).run
  Object98_1.run
  (new Trait99_1 {}).run
  (new Class102_1).run
  Object103_1.run
  (new Trait104_1 {}).run
  (new Class106_1).run
  Object107_1.run
  (new Trait108_1 {}).run
  (new Class110_1).run
  Object111_1.run
  (new Trait112_1 {}).run
  (new Class114_1).run
  Object115_1.run
  (new Trait116_1 {}).run
  (new Class118_1).run
  Object119_1.run
  (new Trait120_1 {}).run
  (new Class122_1).run
  Object123_1.run
  (new Trait124_1 {}).run
  (new Class126_1).run
  Object127_1.run
  (new Trait128_1 {}).run
  (new Class130_1).run
  Object131_1.run
  (new Trait132_1 {}).run
  (new Class135_1).run
  Object136_1.run
  (new Trait137_1 {}).run
  (new Class139_1).run
  Object140_1.run
  (new Trait141_1 {}).run
  (new Class143_1).run
  Object144_1.run
  (new Trait145_1 {}).run
  (new Class147_1).run
  Object148_1.run
  (new Trait149_1 {}).run
  (new Class151_1).run
  Object152_1.run
  (new Trait153_1 {}).run
  (new Class155_1).run
  Object156_1.run
  (new Trait157_1 {}).run
  (new Class159_1).run
  Object160_1.run
  (new Trait161_1 {}).run
  (new Class163_1).run
  Object164_1.run
  (new Trait165_1 {}).run
  (new Class168_1).run
  Object169_1.run
  (new Trait170_1 {}).run
  (new Class172_1).run
  Object173_1.run
  (new Trait174_1 {}).run
  (new Class176_1).run
  Object177_1.run
  (new Trait178_1 {}).run
  (new Class180_1).run
  Object181_1.run
  (new Trait182_1 {}).run
  (new Class184_1).run
  Object185_1.run
  (new Trait186_1 {}).run
  (new Class188_1).run
  Object189_1.run
  (new Trait190_1 {}).run
  (new Class192_1).run
  Object193_1.run
  (new Trait194_1 {}).run
  (new Class196_1).run
  Object197_1.run
  (new Trait198_1 {}).run
  (new Class201_1).run
  Object202_1.run
  (new Trait203_1 {}).run
  (new Class205_1).run
  Object206_1.run
  (new Trait207_1 {}).run
  (new Class209_1).run
  Object210_1.run
  (new Trait211_1 {}).run
  (new Class213_1).run
  Object214_1.run
  (new Trait215_1 {}).run
  (new Class217_1).run
  Object218_1.run
  (new Trait219_1 {}).run
  (new Class221_1).run
  Object222_1.run
  (new Trait223_1 {}).run
  (new Class225_1).run
  Object226_1.run
  (new Trait227_1 {}).run
  (new Class234_1).run
  Object235_1.run
  (new Trait236_1 {}).run
  (new Class238_1).run
  Object239_1.run
  (new Trait240_1 {}).run
  (new Class242_1).run
  Object243_1.run
  (new Trait244_1 {}).run
  (new Class246_1).run
  Object247_1.run
  (new Trait248_1 {}).run
  (new Class250_1).run
  Object251_1.run
  (new Trait252_1 {}).run
  (new Class254_1).run
  Object255_1.run
  (new Trait256_1 {}).run
  (new Class258_1).run
  Object259_1.run
  (new Trait260_1 {}).run
  (new Class262_1).run
  Object263_1.run
  (new Trait264_1 {}).run
}

