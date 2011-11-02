//A fatal error or Scala compiler
// Scala compiler version 2.7.1-final -- (c) 2002-2011 LAMP/EPFL
// Carlos Loria cloria@artinsoft.com
// 7/10/2008

class A  {
  var name:String = _
  def getName() = name
  def this(name:String, age:Int){this();this.name=name}
  
}

class B(name:String) extends A(name,0){
}

class D {

   object A {
     def unapply(p:A) = Some(p.getName)
   }
   
   object B {
     def unapply(p:B) = Some(p.getName)
   }
   def foo(p:Any) = p match {
      case B(n)    => println("B") 
      case A(n)    => println("A")  
      
        
   }

}
