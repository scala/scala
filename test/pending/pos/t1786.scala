class SomeClass(val intValue:Int)
class MyClass[T <: SomeClass](val myValue:T)

def myMethod(i:MyClass[_]) {
   i.myValue.intValue/2      // << error i is of type Any
}

def myMethod(i:MyClass[_ <: SomeClass]) {
   i.myValue.intValue/2      // << works
}
/*
The below code shows a compiler flaw in that the wildcard "_" as value for a bounded type parameter either breaks the boundry - as it result in Any - or doesnt (as id hoped it to be) evaluates to the boundy.
*/
