
import reflect.dynamic.DynamicProxy

object o1{ def foo( i:Int = 5 ) = i }
object o2{ def foo( i:Int, name:String = "foo" ) = name + i }

object Test extends App {
  val p1 = new DynamicProxy {
    val proxyTarget = o1
  }
  val p2 = new DynamicProxy {
    val proxyTarget = o2
  }

  assert( p1.foo == 5 )
  assert( p1.foo() == 5 )
  assert( p1.foo(6) == 6 )

  assert( p2.foo(6) == "foo6" )
  assert( p2.foo(6,"test") == "test6" )

  // test fails
  try{
    p1.foo(6,bar=8) // <- expected exception, but none thrown
    assert( false ) 
  } catch {  case e:AssertionError => throw e ;case e:java.util.NoSuchElementException => println(e) ;case _: Throwable => () }

  // probably not intended (or at least not speaking): java.util.NoSuchElementException: None.get in DynamicProxy line 181
  try{
    p2.foo // <- expected exception, but probably not java.util.NoSuchElementException
    assert( false )
  } catch {  case e:AssertionError => throw e ;case e:java.util.NoSuchElementException => println(e);case _: Throwable => () }

  // probably not intended (or at least not speaking): java.util.NoSuchElementException: None.get in DynamicProxy line 181
  try{
    p2.foo() // <- expected exception, but probably not java.util.NoSuchElementException
    assert( false )
  } catch {  case e:AssertionError => throw e ;case e:java.util.NoSuchElementException => println(e);case _: Throwable => () }

  // probably not intended (or at least not speaking): java.util.NoSuchElementException: None.get in DynamicProxy line 181
  try{
    p2.foo(6, xyz = "foo") // <- expected exception, but probably not java.util.NoSuchElementException
    assert( false )
  } catch {  case e:AssertionError => throw e ;case e:java.util.NoSuchElementException => println(e);case _: Throwable => () }

  // test fails
  try{
    p2.foo(6, "test", xyz = "foo") // <- expected exception, but none thrown
    assert( false )
  } catch {  case e:AssertionError => throw e ;case e:java.util.NoSuchElementException => println(e);case _: Throwable => () }

  // probably not intended (or at least not speaking): java.util.NoSuchElementException: None.get in DynamicProxy line 181
  try{
    p2.foo(6, name="test", xyz = "foo") // <- expected exception, but probably not java.util.NoSuchElementException
    assert( false )
  } catch { case e:AssertionError => throw e ;case e:java.util.NoSuchElementException => println(e);case _: Throwable => () }
}
