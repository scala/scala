import scala.util.control.NonFatal

trait NonFatalTests {

	//NonFatals
    val nonFatals: Seq[Throwable] =
      Seq(new StackOverflowError,
          new RuntimeException,
          new Exception,
          new Throwable)
    
    //Fatals
    val fatals: Seq[Throwable] =
      Seq(new InterruptedException,
          new OutOfMemoryError,
          new LinkageError,
          new VirtualMachineError {},
          new Throwable with scala.util.control.ControlThrowable,
          new NotImplementedError)

	def testFatalsUsingApply(): Unit = {
	   fatals foreach { t => assert(NonFatal(t) == false) }
	}

	def testNonFatalsUsingApply(): Unit = {
       nonFatals foreach { t => assert(NonFatal(t) == true) }
	}

	def testFatalsUsingUnapply(): Unit = {
      fatals foreach { t => assert(NonFatal.unapply(t).isEmpty) }
	}

	def testNonFatalsUsingUnapply(): Unit = {
       nonFatals foreach { t => assert(NonFatal.unapply(t).isDefined) }
	}

	testFatalsUsingApply()
	testNonFatalsUsingApply()
	testFatalsUsingUnapply()
	testNonFatalsUsingUnapply()
}

object Test
extends App
with NonFatalTests {
  System.exit(0)
}