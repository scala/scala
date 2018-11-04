package scala.util

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import org.junit.Assert._

import scala.reflect.ClassTag
import scala.util.control.ControlThrowable

@RunWith(classOf[JUnit4])
class UsingTest {
  import UsingTest._

  /* raw `Using.resource` that doesn't use `Try` */

  @Test
  def usingResourceThatThrowsException(): Unit = {
    val exception = use(new ExceptionResource, new UsingException(_))
    assertThrowableClass[UsingException](exception)
    assertSingleSuppressed[ClosingException](exception)

    val error = use(new ExceptionResource, new Error(_))
    assertThrowableClass[Error](error)
    assertSingleSuppressed[ClosingException](error)

    val fatal = use(new ExceptionResource, new OutOfMemoryError(_))
    assertThrowableClass[OutOfMemoryError](fatal)
    assertSingleSuppressed[ClosingException](fatal)

    val control = use(new ExceptionResource, new UsingMarker(_))
    assertThrowableClass[UsingMarker](control)
    assertNoSuppressed(control)
  }

  @Test
  def usingResourceThatThrowsError(): Unit = {
    val exception = use(new ErrorResource, new UsingException(_))
    assertThrowableClass[UsingException](exception)
    assertSingleSuppressed[ClosingError](exception)

    val error = use(new ErrorResource, new Error(_))
    assertThrowableClass[Error](error)
    assertSingleSuppressed[ClosingError](error)

    val fatal = use(new ErrorResource, new OutOfMemoryError(_))
    assertThrowableClass[OutOfMemoryError](fatal)
    assertSingleSuppressed[ClosingError](fatal)

    val control = use(new ErrorResource, new UsingMarker(_))
    assertThrowableClass[UsingMarker](control)
    assertNoSuppressed(control)
  }

  @Test
  def usingResourceThatThrowsFatal(): Unit = {
    val exception = use(new FatalResource, new UsingException(_))
    assertThrowableClass[StackOverflowError](exception)
    assertSingleSuppressed[UsingException](exception)

    val error = use(new FatalResource, new Error(_))
    assertThrowableClass[StackOverflowError](error)
    assertSingleSuppressed[Error](error)

    val fatal = use(new FatalResource, new OutOfMemoryError(_))
    assertThrowableClass[OutOfMemoryError](fatal)
    assertSingleSuppressed[StackOverflowError](fatal)

    val control = use(new FatalResource, new UsingMarker(_))
    assertThrowableClass[UsingMarker](control)
    assertNoSuppressed(control)
  }

  @Test
  def usingResourceThatThrowsControlThrowable(): Unit = {
    val exception = use(new MarkerResource, new UsingException(_))
    assertThrowableClass[ClosingMarker](exception)
    assertNoSuppressed(exception)

    val error = use(new MarkerResource, new Error(_))
    assertThrowableClass[ClosingMarker](error)
    assertNoSuppressed(error)

    val fatal = use(new MarkerResource, new OutOfMemoryError(_))
    assertThrowableClass[OutOfMemoryError](fatal)
    assertSingleSuppressed[ClosingMarker](fatal)

    val control = use(new MarkerResource, new UsingMarker(_))
    assertThrowableClass[UsingMarker](control)
    assertNoSuppressed(control)
  }

  /* safe `Using` that returns `Try` */

  @Test
  def safeUsingResourceThatThrowsException(): Unit = {
    val exception = UseWrapped(new ExceptionResource, new UsingException(_))
    assertThrowableClass[UsingException](exception)
    assertSingleSuppressed[ClosingException](exception)

    val error = UseWrapped(new ExceptionResource, new Error(_))
    assertThrowableClass[Error](error)
    assertSingleSuppressed[ClosingException](error)

    val fatal = UseWrapped.catching(new ExceptionResource, new OutOfMemoryError(_))
    assertThrowableClass[OutOfMemoryError](fatal)
    assertSingleSuppressed[ClosingException](fatal)

    val control = UseWrapped.catching(new ExceptionResource, new UsingMarker(_))
    assertThrowableClass[UsingMarker](control)
    assertNoSuppressed(control)
  }

  @Test
  def safeUsingResourceThatThrowsError(): Unit = {
    val exception = UseWrapped(new ErrorResource, new UsingException(_))
    assertThrowableClass[UsingException](exception)
    assertSingleSuppressed[ClosingError](exception)

    val error = UseWrapped(new ErrorResource, new Error(_))
    assertThrowableClass[Error](error)
    assertSingleSuppressed[ClosingError](error)

    val fatal = UseWrapped.catching(new ErrorResource, new OutOfMemoryError(_))
    assertThrowableClass[OutOfMemoryError](fatal)
    assertSingleSuppressed[ClosingError](fatal)

    val control = UseWrapped.catching(new ErrorResource, new UsingMarker(_))
    assertThrowableClass[UsingMarker](control)
    assertNoSuppressed(control)
  }

  @Test
  def safeUsingResourceThatThrowsFatal(): Unit = {
    val exception = UseWrapped.catching(new FatalResource, new UsingException(_))
    assertThrowableClass[StackOverflowError](exception)
    assertSingleSuppressed[UsingException](exception)

    val error = UseWrapped.catching(new FatalResource, new Error(_))
    assertThrowableClass[StackOverflowError](error)
    assertSingleSuppressed[Error](error)

    val fatal = UseWrapped.catching(new FatalResource, new OutOfMemoryError(_))
    assertThrowableClass[OutOfMemoryError](fatal)
    assertSingleSuppressed[StackOverflowError](fatal)

    val control = UseWrapped.catching(new FatalResource, new UsingMarker(_))
    assertThrowableClass[UsingMarker](control)
    assertNoSuppressed(control)
  }

  @Test
  def safeUsingResourceThatThrowsControlThrowable(): Unit = {
    val exception = UseWrapped.catching(new MarkerResource, new UsingException(_))
    assertThrowableClass[ClosingMarker](exception)
    assertNoSuppressed(exception)

    val error = UseWrapped.catching(new MarkerResource, new Error(_))
    assertThrowableClass[ClosingMarker](error)
    assertNoSuppressed(error)

    val fatal = UseWrapped.catching(new MarkerResource, new OutOfMemoryError(_))
    assertThrowableClass[OutOfMemoryError](fatal)
    assertSingleSuppressed[ClosingMarker](fatal)

    val control = UseWrapped.catching(new MarkerResource, new UsingMarker(_))
    assertThrowableClass[UsingMarker](control)
    assertNoSuppressed(control)
  }

  /* nested resource usage returns the correct exception */

  @Test
  def usingMultipleResourcesPropagatesCorrectlySimple(): Unit = {
    val usingException = catchThrowable {
      Using.resource(new ExceptionResource) { _ =>
        Using.resource(new ErrorResource) { _ =>
          throw new UsingException("nested `Using.resource`")
        }
      }
    }

    // uncomment to debug actual suppression nesting
    //usingException.printStackTrace()

    /*
    UsingException
     |- ClosingError
     |- ClosingException
     */
    assertThrowableClass[UsingException](usingException)
    val suppressed = usingException.getSuppressed
    assertEquals(suppressed.length, 2)
    val closingError = suppressed(0)
    val closingException = suppressed(1)
    assertThrowableClass[ClosingError](closingError)
    assertThrowableClass[ClosingException](closingException)
  }

  @Test
  def usingMultipleResourcesPropagatesCorrectlyComplex(): Unit = {
    val fatal = catchThrowable {
      Using.resource(new ExceptionResource) { _ =>
        Using.resource(new FatalResource) { _ =>
          Using.resource(new ErrorResource) { _ =>
            throw new UsingException("nested `Using.resource`")
          }
        }
      }
    }

    // uncomment to debug actual suppression nesting
    //fatal.printStackTrace()

    /*
    StackOverflowError
     |- UsingException
     |   |- ClosingError
     |- ClosingException
     */
    assertThrowableClass[StackOverflowError](fatal)
    val firstLevelSuppressed = fatal.getSuppressed
    assertEquals(firstLevelSuppressed.length, 2)
    val usingException = firstLevelSuppressed(0)
    val closingException = firstLevelSuppressed(1)
    assertThrowableClass[UsingException](usingException)
    assertThrowableClass[ClosingException](closingException)
    assertSingleSuppressed[ClosingError](usingException)
  }

  @Test
  def safeUsingMultipleResourcesPropagatesCorrectlySimple(): Unit = {
    val scala.util.Failure(usingException) =
      Using(new ExceptionResource) { _ =>
        Using(new ErrorResource) { _ =>
          throw new UsingException("nested `Using`")
        }.get
      }

    // uncomment to debug actual suppression nesting
    //usingException.printStackTrace()

    /*
    UsingException
     |- ClosingError
     |- ClosingException
     */
    assertThrowableClass[UsingException](usingException)
    val suppressed = usingException.getSuppressed
    assertEquals(suppressed.length, 2)
    val closingError = suppressed(0)
    val closingException = suppressed(1)
    assertThrowableClass[ClosingError](closingError)
    assertThrowableClass[ClosingException](closingException)
  }

  @Test
  def safeUsingMultipleResourcesPropagatesCorrectlyComplex(): Unit = {
    val fatal = catchThrowable {
      Using(new ExceptionResource) { _ =>
        Using(new FatalResource) { _ =>
          Using(new ErrorResource) { _ =>
            throw new UsingException("nested `Using`")
          }.get
        }.get
      }
    }

    // uncomment to debug actual suppression nesting
    //fatal.printStackTrace()

    /*
    StackOverflowError
     |- UsingException
     |   |- ClosingError
     |- ClosingException
     */
    assertThrowableClass[StackOverflowError](fatal)
    val firstLevelSuppressed = fatal.getSuppressed
    assertEquals(firstLevelSuppressed.length, 2)
    val usingException = firstLevelSuppressed(0)
    val closingException = firstLevelSuppressed(1)
    assertThrowableClass[UsingException](usingException)
    assertThrowableClass[ClosingException](closingException)
    assertSingleSuppressed[ClosingError](usingException)
  }

  /* works when throwing no exceptions */

  @Test
  def usingResourceWithNoThrow(): Unit = {
    val res = Using.resource(new NoOpResource) { r =>
      r.identity("test")
    }
    assertEquals(res, "test")
  }

  @Test
  def safeUsingResourceWithNoThrow(): Unit = {
    val res = Using(new NoOpResource) { r =>
      r.identity("test")
    }
    assertEquals(res, scala.util.Success("test"))
  }

  /* using multiple resources close in the correct order */

  @Test
  def using2Resources(): Unit = {
    val group = new ResourceGroup
    val res = Using.resources(
      group.newResource(),
      group.newResource(),
    ) { (r1, r2) =>
      r1.identity(1) + r2.identity(1)
    }
    assertEquals(res, 2)
  }

  @Test
  def using3Resources(): Unit = {
    val group = new ResourceGroup
    val res = Using.resources(
      group.newResource(),
      group.newResource(),
      group.newResource(),
    ) { (r1, r2, r3) =>
      r1.identity(1) +
        r2.identity(1) +
        r3.identity(1)
    }
    assertEquals(res, 3)
  }

  @Test
  def using4Resources(): Unit = {
    val group = new ResourceGroup
    val res = Using.resources(
      group.newResource(),
      group.newResource(),
      group.newResource(),
      group.newResource(),
    ) { (r1, r2, r3, r4) =>
      r1.identity(1) +
        r2.identity(1) +
        r3.identity(1) +
        r4.identity(1)
    }
    assertEquals(res, 4)
  }

  @Test
  def safeUsing2Resources(): Unit = {
    val group = new ResourceGroup
    val res = Using(group.newResource()) { r1 =>
      Using(group.newResource()) { r2 =>
        r1.identity(1) + r2.identity(1)
      }.get
    }
    assertEquals(res, scala.util.Success(2))
  }

  @Test
  def safeUsing3Resources(): Unit = {
    val group = new ResourceGroup
    val res = Using(group.newResource()) { r1 =>
      Using(group.newResource()) { r2 =>
        Using(group.newResource()) { r3 =>
          r1.identity(1) +
          r2.identity(1) +
          r3.identity(1)
        }.get
      }.get
    }
    assertEquals(res, scala.util.Success(3))
  }

  /* misc */

  @Test
  def usingDisallowsNull(): Unit = {
    val npe = catchThrowable(Using.resource(null: AutoCloseable)(_ => "test"))
    assertThrowableClass[NullPointerException](npe)
  }

  @Test
  def safeUsingDisallowsNull(): Unit = {
    val npe = Using(null: AutoCloseable)(_ => "test").failed.get
    assertThrowableClass[NullPointerException](npe)
  }

  @Test
  def safeUsingCatchesOpeningException(): Unit = {
    val ex = Using({ throw new RuntimeException }: AutoCloseable)(_ => "test").failed.get
    assertThrowableClass[RuntimeException](ex)
  }
}

object UsingTest {
  final class ClosingException(message: String) extends Exception(message)
  final class UsingException(message: String) extends Exception(message)
  final class ClosingError(message: String) extends Error(message)
  final class UsingError(message: String) extends Error(message)
  final class ClosingMarker(message: String) extends Throwable(message) with ControlThrowable
  final class UsingMarker(message: String) extends Throwable(message) with ControlThrowable

  abstract class BaseResource extends AutoCloseable {
    final def identity[A](a: A): A = a
  }

  final class NoOpResource extends BaseResource {
    override def close(): Unit = ()
  }

  abstract class CustomResource(t: String => Throwable) extends BaseResource {
    override final def close(): Unit = throw t("closing " + getClass.getSimpleName)
  }

  final class ExceptionResource extends CustomResource(new ClosingException(_))
  final class ErrorResource extends CustomResource(new ClosingError(_))
  final class FatalResource extends CustomResource(new StackOverflowError(_))
  final class MarkerResource extends CustomResource(new ClosingMarker(_))

  def assertThrowableClass[T <: Throwable: ClassTag](t: Throwable): Unit = {
    assertEquals(t.getClass, implicitly[ClassTag[T]].runtimeClass)
  }

  def assertSingleSuppressed[T <: Throwable: ClassTag](t: Throwable): Unit = {
    val suppressed = t.getSuppressed
    assertEquals(suppressed.length, 1)
    assertThrowableClass[T](suppressed(0))
  }

  def assertNoSuppressed(t: Throwable): Unit = {
    assertEquals(t.getSuppressed.length, 0)
  }

  def catchThrowable(thunk: => Any): Throwable = {
    try {
      thunk
      throw new AssertionError("unreachable")
    } catch {
      case t: Throwable => t
    }
  }

  object UseWrapped {
    def apply(resource: => BaseResource, t: String => Throwable): Throwable =
      Using(resource)(opThrowing(t)).failed.get

    def catching(resource: => BaseResource, t: String => Throwable): Throwable =
      catchThrowable(Using(resource)(opThrowing(t)))
  }

  def use(resource: BaseResource, t: String => Throwable): Throwable =
    catchThrowable(Using.resource(resource)(opThrowing(t)))

  private def opThrowing(t: String => Throwable): BaseResource => Nothing =
    r => {
      r.identity("test")
      throw t("exception using resource")
    }

  final class ResourceGroup {
    // tracks the number of open resources
    private var openCount: Int = 0

    def newResource(): BaseResource = {
      openCount += 1
      new CountingResource(openCount)
    }

    private final class CountingResource(countWhenCreated: Int) extends BaseResource {
      override def close(): Unit = {
        assertEquals(countWhenCreated, openCount)
        openCount -= 1
      }
    }
  }
}
