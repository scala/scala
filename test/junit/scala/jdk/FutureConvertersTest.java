/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.jdk;

import org.junit.jupiter.api.Test;
import scala.concurrent.Future;
import scala.concurrent.Promise;

import java.util.concurrent.*;

import static java.util.concurrent.TimeUnit.MILLISECONDS;
import static java.util.concurrent.TimeUnit.SECONDS;
import static org.junit.jupiter.api.Assertions.*;
import static scala.jdk.javaapi.FutureConverters.asJava;
import static scala.jdk.javaapi.FutureConverters.asScala;

public class FutureConvertersTest {
    private <T> Promise<T> promise() { return Promise.apply(); }

    @Test
    public void testToScalaSuccess() {
        final CompletableFuture<String> cs = new CompletableFuture<>();
        final Future<String> f = asScala(cs);
        assertFalse(f.isCompleted(), "f must not yet be completed");
        cs.complete("Hello");
        assertTrue(f.isCompleted(), "f must be completed by now");
        assertEquals("Hello", f.value().get().get());
    }

    @Test
    public void testToScalaFailure() {
        final CompletableFuture<String> cs = new CompletableFuture<>();
        final Future<String> f = asScala(cs);
        assertFalse(f.isCompleted(), "f must not yet be completed");
        final Exception ex = new RuntimeException("Hello");
        cs.completeExceptionally(ex);
        assertTrue(f.isCompleted(), "f must be completed by now");
        assertEquals(ex, f.value().get().failed().get());
    }

    @Test
    public void testToJavaSuccess() throws InterruptedException,
            ExecutionException {
        final Promise<String> p = promise();
        final CompletionStage<String> cs = asJava(p.future());
        final CompletableFuture<String> cp = (CompletableFuture<String>) cs;
        assertFalse(cp.isDone(), "cs must not yet be completed");
        p.success("Hello");
        assertTrue(cp.isDone(), "cs must be completed by now");
        assertEquals("Hello", cp.get());
    }

    @Test
    public void testToJavaFailure() throws InterruptedException,
            ExecutionException {
        final Promise<String> p = promise();
        final CompletionStage<String> cs = asJava(p.future());
        final CompletableFuture<String> cp = (CompletableFuture<String>) cs;
        assertFalse(cp.isDone(), "cs must not yet be completed");
        final Exception ex = new RuntimeException("Hello");
        p.failure(ex);
        assertTrue(cp.isDone(), "cs must be completed by now");
        assertEquals(ex.toString(), cp.exceptionally(x -> x.toString()).get(), "exceptionally equals");
        Throwable thr = null;
        try {
            cp.get();
        } catch (Throwable t) {
            thr = t;
        }
        assertNotNull(thr, "get() must throw");
        assertEquals(ExecutionException.class, thr.getClass(), "thrown exception must be wrapped");
        assertEquals(ex, thr.getCause(), "wrapper must contain the right exception");
    }

    @Test
    public void testToJavaThenApply() throws InterruptedException,
            ExecutionException {
        final Promise<String> p = promise();
        final CompletionStage<String> cs = asJava(p.future());
        final CountDownLatch latch = new CountDownLatch(1);
        final CompletionStage<String> second = cs.thenApply(x -> {
            try {
                assertTrue(latch.await(1, SECONDS), "latch must succeed");
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
            return x;
        });
        p.success("Hello");
        latch.countDown();
        assertEquals("Hello", second.toCompletableFuture().get());
    }

    @Test
    public void testToJavaThenAccept() throws InterruptedException,
            ExecutionException {
        final Promise<String> p = promise();
        final CompletionStage<String> cs = asJava(p.future());
        final CountDownLatch latch = new CountDownLatch(1);
        final CompletionStage<Void> second = cs.thenAccept(x -> {
            try {
                assertTrue(latch.await(1, SECONDS), "latch must succeed");
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        });
        p.success("Hello");
        latch.countDown();
        assertNull(second.toCompletableFuture().get(), "result must be Void");
    }

    @Test
    public void testToJavaThenRun() throws InterruptedException,
            ExecutionException {
        final Promise<String> p = promise();
        final CompletionStage<String> cs = asJava(p.future());
        final CountDownLatch latch = new CountDownLatch(1);
        final CompletionStage<Void> second = cs.thenRun(() -> {
            try {
                assertTrue(latch.await(1, SECONDS), "latch must succeed");
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        });
        p.success("Hello");
        latch.countDown();
        assertNull(second.toCompletableFuture().get(), "result must be Void");
    }

    @Test
    public void testToJavaThenCombine() throws InterruptedException,
            ExecutionException {
        final Promise<String> p = promise();
        final CompletionStage<String> cs = asJava(p.future());
        final CompletionStage<Integer> other = CompletableFuture.completedFuture(42);
        final CountDownLatch latch = new CountDownLatch(1);
        final CompletionStage<Integer> second = cs.thenCombine(other, (x, y) -> {
            try {
                assertTrue(latch.await(1, SECONDS), "latch must succeed");
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
            return x.length() + y;
        });
        p.success("Hello");
        latch.countDown();
        assertEquals((Integer) 47, second.toCompletableFuture().get());
    }

    @Test
    public void testToJavaThenAcceptBoth() throws InterruptedException,
            ExecutionException {
        final Promise<String> p = promise();
        final CompletionStage<String> cs = asJava(p.future());
        final CompletionStage<Integer> other = CompletableFuture.completedFuture(42);
        final CountDownLatch latch = new CountDownLatch(1);
        final CompletionStage<Void> second = cs.thenAcceptBoth(other, (x, y) -> {
            try {
                assertTrue(latch.await(1, SECONDS), "latch must succeed");
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        });
        p.success("Hello");
        latch.countDown();
        assertNull(second.toCompletableFuture().get(), "result must be Void");
    }

    @Test
    public void testToJavaRunAfterBoth() throws InterruptedException,
            ExecutionException {
        final Promise<String> p = promise();
        final CompletionStage<String> cs = asJava(p.future());
        final CompletionStage<Integer> other = CompletableFuture.completedFuture(42);
        final CountDownLatch latch = new CountDownLatch(1);
        final CompletionStage<Void> second = cs.runAfterBoth(other, () -> {
            try {
                assertTrue(latch.await(1, SECONDS), "latch must succeed");
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        });
        p.success("Hello");
        latch.countDown();
        assertNull(second.toCompletableFuture().get(), "result must be Void");
    }

    @Test
    public void testToJavaApplyToEither() throws InterruptedException,
            ExecutionException {
        final Promise<String> p = promise();
        final CompletionStage<String> cs = asJava(p.future());
        final CompletionStage<String> other = new CompletableFuture<>();
        final CountDownLatch latch = new CountDownLatch(1);
        final CompletionStage<Integer> second = cs.applyToEither(other, x -> {
            try {
                assertTrue(latch.await(1, SECONDS), "latch must succeed");
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
            return x.length();
        });
        p.success("Hello");
        latch.countDown();
        assertEquals((Integer) 5, second.toCompletableFuture().get());
    }

    @Test
    public void testToJavaAcceptEither() throws InterruptedException,
            ExecutionException {
        final Promise<String> p = promise();
        final CompletionStage<String> cs = asJava(p.future());
        final CompletionStage<String> other = new CompletableFuture<>();
        final CountDownLatch latch = new CountDownLatch(1);
        final CompletionStage<Void> second = cs.acceptEither(other, x -> {
            try {
                assertTrue(latch.await(1, SECONDS), "latch must succeed");
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        });
        p.success("Hello");
        latch.countDown();
        assertNull(second.toCompletableFuture().get(), "result must be Void");
    }

    @Test
    public void testToJavaRunAfterEither() throws InterruptedException,
            ExecutionException {
        final Promise<String> p = promise();
        final CompletionStage<String> cs = asJava(p.future());
        final CompletionStage<String> other = new CompletableFuture<>();
        final CountDownLatch latch = new CountDownLatch(1);
        final CompletionStage<Void> second = cs.runAfterEither(other, () -> {
            try {
                assertTrue(latch.await(1, SECONDS), "latch must succeed");
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        });
        p.success("Hello");
        latch.countDown();
        assertNull(second.toCompletableFuture().get(), "result must be Void");
    }

    @Test
    public void testToJavaThenCompose() throws InterruptedException,
            ExecutionException {
        final Promise<String> p = promise();
        final CompletionStage<String> cs = asJava(p.future());
        final CountDownLatch latch = new CountDownLatch(1);
        final CompletionStage<String> second = cs.thenCompose(x -> {
            try {
                assertTrue(latch.await(1, SECONDS), "latch must succeed");
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
            return CompletableFuture.completedFuture(x);
        });
        p.success("Hello");
        latch.countDown();
        assertEquals("Hello", second.toCompletableFuture().get());
    }

    @Test
    public void testToJavaWhenComplete() throws InterruptedException,
            ExecutionException {
        final Promise<String> p = promise();
        final CompletionStage<String> cs = asJava(p.future());
        final CountDownLatch latch = new CountDownLatch(1);
        final CompletionStage<String> second = cs.whenComplete((v, e) -> {
            try {
                assertTrue(latch.await(1, SECONDS), "latch must succeed");
            } catch (Exception ex) {
                throw new RuntimeException(ex);
            }
        });
        p.success("Hello");
        latch.countDown();
        assertEquals("Hello", second.toCompletableFuture().get());
    }

    @Test
    public void testToJavaHandle() throws InterruptedException,
            ExecutionException {
        final Promise<String> p = promise();
        final CompletionStage<String> cs = asJava(p.future());
        final CountDownLatch latch = new CountDownLatch(1);
        final CompletionStage<Integer> second = cs.handle((v, e) -> {
            try {
                assertTrue(latch.await(1, SECONDS), "latch must succeed");
            } catch (Exception ex) {
                throw new RuntimeException(ex);
            }
            return v.length();
        });
        p.success("Hello");
        latch.countDown();
        assertEquals((Integer) 5, second.toCompletableFuture().get());
    }

    @Test
    public void testToJavaExceptionally() throws InterruptedException,
            ExecutionException {
        final Promise<String> p = promise();
        final CompletionStage<String> cs = asJava(p.future());
        final CountDownLatch latch = new CountDownLatch(1);
        final CompletionStage<String> second = cs.exceptionally(e -> {
            try {
                assertTrue(latch.await(1, SECONDS), "latch must succeed");
            } catch (Exception ex) {
                throw new RuntimeException(ex);
            }
            return e.getMessage();
        });
        p.failure(new RuntimeException("Hello"));
        latch.countDown();
        assertEquals("Hello", second.toCompletableFuture().get());
    }

    @Test
    public void testToJavaThenComposeWithToJavaThenAccept() throws InterruptedException,
            ExecutionException, TimeoutException {
        // Test case from https://github.com/scala/scala-java8-compat/issues/29
        final Promise<String> p1 = promise();
        final CompletableFuture<String> future = new CompletableFuture<>();

        CompletableFuture.supplyAsync(() -> "Hello").
                thenCompose(x -> asJava(p1.future())).handle((x, t) -> future.complete(x));
        p1.success("Hello");
        assertEquals("Hello", future.get(1000, MILLISECONDS));
    }

    @Test
    public void testToJavaToCompletableFuture() throws ExecutionException, InterruptedException {
        final Promise<String> p = promise();
        final CompletionStage<String> cs = asJava(p.future());
        CompletableFuture<String> cf = cs.toCompletableFuture();
        assertEquals("notyet", cf.getNow("notyet"));
        p.success("done");
        assertEquals("done", cf.get());
    }

    @Test
    public void testToJavaToCompletableFutureDoesNotMutateUnderlyingPromise() throws ExecutionException, InterruptedException {
        final Promise<String> p = promise();
        Future<String> sf = p.future();
        final CompletionStage<String> cs = asJava(sf);
        CompletableFuture<String> cf = cs.toCompletableFuture();
        assertEquals("notyet", cf.getNow("notyet"));
        cf.complete("done");
        assertEquals("done", cf.get());
        assertFalse(sf.isCompleted());
        assertFalse(p.isCompleted());
    }

    @Test
    public void testToJavaToCompletableFutureJavaCompleteCalledAfterScalaComplete() throws ExecutionException, InterruptedException {
        final Promise<String> p = promise();
        Future<String> sf = p.future();
        final CompletionStage<String> cs = asJava(sf);
        CompletableFuture<String> cf = cs.toCompletableFuture();
        assertEquals("notyet", cf.getNow("notyet"));
        p.success("scaladone");
        assertEquals("scaladone", cf.get());
        cf.complete("javadone");
        assertEquals("scaladone", cf.get());
    }

    @Test
    public void testToJavaToCompletableFutureJavaCompleteCalledBeforeScalaComplete() throws ExecutionException, InterruptedException {
        final Promise<String> p = promise();
        Future<String> sf = p.future();
        final CompletionStage<String> cs = asJava(sf);
        CompletableFuture<String> cf = cs.toCompletableFuture();
        assertEquals("notyet", cf.getNow("notyet"));
        cf.complete("javadone");
        assertEquals("javadone", cf.get());
        p.success("scaladone");
        assertEquals("javadone", cf.get());
    }

    @Test
    public void testToJavaToCompletableFutureJavaObtrudeCalledBeforeScalaComplete() throws ExecutionException, InterruptedException {
        final Promise<String> p = promise();
        Future<String> sf = p.future();
        final CompletionStage<String> cs = asJava(sf);
        CompletableFuture<String> cf = cs.toCompletableFuture();
        try {
            cf.obtrudeValue("");
            fail();
        } catch (UnsupportedOperationException iae) {
            // okay
        }
        try {
            cf.obtrudeException(new Exception());
            fail();
        } catch (UnsupportedOperationException iae) {
            // okay
        }
    }

    @Test
    public void testToJavaAndBackAvoidsWrappers() {
        final Promise<String> p = promise();
        final Future<String> sf = p.future();
        final CompletionStage<String> cs = asJava(sf);
        Future<String> sf1 = asScala(cs);
        assertSame(sf, sf1);
    }

    @Test
    public void testToScalaAndBackAvoidsWrappers() {
        final CompletableFuture<String> cf = new CompletableFuture<>();
        final Future<String> f = asScala(cf);
        CompletionStage<String> cs1 = asJava(f);
        assertSame(cf, cs1);

    }
}
