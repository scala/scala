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

package scala.jdk.javaapi;

import org.junit.Test;
import scala.Option;

import static org.junit.Assert.*;

import java.util.Optional;
import java.util.OptionalInt;

public final class OptionConvertersJavaTest {
    public Option<String> os() { return Option.apply("hi"); }
    public Option<Integer> oi() { return Option.apply(1); }

    @Test
    public void testConverters() {
        Optional<String> jos = OptionConverters.toJava(os());
        assertEquals("hi", jos.get());
        assertEquals(os(), OptionConverters.toScala(jos));

        Optional<Integer> joI = OptionConverters.toJava(oi());
        assertEquals(1, joI.get().intValue());
        Option<Integer> soI = OptionConverters.toScala(joI);
        assertEquals(oi(), soI);

        OptionalInt joi = OptionConverters.toJavaOptionalInt(oi());
        assertEquals(1, joi.getAsInt());
        Option<Integer> soi = OptionConverters.toScala(joi);
        assertEquals(oi(), soi);
    }
}
