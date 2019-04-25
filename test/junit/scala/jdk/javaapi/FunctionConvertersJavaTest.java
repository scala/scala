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
import scala.Function2;
import scala.runtime.BoxedUnit;

import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.ObjDoubleConsumer;
import static org.junit.Assert.*;

public class FunctionConvertersJavaTest {
    Function2<String, String, BoxedUnit> sf = (x, y) -> BoxedUnit.UNIT;
    Function2<Double, Double, BoxedUnit> df = (x, y) -> BoxedUnit.UNIT;

    @Test
    public void converterTests() {
        BiFunction<String, String, BoxedUnit> sfbf = FunctionConverters.asJavaBiFunction(sf);
        sfbf.apply("", "");
        BiConsumer<String, String> sfbc = FunctionConverters.asJavaBiConsumer(sf);
        sfbc.accept("", "");

        assertSame(sf, FunctionConverters.asScalaFromBiFunction(sfbf));
        assertSame(sf, FunctionConverters.asScalaFromBiConsumer(sfbc));

        BiFunction<Double, Double, BoxedUnit> dfbf = FunctionConverters.asJavaBiFunction(df);
        dfbf.apply(1d, 2d);
        BiConsumer<Double, Double> dfbc = FunctionConverters.asJavaBiConsumer(df);
        dfbc.accept(1d, 2d);
        ObjDoubleConsumer<Double> dfodc = FunctionConverters.asJavaObjDoubleConsumer(df);
        dfodc.accept(1d, 2d);

        assertSame(df, FunctionConverters.asScalaFromBiFunction(dfbf));
        assertSame(df, FunctionConverters.asScalaFromBiConsumer(dfbc));
        assertSame(df, FunctionConverters.asScalaFromObjDoubleConsumer(dfodc));
    }

}
