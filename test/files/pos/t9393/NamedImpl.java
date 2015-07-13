/*
 * Copyright (C) 2009-2015 Typesafe Inc. <http://www.typesafe.com>
 */
package bug;

import bug.Named;
import java.io.Serializable;
import java.lang.annotation.Annotation;

public class NamedImpl implements Named {

    public Class<? extends Annotation> annotationType() {
        return null;
    }
}
