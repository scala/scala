package ch.epfl.lamp.compiler.msil;

import ch.epfl.lamp.compiler.msil.util.PECustomMod;

public final class PrimitiveType extends Type {
    public PrimitiveType(Module module,
                         int attributes,
                         String fullName,
                         Type baseType,
                         Type[] interfaces,
                         Type declType,
                         int auxAttr,
                         Type elemType) {
        super(module, attributes, fullName,
                baseType, interfaces, declType, auxAttr, elemType);
        clearMembers();
    }

    public void clearMembers() {
        fields = FieldInfo.EMPTY_ARRAY;
        methods = MethodInfo.EMPTY_ARRAY;
        constructors = ConstructorInfo.EMPTY_ARRAY;
        events = EventInfo.EMPTY_ARRAY;

        initBaseType();
        initInterfaces();

        initFields();
        initMethods();
        initEvents();
        initProperties();
        initNestedTypes();
    }

    public FieldInfo addField(String name, int attrs, Type fieldType) {
        PECustomMod fieldTypeWithMods = new PECustomMod(fieldType, null);
        FieldInfo res = new FieldInfo(name, this, attrs, fieldTypeWithMods, null);
        FieldInfo[] ms = new FieldInfo[fields.length + 1];
        System.arraycopy(fields, 0, ms, 0, fields.length);
        ms[ms.length - 1] = res;
        fields = ms;
        return res;
    }

    public MethodInfo addMethod(String name, int attrs, Type returnType, Type[] paramTypes) {
        MethodInfo res = new MethodInfo(name, this, attrs, returnType, paramTypes);
        MethodInfo[] ms = new MethodInfo[methods.length + 1];
        System.arraycopy(methods, 0, ms, 0, methods.length);
        ms[ms.length - 1] = res;
        return res;
    }

    public ConstructorInfo addConstructor(int attrs, Type[] paramTypes) {
        ConstructorInfo res = new ConstructorInfo(this, attrs, paramTypes);
        ConstructorInfo[] ms = new ConstructorInfo[constructors.length + 1];
        System.arraycopy(constructors, 0, ms, 0, constructors.length);
        ms[ms.length - 1] = res;
        return res;
    }

}

