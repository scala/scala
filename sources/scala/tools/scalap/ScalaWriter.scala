package scalap;

import java.io._;
import scala.collection.mutable._;


class ScalaWriter(writer: Writer) extends CodeWriter(writer) {

    def printFlags(flags: Int): Unit = {
    	val buffer = new StringBuffer();
		var x: StringBuffer = buffer;
		if (Flags.isPrivate(flags))
			x = buffer.append("private ");
		if (Flags.isProtected(flags))
			x = buffer.append("protected ");
		if (Flags.isAbstract(flags) && !Flags.isTrait(flags))
			x = buffer.append("abstract ");
		if (Flags.isFinal(flags) && !Flags.isObj(flags))
			x = buffer.append("final ");
		if (Flags.isSealed(flags))
			x = buffer.append("sealed ");
		if (Flags.isCase(flags))
			x = buffer.append("case ");
		if (Flags.isDef(flags))
			x = buffer.append("def ");
		if (Flags.isOverride(flags))
			x = buffer.append("override ");
		print(buffer.toString())*
    }

    def printType(tpe: Type): Unit = {
    	printType0(tpe);
    	tpe match {
    		case ThisType(_) => print(".type")*
    		case SingletonType(_, _) => print(".type")*
    		case _ =>
    	}
    }

	def printTypes(tpes: List[Type], begin: String, infix: String, end: String): Unit = {
	    if (!tpes.isEmpty)
	        printTypes0(tpes, begin, infix, end);
	}

	def printTypes0(tpes: List[Type], begin: String, infix: String, end: String): Unit = {
		print(begin)*;
	    if (!tpes.isEmpty) {
	    	printType(tpes.head);
	      	tpes.tail foreach (t => { print(infix)*; printType(t) });
	    }
	    print(end)*;
	}

	def printType0(tpe: Type): Unit = tpe match {
	    case NoType =>
	    case ThisType(sym) => sym match {
	    	case x: ExternalSymbol => print(sym.fullname)*
	    	case NoSymbol => print("this")*
	    	case _ => print(sym.fullname).print(".this")*
	    }
	    case SingletonType(tpe, sym) =>
	    	printPrefix(tpe);
	    	print(sym.name)*
	    case TypeRef(tpe, sym, args) =>
	    	printPrefix(tpe);
	    	print(sym.name)*;
	    	printTypes(args, "[", ", ", "]");
	    case CompoundType(clazz, components) =>
	        printTypes(components, "", " with ", "");
	        if (clazz != NoSymbol)
	            printScope(clazz.members);
	    case MethodType(_, _) =>
	        var tpe0 = tpe;
	        while (tpe0.isInstanceOf[MethodType]) {
	      		tpe0 match {
	      			case MethodType(argtpes, restpe) =>
	        			printTypes0(argtpes, "(", ", ", ")");
	        			tpe0 = restpe;
	        	}
	        }
	        print(":").newspace*;
	        printType(tpe0);
	    case PolyType(tpe, tvars) =>
	        print("[")*;
	        if (!tvars.isEmpty) {
	            printTVar(tvars.head);
	            tvars.tail foreach (sym => {print(", ")*; printTVar(sym);});
	        }
	        print("]")*;
	        printType(tpe);
	   	case OverloadedType(_, tpes) =>
	   	    printTypes(tpes, "", " <and> ", "");
	   	case TypeFlag(TypeRef(_, _, List(tpe0)), flags) =>
	   	    if ((flags & 8) != 0)
	   	    	print("def ")*;
	   	    printType(tpe0);
	   	    if ((flags & 4) != 0)
	   	    	print("*")*;
	   	case TypeFlag(tpe0, flags) =>
	   	    if ((flags & 8) != 0)
	   	    	print("def ")*;
	   	    printType(tpe0);
	   	case _ => print("<unknown type>")*;
	}

	def printPrefix(tpe: Type): Unit = tpe match {
	    case NoType =>
	    case ThisType(NoSymbol) =>
	    case ThisType(sym) =>
	    	if (sym.name.length() != 0) {
	    		printType0(tpe);
	    		print(".")*
	    	}
	    case TypeRef(_, _, _) =>
	    	printType0(tpe);
	    	print("#")*
	    case _ =>
	    	printType0(tpe);
	    	print(".")*
	}

	def printTVar(tvar: Symbol): Unit = tvar match {
		case sym: TypeSymbol => print(sym.name);
		                        if (!isExternalType(sym.tpe, "Any")) {
		                        	print(" <: ")*;
		                        	printType(sym.tpe);
		                        }
		                        if (!isExternalType(sym.lower, "All")) {
		                        	print(" >: ")*;
		                        	printType(sym.lower);
		                        }
	}

	def isExternalType(tpe: Type, name: String): Boolean = tpe match {
		case TypeRef(SingletonType(ThisType(root), pck), sym, Nil) =>
			(root.name.length() == 0) &&
			pck.name.equals("scala") &&
			sym.name.equals(name)
		case _ => false
	}

	def printSymbol(sym: Symbol): Unit = sym match {
		case NoSymbol =>
			print("<nosymbol>")*
		case s: TypeSymbol =>
		    print(Flags.toString(s.flags))*;
			print("type ")*;
			printTVar(s);
		case s: AliasSymbol =>
			print(Flags.toString(s.flags))*;
		    print("type " + s.name + " = ")*;
		    printType(s.tpe);
		case s: ClassSymbol =>
			print(Flags.toString(s.flags))*;
			if (Flags.isDeferred(s.flags))
			    print("/*deferred*/ ")*;
			if (Flags.isObj(s.flags))
				print("object " + s.name);
			else if (Flags.isTrait(s.flags))
			    print("trait " + s.name)*;
			else
			    print("class " + s.name)*;
			printConstr(s.constr);
			print(" extends ");
			printType(s.tpe);
		case s: ValSymbol =>
		    s.tpe match {
		    	case PolyType(tpe, Nil) =>
		    		print("def " + s.name + ": ")*;
		    		printType(tpe);
		    	case PolyType(_, _) =>
		    	    print("def " + s.name)*;
		    	    printType(s.tpe);
		   		case MethodType(_, _) =>
		   		    print("def " + s.name)*;
		   		    printType(s.tpe);
		   		case _ =>
		   		    print("val " + s.name + ": ")*;
		   		    printType(s.tpe);
		    }
		case s: ExternalSymbol =>
			print("<externalsymbol: " + s.fullname + ">")*
	}

	def printConstr(sym: Symbol): Unit = sym match {
		case s: ValSymbol =>
		    s.tpe match {
		    	case PolyType(MethodType(argtpes, _), tvars) =>
		    	    print("[")*;
					if (!tvars.isEmpty) {
						printTVar(tvars.head);
						tvars.tail foreach (sym => {print(", ")*; printTVar(sym);});
					}
					print("]")*;
		    	    printTypes(argtpes, "(", ", ", ")");
		   		case MethodType(argtpes, _) =>
		   		    printTypes(argtpes, "(", ", ", ")");
		   		case _ =>
		    }
	}

    def printScope(scope: Buffer[Symbol]): Unit = {
        var first = true;
        scope.elements foreach (
        	sym => { sym match {
        	             case s: ValSymbol if
        	             	(s.tpe.isInstanceOf[OverloadedType] ||
        	             	 (Flags.isCaseAccessor(s.flags) &&
        	             	  !s.tpe.isInstanceOf[PolyType])) =>
        	             case _ =>
        	             	if (first) print(" {").indent* else print(";")*;
        	         		first = false;
        	         		newline*;
        	         		printSymbol(sym);
        	         }});
        if (!first)
            newline.undent.print("}")*
    }
}
