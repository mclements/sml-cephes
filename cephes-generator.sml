(* 

Using a list of custom C header definitions, write out a signature and
structures for Poly/ML and MLton

Mark Clements
2020-05-04
BSD license
*)

signature CEPHES_GENERATOR = sig
    val main : unit -> unit;
end;

structure Cephes_generator :> CEPHES_GENERATOR =
struct

(* Comment out in Poly/ML if you have opened Foreign *)
datatype cvarType = cDouble
		  | cInt
		  | cVoid
		  | cStar of cvarType
		  | cStruct2 of (cvarType *cvarType)
		  | cArray of cvarType
		  | cVector of cvarType;

datatype varAttributes = Lower of real
       | Upper of real;
datatype ioVarType = Invar | Outvar | InOutvar;
datatype varType = tDouble
		 | tInt
		 | tComplex
		 | tVoid
		 | tExpr of varType
		 | tStar of (varType * ioVarType) 
		 | tArray of (varType * string) (* assumed to be output only *)
		 | tVector of varType; (* can only be input *)
type definition = {name : string, description : string, prefix : string,
		   retType : varType,
		   vars : (string * varType) list};
exception Unmatched;
val cComplex = cStruct2(cDouble, cDouble);
type cmplx = real * real;

fun varType tDouble = "real"
  | varType tInt = "int"
  | varType tComplex = "cmplx"
  | varType tVoid = "void"
  | varType (tExpr t) = varType t
  | varType (tStar (t, _)) = varType t (* drop ref *)
  | varType (tArray (t, _)) = (varType t) ^ "  Array.array"
  | varType (tVector t) = (varType t) ^ " Vector.vector";
fun outVarType tDouble = "real"
  | outVarType tInt = "int"
  | outVarType tComplex = "cmplx"
  | outVarType tVoid = "unit"
  | outVarType (tStar (t, _)) = outVarType t
  | outVarType (tArray (t, _)) = (outVarType t) ^ " Array.array"
  | outVarType (tVector t) = (outVarType t) ^" Vector.vector"
  | outVarType _ = raise Unmatched;
fun input tDouble = true
  | input tInt = true
  | input tComplex = true
  | input tVoid = true (* corner case: no arguments *)
  | input (tExpr _) = false
  | input (tStar (_, Invar)) = true
  | input (tStar (_, InOutvar)) = true
  | input (tStar (_, Outvar)) = false
  | input (tArray (_, _)) = false
  | input (tVector _) = true;
fun output tDouble = false
  | output tInt = false
  | output tComplex = false
  | output tVoid = false (* corner case: no arguments *)
  | output (tExpr _) = false
  | output (tStar (_, Invar)) = false
  | output (tStar (_, InOutvar)) = true
  | output (tStar (_, Outvar)) = true
  | output (tArray (_, _)) = true
  | output (tVector _) = false;
fun join sep lst = List.foldl (fn (item,agg) => agg ^ sep ^ item) (hd lst) (tl lst);
val commasep = join ", ";
val starsep = join " * ";
val nl = "\n";
val quote = "\"";

fun writePolyStructure (def : definition) =
    let
	fun cTyp tDouble = "cDouble"
	  | cTyp tInt = "cInt"
	  | cTyp tComplex = "cComplex"
	  | cTyp tVoid = "cVoid"
	  | cTyp (tExpr t) = cTyp t
	  | cTyp (tStar (t, _)) = "cStar " ^ (cTyp t)
	  | cTyp (tArray (t, _)) = "cArrayPointer " ^ (cTyp t)
	  | cTyp (tVector t) = "cVectorPointer " ^ (cTyp t)
	fun vheader (var : string * varType) = (#1 var) ^ " : " ^ (varType (#2 var))
	fun quoted x = "\"" ^ x "\""
	val vars = #vars def
	fun getVarType name = hd (List.filter (fn item => #1(item) = name) vars) 
	val invars = List.filter (fn item => input (#2 item)) vars
	val outvars = List.filter (fn item => output (#2 item)) vars
	val args = case invars of [] => "" 
			       | x => commasep (List.map vheader x)
	val args3 = commasep
			(List.map (fn (name,typ) => case typ of
							tStar (_, Invar) => "ref " ^ name
						      | tStar (_, InOutvar) => "ref_" ^ name
						      | _ => name)
			     vars)
	val n = List.length vars
	val descrvars = case (outvars, #retType def) of
			    ([], t) => outVarType t
			  | (hd :: [] , tVoid) => (outVarType o #2) hd
			  | (lst, tVoid) => "(" ^ (commasep (List.map (outVarType o #2) lst)) ^ ")"
			  | (lst, t) => "(" ^ (commasep ((varType t) :: (List.map (outVarType o #2) lst))) ^ ")"
	val description =
	    "(* " ^ (#description def) ^ ". " ^ (#name def) ^ "(" ^ args ^ ") : " ^ descrvars ^ " *)"
	val call = "val call = buildCall" ^ Int.toString(n) ^
		   "(getSymbol lib " ^ quote ^ (#prefix def) ^ (#name def) ^ quote ^
		   ", (" ^ (commasep (List.map cTyp (List.map #2 vars))) ^ ")" ^ ", "
		   ^ (cTyp (#retType def)) ^ ")"
	val letStmts =
	    List.map (fn item => case (#2 item) of
				     tStar (_, InOutvar) => "      val ref_" ^ (#1 item) ^ " = ref " ^ (#1 item)
				   | tStar (tDouble,_) => "      val " ^ (#1 item) ^ " = ref 0.0"
				   | tStar (tInt,_) => "      val " ^ (#1 item) ^ " = ref 0"
				   | tStar (tComplex,_) => "      val " ^ (#1 item) ^ " = ref (0.0,0.0)"
				   | tArray (tDouble, name) => "      val " ^ (#1 item) ^ " = Array.array(" ^ name ^ ", 0.0)"
				   | tArray (tInt, name) => "      val " ^ (#1 item) ^ " = Array.array(" ^ name ^ ", 0)"
				   | _ => raise Unmatched) outvars
	val outExprs = List.map (fn item => case (#2 item) of
						tStar (_,InOutvar) => "! ref_" ^ (#1 item) 
					      | tStar (_,Outvar) => "! " ^ (#1 item) 
					      | tArray (_, _) => (#1 item)
					      | _ => raise Unmatched) outvars
	val outExprs = case #retType(def) of tVoid => outExprs | _ =>  "zzz123" :: outExprs
	val outExprs = if (List.length outExprs)=1 then hd outExprs else "(" ^ (commasep outExprs) ^ ")"
	val z = if (#retType def) = tVoid then "_" else "zzz123"
	val body = case outvars of [] => " call(" ^ args3 ^ ")\n"
				 | _ => 
				   ("\n    let\n" ^ (join "\n" letStmts) ^ "\n      val " ^ z ^ " = call(" ^ args3 ^ ")\n    in\n      " ^ outExprs ^ "\n    end\n")
    in
	description ^ nl ^
	"local\n  " ^ call ^ "\nin\n" ^ 
	"  fun " ^ #name(def) ^ "(" ^ args ^ ") =" ^ body ^ "end;\n"
    end;
(* (print o writePolyStructure) {name="chbevl", description="chbevl", prefix="", *)
(* 			   retType=tDouble, *)
(* 			   vars=[("x", tDouble), ("coef", tVector tDouble), ("Vector.length coef", tExpr tInt)]}; *)

fun writeSignature (def : definition) =
    let
	fun vheader (var : string * varType) = (#1 var) ^ " : " ^ (varType (#2 var))
	val vars = #vars def
	fun getVarType name = hd (List.filter (fn item => #1(item) = name) vars) 
	val invars = List.filter (input o #2) vars
	val outvars = List.filter (output o #2) vars
	val args = case invars of [] => "" 
			       | x => commasep (List.map vheader x)
	val starargs = case invars of [] => "unit" 
			       | x => starsep (List.map (varType o #2) x)
	val descrvars = case (outvars, #retType def) of
			    ([], t) => outVarType t
			  | (hd :: [] , tVoid) => (outVarType o #2) hd
			  | (lst, tVoid) => "(" ^ (commasep (List.map (outVarType o #2) lst)) ^ ")"
			  | (lst, t) => "(" ^ (commasep ((varType t) :: (List.map (outVarType o #2) lst))) ^ ")"
	val valvars = case (outvars, #retType def) of
			    ([], t) => outVarType t
			  | (hd :: [] , tVoid) => (outVarType o #2) hd
			  | (lst, tVoid) => starsep (List.map (outVarType o #2) lst)
			  | (lst, t) => (starsep ((varType t) :: (List.map (outVarType o #2) lst)))
    in
	"(* " ^ (#description def) ^ ". " ^ (#name def) ^ "(" ^ args ^ ") : " ^ descrvars ^ " *)" ^
	    nl ^ "val " ^ (#name def) ^ " : " ^ starargs ^ " -> " ^ valvars ^ nl 
    end;

fun writeSmlsharpSmiStructure (def : definition) =
    let
	fun vheader (var : string * varType) = (#1 var) ^ " : " ^ (varType (#2 var))
	val vars = #vars def
	fun getVarType name = hd (List.filter (fn item => #1(item) = name) vars) 
	val invars = List.filter (input o #2) vars
	val outvars = List.filter (output o #2) vars
	val args = case invars of [] => "" 
			       | x => commasep (List.map vheader x)
	val starargs = case invars of [] => "unit" 
			       | x => starsep (List.map (varType o #2) x)
	val descrvars = case (outvars, #retType def) of
			    ([], t) => outVarType t
			  | (hd :: [] , tVoid) => (outVarType o #2) hd
			  | (lst, tVoid) => "(" ^ (commasep (List.map (outVarType o #2) lst)) ^ ")"
			  | (lst, t) => "(" ^ (commasep ((varType t) :: (List.map (outVarType o #2) lst))) ^ ")"
	val valvars = case (outvars, #retType def) of
			    ([], t) => outVarType t
			  | (hd :: [] , tVoid) => (outVarType o #2) hd
			  | (lst, tVoid) => starsep (List.map (outVarType o #2) lst)
			  | (lst, t) => (starsep ((varType t) :: (List.map (outVarType o #2) lst)))
    in
	"(* " ^ (#description def) ^ ". " ^ (#name def) ^ "(" ^ args ^ ") : " ^ descrvars ^ " *)" ^
	    nl ^ "val " ^ (#name def) ^ " : " ^ starargs ^ " -> " ^ valvars ^ nl 
    end;

(* changes from writePolyStructure: cTyp, call *)
fun writeMltonStructure (def : definition) =
    let
	fun cTyp tDouble = "real"
	  | cTyp tInt = "int"
	  | cTyp tComplex = "cmplx"
	  | cTyp tVoid = "unit"
	  | cTyp (tExpr t) = cTyp t
	  | cTyp (tStar (t, _)) = (cTyp t) ^ " ref" (* keep ref *)
	  | cTyp (tArray (t, _)) = (cTyp t) ^ " Array.array"
	  | cTyp (tVector t) = (cTyp t) ^ " Vector.vector"
	fun vheader (var : string * varType) = (#1 var) ^ " : " ^ (varType (#2 var))
	fun join sep lst = List.foldl (fn (item,agg) => agg ^ sep ^ item) (hd lst) (tl lst)
	val vars = #vars def
	fun getVarType name = hd (List.filter (fn item => #1(item) = name) vars) 
	val invars = List.filter (fn item => input (#2 item)) vars
	val outvars = List.filter (fn item => output (#2 item)) vars
	val args = case invars of [] => "" 
			       | x => commasep (List.map vheader x)
	val args3 = commasep
			(List.map (fn (name,typ) => case typ of
							tStar (_, Invar) => "ref " ^ name
						      | tStar (_, InOutvar) => "ref_" ^ name
						      | _ => name)
			     vars)
	val n = List.length vars
	val descrvars = case (outvars, #retType def) of
			    ([], t) => outVarType t
			  | (hd :: [] , tVoid) => (outVarType o #2) hd
			  | (lst, tVoid) => "(" ^ (commasep (List.map (outVarType o #2) lst)) ^ ")"
			  | (lst, t) => "(" ^ (commasep ((varType t) :: (List.map (outVarType o #2) lst))) ^ ")"
	val description =
	    "(* " ^ (#description def) ^ ". " ^ (#name def) ^ "(" ^ args ^ ") : " ^ descrvars ^ " *)"
	val call = "val call = _import " ^ quote ^ (#prefix def) ^ (#name def) ^ quote ^ " public: " ^
		   (starsep (List.map cTyp (List.map #2 vars))) ^ " -> " ^ (cTyp (#retType def)) ^ ";"
	val letStmts =
	    List.map (fn item => case (#2 item) of
				     tStar (_, InOutvar) => "      val ref_" ^ (#1 item) ^ " = ref " ^ (#1 item)
				   | tStar (tDouble,_) => "      val " ^ (#1 item) ^ " = ref 0.0"
				   | tStar (tInt,_) => "      val " ^ (#1 item) ^ " = ref 0"
				   | tStar (tComplex,_) => "      val " ^ (#1 item) ^ " = ref (0.0,0.0)"
				   | tArray (tDouble, name) => "      val " ^ (#1 item) ^ " = Array.array(" ^ name ^ ", 0.0)"
				   | tArray (tInt, name) => "      val " ^ (#1 item) ^ " = Array.array(" ^ name ^ ", 0)"
				   | _ => raise Unmatched) outvars
	val outExprs = List.map (fn item => case (#2 item) of
						tStar (_,InOutvar) => "! ref_" ^ (#1 item) 
					      | tStar (_,Outvar) => "! " ^ (#1 item) 
					      | tArray (_, _) => (#1 item)
					      | _ => raise Unmatched) outvars
	val outExprs = case #retType(def) of tVoid => outExprs | _ =>  "zzz123" :: outExprs
	val outExprs = if (List.length outExprs)=1 then hd outExprs else "(" ^ (commasep outExprs) ^ ")"
													 val z = if (#retType def) = tVoid then "_" else "zzz123"
	val body = case outvars of [] => " call(" ^ args3 ^ ")\n"
				 | _ => 
				   ("\n    let\n" ^ (join "\n" letStmts) ^ "\n      val " ^ z ^ " = call(" ^ args3 ^ ")\n    in\n      " ^ outExprs ^ "\n    end\n")
    in
	description ^ nl ^
	"local\n  " ^ call ^ "\nin\n" ^ 
	"  fun " ^ #name(def) ^ "(" ^ args ^ ") =" ^ body ^ "end;\n"
    end;

(* changes from writeMltonStructure: call *)
fun writeSmlsharpStructure (def : definition) =
    let
	fun cTyp tDouble = "real"
	  | cTyp tInt = "int"
	  | cTyp tComplex = "cmplx"
	  | cTyp tVoid = "()"
	  | cTyp (tExpr t) = cTyp t
	  | cTyp (tStar (tComplex, Invar)) = "cmplx"
	  | cTyp (tStar (tComplex, InOutvar)) = "real array"
	  | cTyp (tStar (tComplex, Outvar)) = "real array"
	  | cTyp (tStar (t, _)) = (cTyp t) ^ " ref" (* keep ref *)
	  | cTyp (tArray (t, _)) = (cTyp t) ^ " Array.array"
	  | cTyp (tVector t) = (cTyp t) ^ " Vector.vector"
	fun vheader (var : string * varType) = (#1 var) ^ " : " ^ (varType (#2 var))
	fun join sep lst = List.foldl (fn (item,agg) => agg ^ sep ^ item) (hd lst) (tl lst)
	val vars = #vars def
	fun getVarType name = hd (List.filter (fn item => #1(item) = name) vars) 
	val invars = List.filter (fn item => input (#2 item)) vars
	val outvars = List.filter (fn item => output (#2 item)) vars
	val args = case invars of [] => "" 
			       | x => commasep (List.map vheader x)
	val args3 = commasep
			(List.map (fn (name,typ) => case typ of
							tStar (_, Invar) => name
						      | tStar (tComplex, InOutvar) => "arr_" ^ name
						      | tStar (_, InOutvar) => "ref_" ^ name
						      | _ => name)
			     vars)
	val n = List.length vars
	val descrvars = case (outvars, #retType def) of
			    ([], t) => outVarType t
			  | (hd :: [] , tVoid) => (outVarType o #2) hd
			  | (lst, tVoid) => "(" ^ (commasep (List.map (outVarType o #2) lst)) ^ ")"
			  | (lst, t) => "(" ^ (commasep ((varType t) :: (List.map (outVarType o #2) lst))) ^ ")"
	val description =
	    "(* " ^ (#description def) ^ ". " ^ (#name def) ^ "(" ^ args ^ ") : " ^ descrvars ^ " *)"
	val call = "val call = _import " ^ quote ^ (#prefix def) ^ (#name def) ^ quote ^ " : (" ^
		   (commasep (List.map cTyp (List.map #2 vars))) ^ ") -> " ^ (cTyp (#retType def)) ^ ";"
	val letStmts =
	    List.map (fn item => case (#2 item) of
				     tStar (tComplex, Outvar) => "      val " ^ (#1 item) ^ " = Array.array (2, 0.0)"
				   | tStar (tComplex, InOutvar) => "      val arr_" ^ (#1 item) ^ " = Array.fromList [Array.sub(" ^ (#1 item) ^ ", 0), Array.sub(" ^  (#1 item) ^ ", 1)]"
				   | tStar (_, InOutvar) => "      val ref_" ^ (#1 item) ^ " = ref " ^ (#1 item)
				   | tStar (tDouble,_) => "      val " ^ (#1 item) ^ " = ref 0.0"
				   | tStar (tInt,_) => "      val " ^ (#1 item) ^ " = ref 0"
				   | tArray (tDouble, name) => "      val " ^ (#1 item) ^ " = Array.array(" ^ name ^ ", 0.0)"
				   | tArray (tInt, name) => "      val " ^ (#1 item) ^ " = Array.array(" ^ name ^ ", 0)"
				   | _ => raise Unmatched) outvars
	val outExprs = List.map (fn item => case (#2 item) of
						tStar (tComplex,InOutvar) => "((Array.sub(" ^ (#1 item) ^ ", 0), Array.sub(" ^ (#1 item) ^ ", 1)) : cmplx)"
					      | tStar (tComplex,Outvar) => "((Array.sub(" ^ (#1 item) ^ ", 0), Array.sub(" ^ (#1 item) ^ ", 1)) : cmplx)"
					      |  tStar (_,InOutvar) => "! ref_" ^ (#1 item) 
					      | tStar (_,Outvar) => "! " ^ (#1 item) 
					      | tArray (_, _) => (#1 item)
					      | _ => raise Unmatched) outvars
	val outExprs = case #retType(def) of tVoid => outExprs | _ =>  "zzz123" :: outExprs
	val outExprs = if (List.length outExprs)=1 then hd outExprs else "(" ^ (commasep outExprs) ^ ")"
													 val z = if (#retType def) = tVoid then "_" else "zzz123"
	val body = case outvars of [] => " call(" ^ args3 ^ ")\n"
				 | _ => 
				   ("\n    let\n" ^ (join "\n" letStmts) ^ "\n      val " ^ z ^ " = call(" ^ args3 ^ ")\n    in\n      " ^ outExprs ^ "\n    end\n")
    in
	description ^ nl ^
	"local\n  " ^ call ^ "\nin\n" ^ 
	"  fun " ^ #name(def) ^ "(" ^ args ^ ") =" ^ body ^ "end;\n"
    end;
(* (print o writeSmlsharpStructure) {name="plus", description="cplus", prefix="md_", *)
(* 			   retType=tVoid, *)
(* 			   vars=[("x", tStar (tComplex,Invar)), ("y", tStar (tComplex,Outvar))]}; *)
(* (print o writeSmlsharpStructure) {name="frexp", description="frexp", prefix="", *)
(* 				  retType=tDouble, *)
(* 				  vars=[("x",tDouble),("expnt",tStar (tInt,InOutvar))]} *)

local
    fun dd(name, description) = {name=name, description=description, prefix="",
				 retType=tDouble,
				 vars=[("x", tDouble)]}
    fun ddp(name, description) = {name=name, description=description, prefix="md_",
				 retType=tDouble,
				 vars=[("x", tDouble)]}
    fun dddp(name, description) = {name=name, description=description, prefix="md_",
				  retType=tDouble,
				  vars=[("x", tDouble), ("y", tDouble)]}
    fun ccp(name, description) =
	{name=name, description=description, prefix="md_",
	 retType=tVoid,
	 vars=[("x", tStar (tComplex, Invar)), ("y", tStar (tComplex, Outvar))]}
    fun cc(name, description) =
	{name=name, description=description, prefix="",
	 retType=tVoid,
	 vars=[("x", tStar (tComplex, Invar)), ("y", tStar (tComplex, Outvar))]}
    fun cccp(name, description) =
	{name=name, description=description, prefix="md_",
	 retType=tVoid,
	 vars=[("x", tStar (tComplex, Invar)), ("y", tStar (tComplex, Invar)),
	       ("z", tStar (tComplex, Outvar))]}
    fun ccc(name, description) =
	{name=name, description=description, prefix="",
	 retType=tVoid,
	 vars=[("x", tStar (tComplex, Invar)), ("y", tStar (tComplex, Invar)),
	       ("z", tStar (tComplex, Outvar))]}
in
val defs = [ddp("acosh","Inverse hyperbolic cosine"),
	    {name="airy", description="Airy function", prefix="",
	     retType=tInt,
	     vars=[("x", tDouble),
		   ("ai", tStar (tDouble, Outvar)),
		   ("aip", tStar (tDouble, Outvar)),
		   ("bi", tStar (tDouble, Outvar)),
		   ("bip", tStar (tDouble, Outvar))]},
	    ddp("asin", "Inverse circular sine"),
	    ddp("acos", "Inverse circular cosine"),
	    ddp("asinh", "Inverse hyperbolic sine"),
	    ddp("atan", "Inverse circular tangent (arctangent)"),
	    {name="atan2", description="Quadrant correct inverse circular tangent", prefix="md_",
	     retType=tDouble,
	     vars=[("y", tDouble), ("x", tDouble)]},
	    ddp("atanh","Inverse hyperbolic tangent"),
	    {name="bdtr", description="Binomial distribution", prefix="", retType=tDouble,
	     vars=[("k", tInt), ("n", tInt), ("p", tDouble)]},
	    {name="bdtrc", description="Complemented binomial distribution",
	     prefix="", retType=tDouble,
	     vars=[("k", tInt), ("n", tInt), ("p", tDouble)]},
	    {name="bdtri", description="Inverse binomial distribution", prefix="", retType=tDouble,
	     vars=[("k", tInt), ("n", tInt), ("y", tDouble)]},
	    {name="beta", description="Beta function", prefix="", retType=tDouble,
	     vars=[("a", tDouble), ("b", tDouble)]},
	    {name="btdtr", description="Beta distribution", prefix="", retType=tDouble,
	     vars=[("a", tDouble), ("b", tDouble), ("x", tDouble)]},
	    ddp("cbrt", "Cube root"),
	    {name="chbevl", description="Evaluate Chebyshev series", prefix="",
	     retType=tDouble,
	     vars=[("x", tDouble), ("coef", tVector tDouble), ("Vector.length coef", tExpr tInt)]},
	    {name="chdtr", description="Chi-square distribution", prefix="",
	     retType=tDouble,
	     vars=[("df", tDouble), ("x", tDouble)]},
	    {name="chdtrc", description="Complemented Chi-square distribution", prefix="",
	     retType=tDouble,
	     vars=[("df", tDouble), ("x", tDouble)]},
	    {name="chdtri", description="Inverse of complemented Chi-square distribution", prefix="",
	     retType=tDouble,
	     vars=[("df", tDouble), ("x", tDouble)]},
	    ccp("clog", "Complex natural logarithm"), 
	    ccp("cexp", "Complex exponential function"),
	    ccp("csin", "Complex circular sine"),
	    ccp("ccos", "Complex circular cosine"),
	    ccp("ctan", "Complex circular tangent"),
	    cc("ccot", "Complex circular cotangent"),
	    ccp("casin", "Complex circular arc sine"),
	    ccp("cacos", "Complex circular arc cosine"),
	    ccp("catan", "Complex circular arc tangent"),
	    ccp("csinh", "Complex hyperbolic sine"),
	    ccp("ccosh", "Complex hyperbolic cosine"),
	    ccp("cacosh", "Complex inverse hyperbolic cosine"),
	    ccp("ctanh", "Complex hyperbolic tangent"),
	    ccp("catanh", "Complex inverse hyperbolic tangent"),
	    cccp("cpow", "Complex power function"),
	    ccc("cadd", "Complex addition"),
	    ccc("csub", "Complex subtraction"),
	    ccc("cmul", "Complex multiplication"),
	    ccc("cdiv", "Complex division"),
	    (* {name="cneg", description="Complex negative", prefix="md_", *)
	    (*  retType=tVoid, *)
	    (*  vars=[("c", tStar (tComplex, InOutvar))]}, *)
	    (* {name="cmov", description="Complex move", prefix="md_", *)
	    (*  retType=tVoid, *)
	    (*  vars=[("b", tStar (tComplex, Invar)), ("c", tStar (tComplex, InOutvar))]}, *)
	    ccp("cabs", "Complex absolute value"),
	    ccp("csqrt", "Complex square root"),
	    ddp("cosh","Hyperbolic cosine"),
	    dd("dawsn", "Dawson's integral"),
	    {name="drand", description="Pseudorandom number generator", prefix="",
	     retType=tVoid,
	     vars=[("y", tStar (tDouble, Outvar))]},
	    dd("ei","Exponential integral"),
	    {name="ellie", description="Incomplete elliptic integral of the second kind", prefix="",
	     retType=tDouble, vars=[("phi", tDouble),("m", tDouble)]},
	    {name="ellik", description="Incomplete elliptic integral of the first kind", prefix="",
	     retType=tDouble, vars=[("phi", tDouble),("m", tDouble)]},
	    {name="ellpe", description="Complete elliptic integral of the second kind", prefix="",
	     retType=tDouble, vars=[("m1", tDouble)]},
	    {name="ellpj", description="Jacobian Elliptic Functions", prefix="",
	     retType=tInt,
	     vars=[("u", tDouble), ("m", tDouble),
		   ("sn", tStar (tDouble, Outvar)),
		   ("cn", tStar (tDouble, Outvar)),
		   ("dn", tStar (tDouble, Outvar)),
		   ("phi", tStar (tDouble, Outvar))]},
	    {name="ellpk", description="Complete elliptic integral of the first kind", prefix="",
	     retType=tDouble, vars=[("m1", tDouble)]},
	    ddp("exp", "Exponential function"),
	    ddp("exp10", "Base 10 exponential function"),
	    ddp("exp2", "Base 2 exponential function"),
	    {name="expn", description="Exponential integral En", prefix="md_",
	     retType=tDouble, vars=[("n", tInt),("x",tDouble)]},
	    {name="expx2", description="Exponential of squared argument", prefix="",
	     retType=tDouble, vars=[("x", tDouble),("sign",tInt)]},
	    ddp("fabs","Absolute value"),
	    {name="fac", description="Factorial function", prefix="",
	     retType=tDouble, vars=[("i",tInt)]},
	    {name="fdtr", description="F distribution", prefix="",
	     retType=tDouble, vars=[("df1",tInt),("df2",tInt),("x",tDouble)]},
	    {name="fdtrc", description="Complemented F distribution", prefix="",
	     retType=tDouble, vars=[("df1",tInt),("df2",tInt),("x",tDouble)]},
	    {name="fdtri", description="Inverse of complemented F distribution", prefix="",
	     retType=tDouble, vars=[("df1",tInt),("df2",tInt),("p",tDouble)]},
	    ddp("floor","Floor function"),
	    ddp("ceil","Ceiling function"),
	    {name="frexp", description="frexp", prefix="",
	     retType=tDouble, vars=[("x",tDouble),("expnt",tStar (tInt,InOutvar))]},
	    {name="ldexp", description="ldexp", prefix="",
	     retType=tDouble, vars=[("x",tDouble),("n",tInt)]},
	    {name="fresnl", description="Fresnel integral", prefix="",
	     retType=tVoid, vars=[("x",tDouble),
				  ("S",tStar (tDouble,Outvar)),
				  ("C",tStar (tDouble,Outvar))]},
	    ddp("gamma","Gamma function"),
	    dd("lgam", "Natural logarithm of gamma function"),
	    {name="gdtr", description="Gamma distribution function", prefix="",
	     retType=tDouble, vars=[("a",tDouble), ("b",tDouble), ("x",tDouble)]},
	    {name="gdtrc", description="Complemented gamma distribution function", prefix="",
	     retType=tDouble, vars=[("a",tDouble), ("b",tDouble), ("x",tDouble)]},
	    {name="hyp2f1", description="Gauss hypergeometric function _2F_1", prefix="",
	     retType=tDouble, vars=[("a",tDouble), ("b",tDouble), ("c",tDouble), ("x",tDouble)]},
	    {name="hyperg", description="Confluent hypergeometric function", prefix="",
	     retType=tDouble, vars=[("a",tDouble), ("b",tDouble), ("x",tDouble)]},
	    dd("i0", "Modified Bessel function of order zero"),
	    dd("i0e", "Modified Bessel function of order zero, exponentially scaled"),
	    dd("i1", "Modified Bessel function of order one"),
	    dd("i1e", "Modified Bessel function of order one, exponentially scaled"),
	    {name="igam", description="Incomplete gamma integral", prefix="",
	     retType=tDouble, vars=[("a",tDouble),("x",tDouble)]},
	    {name="igamc", description="Complemented incomplete gamma integral", prefix="",
	     retType=tDouble, vars=[("a",tDouble),("x",tDouble)]},
	    {name="igami", description="Inverse of complemented incomplete gamma integral", prefix="",
	     retType=tDouble, vars=[("a",tDouble),("p",tDouble)]},
	    {name="incbet", description="Incomplete beta integral", prefix="",
	     retType=tDouble, vars=[("a",tDouble),("b",tDouble),("x",tDouble)]},
	    {name="incbi", description="Inverse of incomplete beta integral", prefix="",
	     retType=tDouble, vars=[("a",tDouble),("b",tDouble),("y",tDouble)]},
	    {name="signbit", description="signbit", prefix="",
	     retType=tInt, vars=[("x",tDouble)]},
	    {name="isnan", description="isnan", prefix="",
	     retType=tInt, vars=[("x",tDouble)]},
	    {name="isfinite", description="isfinite", prefix="",
	     retType=tInt, vars=[("x",tDouble)]},
	    {name="iv", description="Modified Bessel function of noninteger order", prefix="",
	     retType=tDouble, vars=[("v",tDouble),("x",tDouble)]},
	    dd("j0", "Bessel function of order zero"),
	    dd("y0", "Bessel function of second kind, order zero"),
	    dd("j1", "Bessel function of order one"),
	    dd("y1", "Bessel function of second kind of order one"),
	    {name="jn", description="Bessel function of integer order", prefix="",
	     retType=tDouble, vars=[("n",tInt),("x",tDouble)]},
	    {name="jv", description="Bessel function of noninteger order", prefix="",
	     retType=tDouble, vars=[("v",tDouble),("x",tDouble)]},
	    dd("k0", "Modified Bessel function, third kind, order zero"),
	    dd("k0e", "Modified Bessel function, third kind, order zero, exponentially scaled"),
	    dd("k1", "Modified Bessel function, third kind, order one"),
	    dd("k1e", "Modified Bessel function, third kind, order one, exponentially scaled"),
	    {name="kn", description="Modified Bessel function, third kind, integer order", prefix="",
	     retType=tDouble, vars=[("n",tInt), ("x",tDouble)]},
	    ddp("log","Natural logarithm"),
	    ddp("log10","Common logarithm"),
	    ddp("log2","Base 2 logarithm"),
	    (* lsqrt *)
	    {name="nbdtr", description="Negative binomial distribution", prefix="",
	     retType=tDouble, vars=[("k",tInt), ("n",tInt), ("p",tDouble)]},
	    {name="nbdtrc", description="Complemented negative binomial distribution", prefix="",
	     retType=tDouble, vars=[("k",tInt), ("n",tInt), ("p",tDouble)]},
	    {name="nbdtri", description="Functional inverse of negative binomial distribution", prefix="",
	     retType=tDouble, vars=[("k",tInt), ("n",tInt), ("y",tDouble)]},
	    dd("ndtr", "Normal distribution function"),
	    ddp("erf","Error function"),
	    ddp("erfc","Complementary error function"),
	    dd("ndtri", "Inverse of Normal distribution function"),
	    {name="pdtr", description="Poisson distribution function", prefix="",
	     retType=tDouble, vars=[("k",tInt),("m",tDouble)]},
	    {name="pdtrc", description="Complemented Poisson distribution", prefix="",
	     retType=tDouble, vars=[("k",tInt),("m",tDouble)]},
	    {name="pdtri", description="Inverse Poisson distribution", prefix="",
	     retType=tDouble, vars=[("k",tInt),("y",tDouble)]},
	    (* {name="planki", description="Integral of Planck's black body radiation formula", prefix="", *)
	    (*  retType=tDouble, vars=[("lambda",tDouble),("T",tDouble)]}, *)
	    {name="polevl", description="Evaluate polynomial", prefix="",
	     retType=tDouble,
	     vars=[("x", tDouble), ("coef", tVector tDouble), ("Vector.length coef", tExpr tInt)]},
	    (* polylog *)
	    dddp("pow","Power function"),
	    {name="powi", description="Real raised to integer power", prefix="md_",
	     retType=tDouble,
	     vars=[("x", tDouble), ("n", tInt)]},
	    dd("psi","Psi (digamma) function"),
	    (* revers *)
	    {name="rgamma", description="Reciprocal gamma function", prefix="",
	     retType=tDouble, vars=[("x", tDouble)]},
	    ddp("round", "Round double to nearest or even integer valued double"),
	    {name="shichi", description="Hyperbolic sine and cosine integrals", prefix="",
	     retType=tDouble,
	     vars=[("x", tDouble), ("Chi", tStar (tDouble,Outvar)), ("Shi", tStar (tDouble,Outvar))]},
	    {name="sici", description="Sine and cosine integrals", prefix="",
	     retType=tDouble,
	     vars=[("x", tDouble), ("Si", tStar (tDouble,Outvar)), ("Ci", tStar (tDouble,Outvar))]},
	    (* simq *)
	    ddp("sin","Circular sine"),
	    ddp("cos","Circular cosine"),
	    ddp("sindg","Circular sine of angle in degrees"),
	    dd("cosdg", "Circular cosine of angle in degrees"),
	    ddp("sinh","Hyperbolic sine"),
	    dd("spence", "Dilogarithm"),
	    dd("sqrt", "Square root"),
	    {name="stdtr", description="Student's t distribution", prefix="",
	     retType=tDouble,
	     vars=[("k", tInt), ("t", tDouble)]},
	    {name="stdtri", description="Functional inverse of Student's t distribution", prefix="",
	     retType=tDouble,
	     vars=[("k", tInt), ("p", tDouble)]},
	    {name="struve", description="Struve function", prefix="",
	     retType=tDouble,
	     vars=[("v", tInt), ("x", tDouble)]},
	    ddp("tan", "Circular tangent"),
	    dd("cot", "Circular cotangent"),
	    dd("tandg", "Circular tangent of argument in degrees"),
	    dd("cotdg", "Circular cotangent of argument in degrees"),
	    ddp("tanh", "Hyperbolic tangent"),
	    {name="yn", description="Bessel function of second kind of integer order", prefix="",
	     retType=tDouble,
	     vars=[("n", tInt), ("x", tDouble)]},
	    dd("zetac", "Riemann zetac function")
	   ]
end;

fun mapStream stream f lst = 
    let
	fun inner x = TextIO.output (stream, f x)
    in
	List.app inner lst
    end;
fun writer fileName lst =
    let
	open TextIO
	val s = openOut fileName
	val _ = List.app (fn x => output (s, x)) lst
	val _ = closeOut s
    in
	()
    end;

val constants = [("MACHEP","2**-53"),
	     ("MAXLOG","log(2**1024)"),
	     ("MINLOG","log(2**-1022)"),
	     ("MAXNUM","2**1024"),
	     ("PI","pi"),
	     ("PIO2","pi/2"),
	     ("PIO4","pi/4"),
	     ("SQRT2","sqrt(2)"),
	     ("SQRTH","sqrt(2)/2"),
	     ("LOG2E","1/log(2)"),
	     ("SQ2OPI","sqrt(2/pi)"),
	     ("LOGE2","log(2)"),
	     ("LOGSQ2","log(2)/2"),
	     ("THPIO4","2*pi/4"),
	     ("TWOOPI","2/pi")];

fun main () = 

    let
	val _ = writer "cephes-sig.sml" (List.concat [["signature CEPHES = sig\n",
						      "type cmplx = real * real;\n"],
						      List.map (fn (name,description) => "(* " ^ description ^ " *)\nval " ^ name ^ " : real;\n") constants,
						      List.map writeSignature defs, ["\nend;\n"]])
	val _ = writer "cephes-smlsharp.smi" (List.concat [["_require \"basis.smi\"\n",
							    "structure Cephes = struct\n",
							    "type cmplx = real * real;\n"],
							   List.map (fn (name,description) => "(* " ^ description ^ " *)\nval " ^ name ^ " : unit -> real;\n") constants,
							   List.map writeSmlsharpSmiStructure defs,
							   ["\nend;\n"]])
	val _ = writer "cephes-mlton.sml" (List.concat [["structure Cephes :> CEPHES = struct\n",
							"type cmplx = real * real;\n"],
					List.map (fn (name,description) => "(* " ^ description ^ " *)\nlocal\n  val addr = _address \"" ^ name ^ "\" : MLton.Pointer.t;\nin\n  val " ^ name ^ " = MLton.Pointer.getReal64(addr, 0)\nend;\n") constants,
					List.map writeMltonStructure defs, ["\nend;\n"]])
	val _ = writer "cephes-polyml.sml" (List.concat [["structure Cephes :> CEPHES = struct\n",
							  "type cmplx = real * real;\n",
							  "open Foreign;\n",
							  "val cComplex = cStruct2(cDouble, cDouble);\n",
							 "val lib = loadLibrary \"./libcephes.so\";\n"],
							 List.map (fn (name,description) => "(* " ^ description ^ " *)\nval " ^ name ^ " = Memory.getDouble(symbolAsAddress(getSymbol lib \"" ^ name ^ "\"), Word.fromInt 0);\n") constants,
							 List.map writePolyStructure defs, ["\nend;\n"]])
	val _ = writer "cephes-smlsharp.sml" (List.concat [["structure Cephes = struct\n",
							  "type cmplx = real * real;\n"],
							 List.map (fn (name,description) => "(* " ^ description ^ " *)\nval " ^ name ^ " = _import \"" ^ name ^ "\" : () -> real;\n") constants,
							 List.map writeSmlsharpStructure defs,
							 ["\nend;\n"]])
    in
	()
    end;

end;


val _ = Cephes_generator.main();


use "cephes-sig.sml";
use "cephes-polyml.sml";

let
    open Cephes;
    val c1 = (1.0, 2.0)
    val c2 = (3.0, 4.0)
in
    {
      PI=PI,
      acos = acos 1.0,
      acosh = acosh 1.0,
      airy = airy 0.5,
      asin = asin 0.0,
      asinh = asinh 0.0,
      atan = atan 0.0,
      atan2 = atan2 (1.0, 2.0),
      bdtr = bdtr (10, 20, 0.5),
      bdtrc = bdtrc (10, 20, 0.5),
      bdtri = bdtri (10, 20, 0.5),
      ndtri = ndtri 0.975,
      ndtr = ndtr 1.96,
      cadd = cadd(c1,c2)
    }
end;

(* TODO: 
 - Tests
 - Outputs for MonetDB
 - Output for ...
 - Output to IDL (extension)
 - Use idl parser from mlworks (extension)
*)
