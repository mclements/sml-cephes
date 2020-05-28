structure Cephes :> CEPHES = struct
type cmplx = real * real;
open Foreign;
val cComplex = cStruct2(cDouble, cDouble);
val lib = loadLibrary "./libcephes.so";
(* 2**-53 *)
val MACHEP = Memory.getDouble(symbolAsAddress(getSymbol lib "MACHEP"), Word.fromInt 0);
(* log(2**1024) *)
val MAXLOG = Memory.getDouble(symbolAsAddress(getSymbol lib "MAXLOG"), Word.fromInt 0);
(* log(2**-1022) *)
val MINLOG = Memory.getDouble(symbolAsAddress(getSymbol lib "MINLOG"), Word.fromInt 0);
(* 2**1024 *)
val MAXNUM = Memory.getDouble(symbolAsAddress(getSymbol lib "MAXNUM"), Word.fromInt 0);
(* pi *)
val PI = Memory.getDouble(symbolAsAddress(getSymbol lib "PI"), Word.fromInt 0);
(* pi/2 *)
val PIO2 = Memory.getDouble(symbolAsAddress(getSymbol lib "PIO2"), Word.fromInt 0);
(* pi/4 *)
val PIO4 = Memory.getDouble(symbolAsAddress(getSymbol lib "PIO4"), Word.fromInt 0);
(* sqrt(2) *)
val SQRT2 = Memory.getDouble(symbolAsAddress(getSymbol lib "SQRT2"), Word.fromInt 0);
(* sqrt(2)/2 *)
val SQRTH = Memory.getDouble(symbolAsAddress(getSymbol lib "SQRTH"), Word.fromInt 0);
(* 1/log(2) *)
val LOG2E = Memory.getDouble(symbolAsAddress(getSymbol lib "LOG2E"), Word.fromInt 0);
(* sqrt(2/pi) *)
val SQ2OPI = Memory.getDouble(symbolAsAddress(getSymbol lib "SQ2OPI"), Word.fromInt 0);
(* log(2) *)
val LOGE2 = Memory.getDouble(symbolAsAddress(getSymbol lib "LOGE2"), Word.fromInt 0);
(* log(2)/2 *)
val LOGSQ2 = Memory.getDouble(symbolAsAddress(getSymbol lib "LOGSQ2"), Word.fromInt 0);
(* 2*pi/4 *)
val THPIO4 = Memory.getDouble(symbolAsAddress(getSymbol lib "THPIO4"), Word.fromInt 0);
(* 2/pi *)
val TWOOPI = Memory.getDouble(symbolAsAddress(getSymbol lib "TWOOPI"), Word.fromInt 0);
(* Inverse hyperbolic cosine. acosh(x : real) : real *)
local
  val call = buildCall1(getSymbol lib "md_acosh", (cDouble), cDouble)
in
  fun acosh(x : real) = call(x)
end;
(* Airy function. airy(x : real) : (int, real, real, real, real) *)
local
  val call = buildCall5(getSymbol lib "airy", (cDouble, cStar cDouble, cStar cDouble, cStar cDouble, cStar cDouble), cInt)
in
  fun airy(x : real) =
    let
      val ai = ref 0.0
      val aip = ref 0.0
      val bi = ref 0.0
      val bip = ref 0.0
      val zzz123 = call(x, ai, aip, bi, bip)
    in
      (zzz123, ! ai, ! aip, ! bi, ! bip)
    end
end;
(* Inverse circular sine. asin(x : real) : real *)
local
  val call = buildCall1(getSymbol lib "md_asin", (cDouble), cDouble)
in
  fun asin(x : real) = call(x)
end;
(* Inverse circular cosine. acos(x : real) : real *)
local
  val call = buildCall1(getSymbol lib "md_acos", (cDouble), cDouble)
in
  fun acos(x : real) = call(x)
end;
(* Inverse hyperbolic sine. asinh(x : real) : real *)
local
  val call = buildCall1(getSymbol lib "md_asinh", (cDouble), cDouble)
in
  fun asinh(x : real) = call(x)
end;
(* Inverse circular tangent (arctangent). atan(x : real) : real *)
local
  val call = buildCall1(getSymbol lib "md_atan", (cDouble), cDouble)
in
  fun atan(x : real) = call(x)
end;
(* Quadrant correct inverse circular tangent. atan2(y : real, x : real) : real *)
local
  val call = buildCall2(getSymbol lib "md_atan2", (cDouble, cDouble), cDouble)
in
  fun atan2(y : real, x : real) = call(y, x)
end;
(* Inverse hyperbolic tangent. atanh(x : real) : real *)
local
  val call = buildCall1(getSymbol lib "md_atanh", (cDouble), cDouble)
in
  fun atanh(x : real) = call(x)
end;
(* Binomial distribution. bdtr(k : int, n : int, p : real) : real *)
local
  val call = buildCall3(getSymbol lib "bdtr", (cInt, cInt, cDouble), cDouble)
in
  fun bdtr(k : int, n : int, p : real) = call(k, n, p)
end;
(* Complemented binomial distribution. bdtrc(k : int, n : int, p : real) : real *)
local
  val call = buildCall3(getSymbol lib "bdtrc", (cInt, cInt, cDouble), cDouble)
in
  fun bdtrc(k : int, n : int, p : real) = call(k, n, p)
end;
(* Inverse binomial distribution. bdtri(k : int, n : int, y : real) : real *)
local
  val call = buildCall3(getSymbol lib "bdtri", (cInt, cInt, cDouble), cDouble)
in
  fun bdtri(k : int, n : int, y : real) = call(k, n, y)
end;
(* Beta function. beta(a : real, b : real) : real *)
local
  val call = buildCall2(getSymbol lib "beta", (cDouble, cDouble), cDouble)
in
  fun beta(a : real, b : real) = call(a, b)
end;
(* Beta distribution. btdtr(a : real, b : real, x : real) : real *)
local
  val call = buildCall3(getSymbol lib "btdtr", (cDouble, cDouble, cDouble), cDouble)
in
  fun btdtr(a : real, b : real, x : real) = call(a, b, x)
end;
(* Cube root. cbrt(x : real) : real *)
local
  val call = buildCall1(getSymbol lib "md_cbrt", (cDouble), cDouble)
in
  fun cbrt(x : real) = call(x)
end;
(* Evaluate Chebyshev series. chbevl(x : real, coef : real Vector.vector) : real *)
local
  val call = buildCall3(getSymbol lib "chbevl", (cDouble, cVectorPointer cDouble, cInt), cDouble)
in
  fun chbevl(x : real, coef : real Vector.vector) = call(x, coef, Vector.length coef)
end;
(* Chi-square distribution. chdtr(df : real, x : real) : real *)
local
  val call = buildCall2(getSymbol lib "chdtr", (cDouble, cDouble), cDouble)
in
  fun chdtr(df : real, x : real) = call(df, x)
end;
(* Complemented Chi-square distribution. chdtrc(df : real, x : real) : real *)
local
  val call = buildCall2(getSymbol lib "chdtrc", (cDouble, cDouble), cDouble)
in
  fun chdtrc(df : real, x : real) = call(df, x)
end;
(* Inverse of complemented Chi-square distribution. chdtri(df : real, x : real) : real *)
local
  val call = buildCall2(getSymbol lib "chdtri", (cDouble, cDouble), cDouble)
in
  fun chdtri(df : real, x : real) = call(df, x)
end;
(* Complex natural logarithm. clog(x : cmplx) : cmplx *)
local
  val call = buildCall2(getSymbol lib "md_clog", (cStar cComplex, cStar cComplex), cVoid)
in
  fun clog(x : cmplx) =
    let
      val y = ref (0.0,0.0)
      val _ = call(ref x, y)
    in
      ! y
    end
end;
(* Complex exponential function. cexp(x : cmplx) : cmplx *)
local
  val call = buildCall2(getSymbol lib "md_cexp", (cStar cComplex, cStar cComplex), cVoid)
in
  fun cexp(x : cmplx) =
    let
      val y = ref (0.0,0.0)
      val _ = call(ref x, y)
    in
      ! y
    end
end;
(* Complex circular sine. csin(x : cmplx) : cmplx *)
local
  val call = buildCall2(getSymbol lib "md_csin", (cStar cComplex, cStar cComplex), cVoid)
in
  fun csin(x : cmplx) =
    let
      val y = ref (0.0,0.0)
      val _ = call(ref x, y)
    in
      ! y
    end
end;
(* Complex circular cosine. ccos(x : cmplx) : cmplx *)
local
  val call = buildCall2(getSymbol lib "md_ccos", (cStar cComplex, cStar cComplex), cVoid)
in
  fun ccos(x : cmplx) =
    let
      val y = ref (0.0,0.0)
      val _ = call(ref x, y)
    in
      ! y
    end
end;
(* Complex circular tangent. ctan(x : cmplx) : cmplx *)
local
  val call = buildCall2(getSymbol lib "md_ctan", (cStar cComplex, cStar cComplex), cVoid)
in
  fun ctan(x : cmplx) =
    let
      val y = ref (0.0,0.0)
      val _ = call(ref x, y)
    in
      ! y
    end
end;
(* Complex circular cotangent. ccot(x : cmplx) : cmplx *)
local
  val call = buildCall2(getSymbol lib "ccot", (cStar cComplex, cStar cComplex), cVoid)
in
  fun ccot(x : cmplx) =
    let
      val y = ref (0.0,0.0)
      val _ = call(ref x, y)
    in
      ! y
    end
end;
(* Complex circular arc sine. casin(x : cmplx) : cmplx *)
local
  val call = buildCall2(getSymbol lib "md_casin", (cStar cComplex, cStar cComplex), cVoid)
in
  fun casin(x : cmplx) =
    let
      val y = ref (0.0,0.0)
      val _ = call(ref x, y)
    in
      ! y
    end
end;
(* Complex circular arc cosine. cacos(x : cmplx) : cmplx *)
local
  val call = buildCall2(getSymbol lib "md_cacos", (cStar cComplex, cStar cComplex), cVoid)
in
  fun cacos(x : cmplx) =
    let
      val y = ref (0.0,0.0)
      val _ = call(ref x, y)
    in
      ! y
    end
end;
(* Complex circular arc tangent. catan(x : cmplx) : cmplx *)
local
  val call = buildCall2(getSymbol lib "md_catan", (cStar cComplex, cStar cComplex), cVoid)
in
  fun catan(x : cmplx) =
    let
      val y = ref (0.0,0.0)
      val _ = call(ref x, y)
    in
      ! y
    end
end;
(* Complex hyperbolic sine. csinh(x : cmplx) : cmplx *)
local
  val call = buildCall2(getSymbol lib "md_csinh", (cStar cComplex, cStar cComplex), cVoid)
in
  fun csinh(x : cmplx) =
    let
      val y = ref (0.0,0.0)
      val _ = call(ref x, y)
    in
      ! y
    end
end;
(* Complex hyperbolic cosine. ccosh(x : cmplx) : cmplx *)
local
  val call = buildCall2(getSymbol lib "md_ccosh", (cStar cComplex, cStar cComplex), cVoid)
in
  fun ccosh(x : cmplx) =
    let
      val y = ref (0.0,0.0)
      val _ = call(ref x, y)
    in
      ! y
    end
end;
(* Complex inverse hyperbolic cosine. cacosh(x : cmplx) : cmplx *)
local
  val call = buildCall2(getSymbol lib "md_cacosh", (cStar cComplex, cStar cComplex), cVoid)
in
  fun cacosh(x : cmplx) =
    let
      val y = ref (0.0,0.0)
      val _ = call(ref x, y)
    in
      ! y
    end
end;
(* Complex hyperbolic tangent. ctanh(x : cmplx) : cmplx *)
local
  val call = buildCall2(getSymbol lib "md_ctanh", (cStar cComplex, cStar cComplex), cVoid)
in
  fun ctanh(x : cmplx) =
    let
      val y = ref (0.0,0.0)
      val _ = call(ref x, y)
    in
      ! y
    end
end;
(* Complex inverse hyperbolic tangent. catanh(x : cmplx) : cmplx *)
local
  val call = buildCall2(getSymbol lib "md_catanh", (cStar cComplex, cStar cComplex), cVoid)
in
  fun catanh(x : cmplx) =
    let
      val y = ref (0.0,0.0)
      val _ = call(ref x, y)
    in
      ! y
    end
end;
(* Complex power function. cpow(x : cmplx, y : cmplx) : cmplx *)
local
  val call = buildCall3(getSymbol lib "md_cpow", (cStar cComplex, cStar cComplex, cStar cComplex), cVoid)
in
  fun cpow(x : cmplx, y : cmplx) =
    let
      val z = ref (0.0,0.0)
      val _ = call(ref x, ref y, z)
    in
      ! z
    end
end;
(* Complex addition. cadd(x : cmplx, y : cmplx) : cmplx *)
local
  val call = buildCall3(getSymbol lib "cadd", (cStar cComplex, cStar cComplex, cStar cComplex), cVoid)
in
  fun cadd(x : cmplx, y : cmplx) =
    let
      val z = ref (0.0,0.0)
      val _ = call(ref x, ref y, z)
    in
      ! z
    end
end;
(* Complex subtraction. csub(x : cmplx, y : cmplx) : cmplx *)
local
  val call = buildCall3(getSymbol lib "csub", (cStar cComplex, cStar cComplex, cStar cComplex), cVoid)
in
  fun csub(x : cmplx, y : cmplx) =
    let
      val z = ref (0.0,0.0)
      val _ = call(ref x, ref y, z)
    in
      ! z
    end
end;
(* Complex multiplication. cmul(x : cmplx, y : cmplx) : cmplx *)
local
  val call = buildCall3(getSymbol lib "cmul", (cStar cComplex, cStar cComplex, cStar cComplex), cVoid)
in
  fun cmul(x : cmplx, y : cmplx) =
    let
      val z = ref (0.0,0.0)
      val _ = call(ref x, ref y, z)
    in
      ! z
    end
end;
(* Complex division. cdiv(x : cmplx, y : cmplx) : cmplx *)
local
  val call = buildCall3(getSymbol lib "cdiv", (cStar cComplex, cStar cComplex, cStar cComplex), cVoid)
in
  fun cdiv(x : cmplx, y : cmplx) =
    let
      val z = ref (0.0,0.0)
      val _ = call(ref x, ref y, z)
    in
      ! z
    end
end;
(* Complex absolute value. cabs(x : cmplx) : cmplx *)
local
  val call = buildCall2(getSymbol lib "md_cabs", (cStar cComplex, cStar cComplex), cVoid)
in
  fun cabs(x : cmplx) =
    let
      val y = ref (0.0,0.0)
      val _ = call(ref x, y)
    in
      ! y
    end
end;
(* Complex square root. csqrt(x : cmplx) : cmplx *)
local
  val call = buildCall2(getSymbol lib "md_csqrt", (cStar cComplex, cStar cComplex), cVoid)
in
  fun csqrt(x : cmplx) =
    let
      val y = ref (0.0,0.0)
      val _ = call(ref x, y)
    in
      ! y
    end
end;
(* Hyperbolic cosine. cosh(x : real) : real *)
local
  val call = buildCall1(getSymbol lib "md_cosh", (cDouble), cDouble)
in
  fun cosh(x : real) = call(x)
end;
(* Dawson's integral. dawsn(x : real) : real *)
local
  val call = buildCall1(getSymbol lib "dawsn", (cDouble), cDouble)
in
  fun dawsn(x : real) = call(x)
end;
(* Pseudorandom number generator. drand() : real *)
local
  val call = buildCall1(getSymbol lib "drand", (cStar cDouble), cVoid)
in
  fun drand() =
    let
      val y = ref 0.0
      val _ = call(y)
    in
      ! y
    end
end;
(* Exponential integral. ei(x : real) : real *)
local
  val call = buildCall1(getSymbol lib "ei", (cDouble), cDouble)
in
  fun ei(x : real) = call(x)
end;
(* Incomplete elliptic integral of the second kind. ellie(phi : real, m : real) : real *)
local
  val call = buildCall2(getSymbol lib "ellie", (cDouble, cDouble), cDouble)
in
  fun ellie(phi : real, m : real) = call(phi, m)
end;
(* Incomplete elliptic integral of the first kind. ellik(phi : real, m : real) : real *)
local
  val call = buildCall2(getSymbol lib "ellik", (cDouble, cDouble), cDouble)
in
  fun ellik(phi : real, m : real) = call(phi, m)
end;
(* Complete elliptic integral of the second kind. ellpe(m1 : real) : real *)
local
  val call = buildCall1(getSymbol lib "ellpe", (cDouble), cDouble)
in
  fun ellpe(m1 : real) = call(m1)
end;
(* Jacobian Elliptic Functions. ellpj(u : real, m : real) : (int, real, real, real, real) *)
local
  val call = buildCall6(getSymbol lib "ellpj", (cDouble, cDouble, cStar cDouble, cStar cDouble, cStar cDouble, cStar cDouble), cInt)
in
  fun ellpj(u : real, m : real) =
    let
      val sn = ref 0.0
      val cn = ref 0.0
      val dn = ref 0.0
      val phi = ref 0.0
      val zzz123 = call(u, m, sn, cn, dn, phi)
    in
      (zzz123, ! sn, ! cn, ! dn, ! phi)
    end
end;
(* Complete elliptic integral of the first kind. ellpk(m1 : real) : real *)
local
  val call = buildCall1(getSymbol lib "ellpk", (cDouble), cDouble)
in
  fun ellpk(m1 : real) = call(m1)
end;
(* Exponential function. exp(x : real) : real *)
local
  val call = buildCall1(getSymbol lib "md_exp", (cDouble), cDouble)
in
  fun exp(x : real) = call(x)
end;
(* Base 10 exponential function. exp10(x : real) : real *)
local
  val call = buildCall1(getSymbol lib "md_exp10", (cDouble), cDouble)
in
  fun exp10(x : real) = call(x)
end;
(* Base 2 exponential function. exp2(x : real) : real *)
local
  val call = buildCall1(getSymbol lib "md_exp2", (cDouble), cDouble)
in
  fun exp2(x : real) = call(x)
end;
(* Exponential integral En. expn(n : int, x : real) : real *)
local
  val call = buildCall2(getSymbol lib "md_expn", (cInt, cDouble), cDouble)
in
  fun expn(n : int, x : real) = call(n, x)
end;
(* Exponential of squared argument. expx2(x : real, sign : int) : real *)
local
  val call = buildCall2(getSymbol lib "expx2", (cDouble, cInt), cDouble)
in
  fun expx2(x : real, sign : int) = call(x, sign)
end;
(* Absolute value. fabs(x : real) : real *)
local
  val call = buildCall1(getSymbol lib "md_fabs", (cDouble), cDouble)
in
  fun fabs(x : real) = call(x)
end;
(* Factorial function. fac(i : int) : real *)
local
  val call = buildCall1(getSymbol lib "fac", (cInt), cDouble)
in
  fun fac(i : int) = call(i)
end;
(* F distribution. fdtr(df1 : int, df2 : int, x : real) : real *)
local
  val call = buildCall3(getSymbol lib "fdtr", (cInt, cInt, cDouble), cDouble)
in
  fun fdtr(df1 : int, df2 : int, x : real) = call(df1, df2, x)
end;
(* Complemented F distribution. fdtrc(df1 : int, df2 : int, x : real) : real *)
local
  val call = buildCall3(getSymbol lib "fdtrc", (cInt, cInt, cDouble), cDouble)
in
  fun fdtrc(df1 : int, df2 : int, x : real) = call(df1, df2, x)
end;
(* Inverse of complemented F distribution. fdtri(df1 : int, df2 : int, p : real) : real *)
local
  val call = buildCall3(getSymbol lib "fdtri", (cInt, cInt, cDouble), cDouble)
in
  fun fdtri(df1 : int, df2 : int, p : real) = call(df1, df2, p)
end;
(* Floor function. floor(x : real) : real *)
local
  val call = buildCall1(getSymbol lib "md_floor", (cDouble), cDouble)
in
  fun floor(x : real) = call(x)
end;
(* Ceiling function. ceil(x : real) : real *)
local
  val call = buildCall1(getSymbol lib "md_ceil", (cDouble), cDouble)
in
  fun ceil(x : real) = call(x)
end;
(* frexp. frexp(x : real, expnt : int) : (real, int) *)
local
  val call = buildCall2(getSymbol lib "frexp", (cDouble, cStar cInt), cDouble)
in
  fun frexp(x : real, expnt : int) =
    let
      val ref_expnt = ref expnt
      val zzz123 = call(x, ref_expnt)
    in
      (zzz123, ! ref_expnt)
    end
end;
(* ldexp. ldexp(x : real, n : int) : real *)
local
  val call = buildCall2(getSymbol lib "ldexp", (cDouble, cInt), cDouble)
in
  fun ldexp(x : real, n : int) = call(x, n)
end;
(* Fresnel integral. fresnl(x : real) : (real, real) *)
local
  val call = buildCall3(getSymbol lib "fresnl", (cDouble, cStar cDouble, cStar cDouble), cVoid)
in
  fun fresnl(x : real) =
    let
      val S = ref 0.0
      val C = ref 0.0
      val _ = call(x, S, C)
    in
      (! S, ! C)
    end
end;
(* Gamma function. gamma(x : real) : real *)
local
  val call = buildCall1(getSymbol lib "md_gamma", (cDouble), cDouble)
in
  fun gamma(x : real) = call(x)
end;
(* Natural logarithm of gamma function. lgam(x : real) : real *)
local
  val call = buildCall1(getSymbol lib "lgam", (cDouble), cDouble)
in
  fun lgam(x : real) = call(x)
end;
(* Gamma distribution function. gdtr(a : real, b : real, x : real) : real *)
local
  val call = buildCall3(getSymbol lib "gdtr", (cDouble, cDouble, cDouble), cDouble)
in
  fun gdtr(a : real, b : real, x : real) = call(a, b, x)
end;
(* Complemented gamma distribution function. gdtrc(a : real, b : real, x : real) : real *)
local
  val call = buildCall3(getSymbol lib "gdtrc", (cDouble, cDouble, cDouble), cDouble)
in
  fun gdtrc(a : real, b : real, x : real) = call(a, b, x)
end;
(* Gauss hypergeometric function _2F_1. hyp2f1(a : real, b : real, c : real, x : real) : real *)
local
  val call = buildCall4(getSymbol lib "hyp2f1", (cDouble, cDouble, cDouble, cDouble), cDouble)
in
  fun hyp2f1(a : real, b : real, c : real, x : real) = call(a, b, c, x)
end;
(* Confluent hypergeometric function. hyperg(a : real, b : real, x : real) : real *)
local
  val call = buildCall3(getSymbol lib "hyperg", (cDouble, cDouble, cDouble), cDouble)
in
  fun hyperg(a : real, b : real, x : real) = call(a, b, x)
end;
(* Modified Bessel function of order zero. i0(x : real) : real *)
local
  val call = buildCall1(getSymbol lib "i0", (cDouble), cDouble)
in
  fun i0(x : real) = call(x)
end;
(* Modified Bessel function of order zero, exponentially scaled. i0e(x : real) : real *)
local
  val call = buildCall1(getSymbol lib "i0e", (cDouble), cDouble)
in
  fun i0e(x : real) = call(x)
end;
(* Modified Bessel function of order one. i1(x : real) : real *)
local
  val call = buildCall1(getSymbol lib "i1", (cDouble), cDouble)
in
  fun i1(x : real) = call(x)
end;
(* Modified Bessel function of order one, exponentially scaled. i1e(x : real) : real *)
local
  val call = buildCall1(getSymbol lib "i1e", (cDouble), cDouble)
in
  fun i1e(x : real) = call(x)
end;
(* Incomplete gamma integral. igam(a : real, x : real) : real *)
local
  val call = buildCall2(getSymbol lib "igam", (cDouble, cDouble), cDouble)
in
  fun igam(a : real, x : real) = call(a, x)
end;
(* Complemented incomplete gamma integral. igamc(a : real, x : real) : real *)
local
  val call = buildCall2(getSymbol lib "igamc", (cDouble, cDouble), cDouble)
in
  fun igamc(a : real, x : real) = call(a, x)
end;
(* Inverse of complemented incomplete gamma integral. igami(a : real, p : real) : real *)
local
  val call = buildCall2(getSymbol lib "igami", (cDouble, cDouble), cDouble)
in
  fun igami(a : real, p : real) = call(a, p)
end;
(* Incomplete beta integral. incbet(a : real, b : real, x : real) : real *)
local
  val call = buildCall3(getSymbol lib "incbet", (cDouble, cDouble, cDouble), cDouble)
in
  fun incbet(a : real, b : real, x : real) = call(a, b, x)
end;
(* Inverse of incomplete beta integral. incbi(a : real, b : real, y : real) : real *)
local
  val call = buildCall3(getSymbol lib "incbi", (cDouble, cDouble, cDouble), cDouble)
in
  fun incbi(a : real, b : real, y : real) = call(a, b, y)
end;
(* signbit. signbit(x : real) : int *)
local
  val call = buildCall1(getSymbol lib "signbit", (cDouble), cInt)
in
  fun signbit(x : real) = call(x)
end;
(* isnan. isnan(x : real) : int *)
local
  val call = buildCall1(getSymbol lib "isnan", (cDouble), cInt)
in
  fun isnan(x : real) = call(x)
end;
(* isfinite. isfinite(x : real) : int *)
local
  val call = buildCall1(getSymbol lib "isfinite", (cDouble), cInt)
in
  fun isfinite(x : real) = call(x)
end;
(* Modified Bessel function of noninteger order. iv(v : real, x : real) : real *)
local
  val call = buildCall2(getSymbol lib "iv", (cDouble, cDouble), cDouble)
in
  fun iv(v : real, x : real) = call(v, x)
end;
(* Bessel function of order zero. j0(x : real) : real *)
local
  val call = buildCall1(getSymbol lib "j0", (cDouble), cDouble)
in
  fun j0(x : real) = call(x)
end;
(* Bessel function of second kind, order zero. y0(x : real) : real *)
local
  val call = buildCall1(getSymbol lib "y0", (cDouble), cDouble)
in
  fun y0(x : real) = call(x)
end;
(* Bessel function of order one. j1(x : real) : real *)
local
  val call = buildCall1(getSymbol lib "j1", (cDouble), cDouble)
in
  fun j1(x : real) = call(x)
end;
(* Bessel function of second kind of order one. y1(x : real) : real *)
local
  val call = buildCall1(getSymbol lib "y1", (cDouble), cDouble)
in
  fun y1(x : real) = call(x)
end;
(* Bessel function of integer order. jn(n : int, x : real) : real *)
local
  val call = buildCall2(getSymbol lib "jn", (cInt, cDouble), cDouble)
in
  fun jn(n : int, x : real) = call(n, x)
end;
(* Bessel function of noninteger order. jv(v : real, x : real) : real *)
local
  val call = buildCall2(getSymbol lib "jv", (cDouble, cDouble), cDouble)
in
  fun jv(v : real, x : real) = call(v, x)
end;
(* Modified Bessel function, third kind, order zero. k0(x : real) : real *)
local
  val call = buildCall1(getSymbol lib "k0", (cDouble), cDouble)
in
  fun k0(x : real) = call(x)
end;
(* Modified Bessel function, third kind, order zero, exponentially scaled. k0e(x : real) : real *)
local
  val call = buildCall1(getSymbol lib "k0e", (cDouble), cDouble)
in
  fun k0e(x : real) = call(x)
end;
(* Modified Bessel function, third kind, order one. k1(x : real) : real *)
local
  val call = buildCall1(getSymbol lib "k1", (cDouble), cDouble)
in
  fun k1(x : real) = call(x)
end;
(* Modified Bessel function, third kind, order one, exponentially scaled. k1e(x : real) : real *)
local
  val call = buildCall1(getSymbol lib "k1e", (cDouble), cDouble)
in
  fun k1e(x : real) = call(x)
end;
(* Modified Bessel function, third kind, integer order. kn(n : int, x : real) : real *)
local
  val call = buildCall2(getSymbol lib "kn", (cInt, cDouble), cDouble)
in
  fun kn(n : int, x : real) = call(n, x)
end;
(* Natural logarithm. log(x : real) : real *)
local
  val call = buildCall1(getSymbol lib "md_log", (cDouble), cDouble)
in
  fun log(x : real) = call(x)
end;
(* Common logarithm. log10(x : real) : real *)
local
  val call = buildCall1(getSymbol lib "md_log10", (cDouble), cDouble)
in
  fun log10(x : real) = call(x)
end;
(* Base 2 logarithm. log2(x : real) : real *)
local
  val call = buildCall1(getSymbol lib "md_log2", (cDouble), cDouble)
in
  fun log2(x : real) = call(x)
end;
(* Negative binomial distribution. nbdtr(k : int, n : int, p : real) : real *)
local
  val call = buildCall3(getSymbol lib "nbdtr", (cInt, cInt, cDouble), cDouble)
in
  fun nbdtr(k : int, n : int, p : real) = call(k, n, p)
end;
(* Complemented negative binomial distribution. nbdtrc(k : int, n : int, p : real) : real *)
local
  val call = buildCall3(getSymbol lib "nbdtrc", (cInt, cInt, cDouble), cDouble)
in
  fun nbdtrc(k : int, n : int, p : real) = call(k, n, p)
end;
(* Functional inverse of negative binomial distribution. nbdtri(k : int, n : int, y : real) : real *)
local
  val call = buildCall3(getSymbol lib "nbdtri", (cInt, cInt, cDouble), cDouble)
in
  fun nbdtri(k : int, n : int, y : real) = call(k, n, y)
end;
(* Normal distribution function. ndtr(x : real) : real *)
local
  val call = buildCall1(getSymbol lib "ndtr", (cDouble), cDouble)
in
  fun ndtr(x : real) = call(x)
end;
(* Error function. erf(x : real) : real *)
local
  val call = buildCall1(getSymbol lib "md_erf", (cDouble), cDouble)
in
  fun erf(x : real) = call(x)
end;
(* Complementary error function. erfc(x : real) : real *)
local
  val call = buildCall1(getSymbol lib "md_erfc", (cDouble), cDouble)
in
  fun erfc(x : real) = call(x)
end;
(* Inverse of Normal distribution function. ndtri(x : real) : real *)
local
  val call = buildCall1(getSymbol lib "ndtri", (cDouble), cDouble)
in
  fun ndtri(x : real) = call(x)
end;
(* Poisson distribution function. pdtr(k : int, m : real) : real *)
local
  val call = buildCall2(getSymbol lib "pdtr", (cInt, cDouble), cDouble)
in
  fun pdtr(k : int, m : real) = call(k, m)
end;
(* Complemented Poisson distribution. pdtrc(k : int, m : real) : real *)
local
  val call = buildCall2(getSymbol lib "pdtrc", (cInt, cDouble), cDouble)
in
  fun pdtrc(k : int, m : real) = call(k, m)
end;
(* Inverse Poisson distribution. pdtri(k : int, y : real) : real *)
local
  val call = buildCall2(getSymbol lib "pdtri", (cInt, cDouble), cDouble)
in
  fun pdtri(k : int, y : real) = call(k, y)
end;
(* Evaluate polynomial. polevl(x : real, coef : real Vector.vector) : real *)
local
  val call = buildCall3(getSymbol lib "polevl", (cDouble, cVectorPointer cDouble, cInt), cDouble)
in
  fun polevl(x : real, coef : real Vector.vector) = call(x, coef, Vector.length coef)
end;
(* Power function. pow(x : real, y : real) : real *)
local
  val call = buildCall2(getSymbol lib "md_pow", (cDouble, cDouble), cDouble)
in
  fun pow(x : real, y : real) = call(x, y)
end;
(* Real raised to integer power. powi(x : real, n : int) : real *)
local
  val call = buildCall2(getSymbol lib "md_powi", (cDouble, cInt), cDouble)
in
  fun powi(x : real, n : int) = call(x, n)
end;
(* Psi (digamma) function. psi(x : real) : real *)
local
  val call = buildCall1(getSymbol lib "psi", (cDouble), cDouble)
in
  fun psi(x : real) = call(x)
end;
(* Reciprocal gamma function. rgamma(x : real) : real *)
local
  val call = buildCall1(getSymbol lib "rgamma", (cDouble), cDouble)
in
  fun rgamma(x : real) = call(x)
end;
(* Round double to nearest or even integer valued double. round(x : real) : real *)
local
  val call = buildCall1(getSymbol lib "md_round", (cDouble), cDouble)
in
  fun round(x : real) = call(x)
end;
(* Hyperbolic sine and cosine integrals. shichi(x : real) : (real, real, real) *)
local
  val call = buildCall3(getSymbol lib "shichi", (cDouble, cStar cDouble, cStar cDouble), cDouble)
in
  fun shichi(x : real) =
    let
      val Chi = ref 0.0
      val Shi = ref 0.0
      val zzz123 = call(x, Chi, Shi)
    in
      (zzz123, ! Chi, ! Shi)
    end
end;
(* Sine and cosine integrals. sici(x : real) : (real, real, real) *)
local
  val call = buildCall3(getSymbol lib "sici", (cDouble, cStar cDouble, cStar cDouble), cDouble)
in
  fun sici(x : real) =
    let
      val Si = ref 0.0
      val Ci = ref 0.0
      val zzz123 = call(x, Si, Ci)
    in
      (zzz123, ! Si, ! Ci)
    end
end;
(* Circular sine. sin(x : real) : real *)
local
  val call = buildCall1(getSymbol lib "md_sin", (cDouble), cDouble)
in
  fun sin(x : real) = call(x)
end;
(* Circular cosine. cos(x : real) : real *)
local
  val call = buildCall1(getSymbol lib "md_cos", (cDouble), cDouble)
in
  fun cos(x : real) = call(x)
end;
(* Circular sine of angle in degrees. sindg(x : real) : real *)
local
  val call = buildCall1(getSymbol lib "md_sindg", (cDouble), cDouble)
in
  fun sindg(x : real) = call(x)
end;
(* Circular cosine of angle in degrees. cosdg(x : real) : real *)
local
  val call = buildCall1(getSymbol lib "cosdg", (cDouble), cDouble)
in
  fun cosdg(x : real) = call(x)
end;
(* Hyperbolic sine. sinh(x : real) : real *)
local
  val call = buildCall1(getSymbol lib "md_sinh", (cDouble), cDouble)
in
  fun sinh(x : real) = call(x)
end;
(* Dilogarithm. spence(x : real) : real *)
local
  val call = buildCall1(getSymbol lib "spence", (cDouble), cDouble)
in
  fun spence(x : real) = call(x)
end;
(* Square root. sqrt(x : real) : real *)
local
  val call = buildCall1(getSymbol lib "sqrt", (cDouble), cDouble)
in
  fun sqrt(x : real) = call(x)
end;
(* Student's t distribution. stdtr(k : int, t : real) : real *)
local
  val call = buildCall2(getSymbol lib "stdtr", (cInt, cDouble), cDouble)
in
  fun stdtr(k : int, t : real) = call(k, t)
end;
(* Functional inverse of Student's t distribution. stdtri(k : int, p : real) : real *)
local
  val call = buildCall2(getSymbol lib "stdtri", (cInt, cDouble), cDouble)
in
  fun stdtri(k : int, p : real) = call(k, p)
end;
(* Struve function. struve(v : int, x : real) : real *)
local
  val call = buildCall2(getSymbol lib "struve", (cInt, cDouble), cDouble)
in
  fun struve(v : int, x : real) = call(v, x)
end;
(* Circular tangent. tan(x : real) : real *)
local
  val call = buildCall1(getSymbol lib "md_tan", (cDouble), cDouble)
in
  fun tan(x : real) = call(x)
end;
(* Circular cotangent. cot(x : real) : real *)
local
  val call = buildCall1(getSymbol lib "cot", (cDouble), cDouble)
in
  fun cot(x : real) = call(x)
end;
(* Circular tangent of argument in degrees. tandg(x : real) : real *)
local
  val call = buildCall1(getSymbol lib "tandg", (cDouble), cDouble)
in
  fun tandg(x : real) = call(x)
end;
(* Circular cotangent of argument in degrees. cotdg(x : real) : real *)
local
  val call = buildCall1(getSymbol lib "cotdg", (cDouble), cDouble)
in
  fun cotdg(x : real) = call(x)
end;
(* Hyperbolic tangent. tanh(x : real) : real *)
local
  val call = buildCall1(getSymbol lib "md_tanh", (cDouble), cDouble)
in
  fun tanh(x : real) = call(x)
end;
(* Bessel function of second kind of integer order. yn(n : int, x : real) : real *)
local
  val call = buildCall2(getSymbol lib "yn", (cInt, cDouble), cDouble)
in
  fun yn(n : int, x : real) = call(n, x)
end;
(* Riemann zetac function. zetac(x : real) : real *)
local
  val call = buildCall1(getSymbol lib "zetac", (cDouble), cDouble)
in
  fun zetac(x : real) = call(x)
end;

end;
