structure Cephes = struct
type cmplx = real * real;
(* 2**-53 *)
val MACHEP = _import "MACHEP" : () -> real;
(* log(2**1024) *)
val MAXLOG = _import "MAXLOG" : () -> real;
(* log(2**-1022) *)
val MINLOG = _import "MINLOG" : () -> real;
(* 2**1024 *)
val MAXNUM = _import "MAXNUM" : () -> real;
(* pi *)
val PI = _import "PI" : () -> real;
(* pi/2 *)
val PIO2 = _import "PIO2" : () -> real;
(* pi/4 *)
val PIO4 = _import "PIO4" : () -> real;
(* sqrt(2) *)
val SQRT2 = _import "SQRT2" : () -> real;
(* sqrt(2)/2 *)
val SQRTH = _import "SQRTH" : () -> real;
(* 1/log(2) *)
val LOG2E = _import "LOG2E" : () -> real;
(* sqrt(2/pi) *)
val SQ2OPI = _import "SQ2OPI" : () -> real;
(* log(2) *)
val LOGE2 = _import "LOGE2" : () -> real;
(* log(2)/2 *)
val LOGSQ2 = _import "LOGSQ2" : () -> real;
(* 2*pi/4 *)
val THPIO4 = _import "THPIO4" : () -> real;
(* 2/pi *)
val TWOOPI = _import "TWOOPI" : () -> real;
(* Inverse hyperbolic cosine. acosh(x : real) : real *)
local
  val call = _import "md_acosh" : (real) -> real;
in
  fun acosh(x : real) = call(x)
end;
(* Airy function. airy(x : real) : (int, real, real, real, real) *)
local
  val call = _import "airy" : (real, real ref, real ref, real ref, real ref) -> int;
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
  val call = _import "md_asin" : (real) -> real;
in
  fun asin(x : real) = call(x)
end;
(* Inverse circular cosine. acos(x : real) : real *)
local
  val call = _import "md_acos" : (real) -> real;
in
  fun acos(x : real) = call(x)
end;
(* Inverse hyperbolic sine. asinh(x : real) : real *)
local
  val call = _import "md_asinh" : (real) -> real;
in
  fun asinh(x : real) = call(x)
end;
(* Inverse circular tangent (arctangent). atan(x : real) : real *)
local
  val call = _import "md_atan" : (real) -> real;
in
  fun atan(x : real) = call(x)
end;
(* Quadrant correct inverse circular tangent. atan2(y : real, x : real) : real *)
local
  val call = _import "md_atan2" : (real, real) -> real;
in
  fun atan2(y : real, x : real) = call(y, x)
end;
(* Inverse hyperbolic tangent. atanh(x : real) : real *)
local
  val call = _import "md_atanh" : (real) -> real;
in
  fun atanh(x : real) = call(x)
end;
(* Binomial distribution. bdtr(k : int, n : int, p : real) : real *)
local
  val call = _import "bdtr" : (int, int, real) -> real;
in
  fun bdtr(k : int, n : int, p : real) = call(k, n, p)
end;
(* Complemented binomial distribution. bdtrc(k : int, n : int, p : real) : real *)
local
  val call = _import "bdtrc" : (int, int, real) -> real;
in
  fun bdtrc(k : int, n : int, p : real) = call(k, n, p)
end;
(* Inverse binomial distribution. bdtri(k : int, n : int, y : real) : real *)
local
  val call = _import "bdtri" : (int, int, real) -> real;
in
  fun bdtri(k : int, n : int, y : real) = call(k, n, y)
end;
(* Beta function. beta(a : real, b : real) : real *)
local
  val call = _import "beta" : (real, real) -> real;
in
  fun beta(a : real, b : real) = call(a, b)
end;
(* Beta distribution. btdtr(a : real, b : real, x : real) : real *)
local
  val call = _import "btdtr" : (real, real, real) -> real;
in
  fun btdtr(a : real, b : real, x : real) = call(a, b, x)
end;
(* Cube root. cbrt(x : real) : real *)
local
  val call = _import "md_cbrt" : (real) -> real;
in
  fun cbrt(x : real) = call(x)
end;
(* Evaluate Chebyshev series. chbevl(x : real, coef : real Vector.vector) : real *)
local
  val call = _import "chbevl" : (real, real Vector.vector, int) -> real;
in
  fun chbevl(x : real, coef : real Vector.vector) = call(x, coef, Vector.length coef)
end;
(* Chi-square distribution. chdtr(df : real, x : real) : real *)
local
  val call = _import "chdtr" : (real, real) -> real;
in
  fun chdtr(df : real, x : real) = call(df, x)
end;
(* Complemented Chi-square distribution. chdtrc(df : real, x : real) : real *)
local
  val call = _import "chdtrc" : (real, real) -> real;
in
  fun chdtrc(df : real, x : real) = call(df, x)
end;
(* Inverse of complemented Chi-square distribution. chdtri(df : real, x : real) : real *)
local
  val call = _import "chdtri" : (real, real) -> real;
in
  fun chdtri(df : real, x : real) = call(df, x)
end;
(* Complex natural logarithm. clog(x : cmplx) : cmplx *)
local
  val call = _import "md_clog" : (cmplx, real array) -> ();
in
  fun clog(x : cmplx) =
    let
      val y = Array.array (2, 0.0)
      val _ = call(x, y)
    in
      ((Array.sub(y, 0), Array.sub(y, 1)) : cmplx)
    end
end;
(* Complex exponential function. cexp(x : cmplx) : cmplx *)
local
  val call = _import "md_cexp" : (cmplx, real array) -> ();
in
  fun cexp(x : cmplx) =
    let
      val y = Array.array (2, 0.0)
      val _ = call(x, y)
    in
      ((Array.sub(y, 0), Array.sub(y, 1)) : cmplx)
    end
end;
(* Complex circular sine. csin(x : cmplx) : cmplx *)
local
  val call = _import "md_csin" : (cmplx, real array) -> ();
in
  fun csin(x : cmplx) =
    let
      val y = Array.array (2, 0.0)
      val _ = call(x, y)
    in
      ((Array.sub(y, 0), Array.sub(y, 1)) : cmplx)
    end
end;
(* Complex circular cosine. ccos(x : cmplx) : cmplx *)
local
  val call = _import "md_ccos" : (cmplx, real array) -> ();
in
  fun ccos(x : cmplx) =
    let
      val y = Array.array (2, 0.0)
      val _ = call(x, y)
    in
      ((Array.sub(y, 0), Array.sub(y, 1)) : cmplx)
    end
end;
(* Complex circular tangent. ctan(x : cmplx) : cmplx *)
local
  val call = _import "md_ctan" : (cmplx, real array) -> ();
in
  fun ctan(x : cmplx) =
    let
      val y = Array.array (2, 0.0)
      val _ = call(x, y)
    in
      ((Array.sub(y, 0), Array.sub(y, 1)) : cmplx)
    end
end;
(* Complex circular cotangent. ccot(x : cmplx) : cmplx *)
local
  val call = _import "ccot" : (cmplx, real array) -> ();
in
  fun ccot(x : cmplx) =
    let
      val y = Array.array (2, 0.0)
      val _ = call(x, y)
    in
      ((Array.sub(y, 0), Array.sub(y, 1)) : cmplx)
    end
end;
(* Complex circular arc sine. casin(x : cmplx) : cmplx *)
local
  val call = _import "md_casin" : (cmplx, real array) -> ();
in
  fun casin(x : cmplx) =
    let
      val y = Array.array (2, 0.0)
      val _ = call(x, y)
    in
      ((Array.sub(y, 0), Array.sub(y, 1)) : cmplx)
    end
end;
(* Complex circular arc cosine. cacos(x : cmplx) : cmplx *)
local
  val call = _import "md_cacos" : (cmplx, real array) -> ();
in
  fun cacos(x : cmplx) =
    let
      val y = Array.array (2, 0.0)
      val _ = call(x, y)
    in
      ((Array.sub(y, 0), Array.sub(y, 1)) : cmplx)
    end
end;
(* Complex circular arc tangent. catan(x : cmplx) : cmplx *)
local
  val call = _import "md_catan" : (cmplx, real array) -> ();
in
  fun catan(x : cmplx) =
    let
      val y = Array.array (2, 0.0)
      val _ = call(x, y)
    in
      ((Array.sub(y, 0), Array.sub(y, 1)) : cmplx)
    end
end;
(* Complex hyperbolic sine. csinh(x : cmplx) : cmplx *)
local
  val call = _import "md_csinh" : (cmplx, real array) -> ();
in
  fun csinh(x : cmplx) =
    let
      val y = Array.array (2, 0.0)
      val _ = call(x, y)
    in
      ((Array.sub(y, 0), Array.sub(y, 1)) : cmplx)
    end
end;
(* Complex hyperbolic cosine. ccosh(x : cmplx) : cmplx *)
local
  val call = _import "md_ccosh" : (cmplx, real array) -> ();
in
  fun ccosh(x : cmplx) =
    let
      val y = Array.array (2, 0.0)
      val _ = call(x, y)
    in
      ((Array.sub(y, 0), Array.sub(y, 1)) : cmplx)
    end
end;
(* Complex inverse hyperbolic cosine. cacosh(x : cmplx) : cmplx *)
local
  val call = _import "md_cacosh" : (cmplx, real array) -> ();
in
  fun cacosh(x : cmplx) =
    let
      val y = Array.array (2, 0.0)
      val _ = call(x, y)
    in
      ((Array.sub(y, 0), Array.sub(y, 1)) : cmplx)
    end
end;
(* Complex hyperbolic tangent. ctanh(x : cmplx) : cmplx *)
local
  val call = _import "md_ctanh" : (cmplx, real array) -> ();
in
  fun ctanh(x : cmplx) =
    let
      val y = Array.array (2, 0.0)
      val _ = call(x, y)
    in
      ((Array.sub(y, 0), Array.sub(y, 1)) : cmplx)
    end
end;
(* Complex inverse hyperbolic tangent. catanh(x : cmplx) : cmplx *)
local
  val call = _import "md_catanh" : (cmplx, real array) -> ();
in
  fun catanh(x : cmplx) =
    let
      val y = Array.array (2, 0.0)
      val _ = call(x, y)
    in
      ((Array.sub(y, 0), Array.sub(y, 1)) : cmplx)
    end
end;
(* Complex power function. cpow(x : cmplx, y : cmplx) : cmplx *)
local
  val call = _import "md_cpow" : (cmplx, cmplx, real array) -> ();
in
  fun cpow(x : cmplx, y : cmplx) =
    let
      val z = Array.array (2, 0.0)
      val _ = call(x, y, z)
    in
      ((Array.sub(z, 0), Array.sub(z, 1)) : cmplx)
    end
end;
(* Complex addition. cadd(x : cmplx, y : cmplx) : cmplx *)
local
  val call = _import "cadd" : (cmplx, cmplx, real array) -> ();
in
  fun cadd(x : cmplx, y : cmplx) =
    let
      val z = Array.array (2, 0.0)
      val _ = call(x, y, z)
    in
      ((Array.sub(z, 0), Array.sub(z, 1)) : cmplx)
    end
end;
(* Complex subtraction. csub(x : cmplx, y : cmplx) : cmplx *)
local
  val call = _import "csub" : (cmplx, cmplx, real array) -> ();
in
  fun csub(x : cmplx, y : cmplx) =
    let
      val z = Array.array (2, 0.0)
      val _ = call(x, y, z)
    in
      ((Array.sub(z, 0), Array.sub(z, 1)) : cmplx)
    end
end;
(* Complex multiplication. cmul(x : cmplx, y : cmplx) : cmplx *)
local
  val call = _import "cmul" : (cmplx, cmplx, real array) -> ();
in
  fun cmul(x : cmplx, y : cmplx) =
    let
      val z = Array.array (2, 0.0)
      val _ = call(x, y, z)
    in
      ((Array.sub(z, 0), Array.sub(z, 1)) : cmplx)
    end
end;
(* Complex division. cdiv(x : cmplx, y : cmplx) : cmplx *)
local
  val call = _import "cdiv" : (cmplx, cmplx, real array) -> ();
in
  fun cdiv(x : cmplx, y : cmplx) =
    let
      val z = Array.array (2, 0.0)
      val _ = call(x, y, z)
    in
      ((Array.sub(z, 0), Array.sub(z, 1)) : cmplx)
    end
end;
(* Complex absolute value. cabs(x : cmplx) : cmplx *)
local
  val call = _import "md_cabs" : (cmplx, real array) -> ();
in
  fun cabs(x : cmplx) =
    let
      val y = Array.array (2, 0.0)
      val _ = call(x, y)
    in
      ((Array.sub(y, 0), Array.sub(y, 1)) : cmplx)
    end
end;
(* Complex square root. csqrt(x : cmplx) : cmplx *)
local
  val call = _import "md_csqrt" : (cmplx, real array) -> ();
in
  fun csqrt(x : cmplx) =
    let
      val y = Array.array (2, 0.0)
      val _ = call(x, y)
    in
      ((Array.sub(y, 0), Array.sub(y, 1)) : cmplx)
    end
end;
(* Hyperbolic cosine. cosh(x : real) : real *)
local
  val call = _import "md_cosh" : (real) -> real;
in
  fun cosh(x : real) = call(x)
end;
(* Dawson's integral. dawsn(x : real) : real *)
local
  val call = _import "dawsn" : (real) -> real;
in
  fun dawsn(x : real) = call(x)
end;
(* Pseudorandom number generator. drand() : real *)
local
  val call = _import "drand" : (real ref) -> ();
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
  val call = _import "ei" : (real) -> real;
in
  fun ei(x : real) = call(x)
end;
(* Incomplete elliptic integral of the second kind. ellie(phi : real, m : real) : real *)
local
  val call = _import "ellie" : (real, real) -> real;
in
  fun ellie(phi : real, m : real) = call(phi, m)
end;
(* Incomplete elliptic integral of the first kind. ellik(phi : real, m : real) : real *)
local
  val call = _import "ellik" : (real, real) -> real;
in
  fun ellik(phi : real, m : real) = call(phi, m)
end;
(* Complete elliptic integral of the second kind. ellpe(m1 : real) : real *)
local
  val call = _import "ellpe" : (real) -> real;
in
  fun ellpe(m1 : real) = call(m1)
end;
(* Jacobian Elliptic Functions. ellpj(u : real, m : real) : (int, real, real, real, real) *)
local
  val call = _import "ellpj" : (real, real, real ref, real ref, real ref, real ref) -> int;
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
  val call = _import "ellpk" : (real) -> real;
in
  fun ellpk(m1 : real) = call(m1)
end;
(* Exponential function. exp(x : real) : real *)
local
  val call = _import "md_exp" : (real) -> real;
in
  fun exp(x : real) = call(x)
end;
(* Base 10 exponential function. exp10(x : real) : real *)
local
  val call = _import "md_exp10" : (real) -> real;
in
  fun exp10(x : real) = call(x)
end;
(* Base 2 exponential function. exp2(x : real) : real *)
local
  val call = _import "md_exp2" : (real) -> real;
in
  fun exp2(x : real) = call(x)
end;
(* Exponential integral En. expn(n : int, x : real) : real *)
local
  val call = _import "md_expn" : (int, real) -> real;
in
  fun expn(n : int, x : real) = call(n, x)
end;
(* Exponential of squared argument. expx2(x : real, sign : int) : real *)
local
  val call = _import "expx2" : (real, int) -> real;
in
  fun expx2(x : real, sign : int) = call(x, sign)
end;
(* Absolute value. fabs(x : real) : real *)
local
  val call = _import "md_fabs" : (real) -> real;
in
  fun fabs(x : real) = call(x)
end;
(* Factorial function. fac(i : int) : real *)
local
  val call = _import "fac" : (int) -> real;
in
  fun fac(i : int) = call(i)
end;
(* F distribution. fdtr(df1 : int, df2 : int, x : real) : real *)
local
  val call = _import "fdtr" : (int, int, real) -> real;
in
  fun fdtr(df1 : int, df2 : int, x : real) = call(df1, df2, x)
end;
(* Complemented F distribution. fdtrc(df1 : int, df2 : int, x : real) : real *)
local
  val call = _import "fdtrc" : (int, int, real) -> real;
in
  fun fdtrc(df1 : int, df2 : int, x : real) = call(df1, df2, x)
end;
(* Inverse of complemented F distribution. fdtri(df1 : int, df2 : int, p : real) : real *)
local
  val call = _import "fdtri" : (int, int, real) -> real;
in
  fun fdtri(df1 : int, df2 : int, p : real) = call(df1, df2, p)
end;
(* Floor function. floor(x : real) : real *)
local
  val call = _import "md_floor" : (real) -> real;
in
  fun floor(x : real) = call(x)
end;
(* Ceiling function. ceil(x : real) : real *)
local
  val call = _import "md_ceil" : (real) -> real;
in
  fun ceil(x : real) = call(x)
end;
(* frexp. frexp(x : real, expnt : int) : (real, int) *)
local
  val call = _import "frexp" : (real, int ref) -> real;
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
  val call = _import "ldexp" : (real, int) -> real;
in
  fun ldexp(x : real, n : int) = call(x, n)
end;
(* Fresnel integral. fresnl(x : real) : (real, real) *)
local
  val call = _import "fresnl" : (real, real ref, real ref) -> ();
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
  val call = _import "md_gamma" : (real) -> real;
in
  fun gamma(x : real) = call(x)
end;
(* Natural logarithm of gamma function. lgam(x : real) : real *)
local
  val call = _import "lgam" : (real) -> real;
in
  fun lgam(x : real) = call(x)
end;
(* Gamma distribution function. gdtr(a : real, b : real, x : real) : real *)
local
  val call = _import "gdtr" : (real, real, real) -> real;
in
  fun gdtr(a : real, b : real, x : real) = call(a, b, x)
end;
(* Complemented gamma distribution function. gdtrc(a : real, b : real, x : real) : real *)
local
  val call = _import "gdtrc" : (real, real, real) -> real;
in
  fun gdtrc(a : real, b : real, x : real) = call(a, b, x)
end;
(* Gauss hypergeometric function _2F_1. hyp2f1(a : real, b : real, c : real, x : real) : real *)
local
  val call = _import "hyp2f1" : (real, real, real, real) -> real;
in
  fun hyp2f1(a : real, b : real, c : real, x : real) = call(a, b, c, x)
end;
(* Confluent hypergeometric function. hyperg(a : real, b : real, x : real) : real *)
local
  val call = _import "hyperg" : (real, real, real) -> real;
in
  fun hyperg(a : real, b : real, x : real) = call(a, b, x)
end;
(* Modified Bessel function of order zero. i0(x : real) : real *)
local
  val call = _import "i0" : (real) -> real;
in
  fun i0(x : real) = call(x)
end;
(* Modified Bessel function of order zero, exponentially scaled. i0e(x : real) : real *)
local
  val call = _import "i0e" : (real) -> real;
in
  fun i0e(x : real) = call(x)
end;
(* Modified Bessel function of order one. i1(x : real) : real *)
local
  val call = _import "i1" : (real) -> real;
in
  fun i1(x : real) = call(x)
end;
(* Modified Bessel function of order one, exponentially scaled. i1e(x : real) : real *)
local
  val call = _import "i1e" : (real) -> real;
in
  fun i1e(x : real) = call(x)
end;
(* Incomplete gamma integral. igam(a : real, x : real) : real *)
local
  val call = _import "igam" : (real, real) -> real;
in
  fun igam(a : real, x : real) = call(a, x)
end;
(* Complemented incomplete gamma integral. igamc(a : real, x : real) : real *)
local
  val call = _import "igamc" : (real, real) -> real;
in
  fun igamc(a : real, x : real) = call(a, x)
end;
(* Inverse of complemented incomplete gamma integral. igami(a : real, p : real) : real *)
local
  val call = _import "igami" : (real, real) -> real;
in
  fun igami(a : real, p : real) = call(a, p)
end;
(* Incomplete beta integral. incbet(a : real, b : real, x : real) : real *)
local
  val call = _import "incbet" : (real, real, real) -> real;
in
  fun incbet(a : real, b : real, x : real) = call(a, b, x)
end;
(* Inverse of incomplete beta integral. incbi(a : real, b : real, y : real) : real *)
local
  val call = _import "incbi" : (real, real, real) -> real;
in
  fun incbi(a : real, b : real, y : real) = call(a, b, y)
end;
(* signbit. signbit(x : real) : int *)
local
  val call = _import "signbit" : (real) -> int;
in
  fun signbit(x : real) = call(x)
end;
(* isnan. isnan(x : real) : int *)
local
  val call = _import "isnan" : (real) -> int;
in
  fun isnan(x : real) = call(x)
end;
(* isfinite. isfinite(x : real) : int *)
local
  val call = _import "isfinite" : (real) -> int;
in
  fun isfinite(x : real) = call(x)
end;
(* Modified Bessel function of noninteger order. iv(v : real, x : real) : real *)
local
  val call = _import "iv" : (real, real) -> real;
in
  fun iv(v : real, x : real) = call(v, x)
end;
(* Bessel function of order zero. j0(x : real) : real *)
local
  val call = _import "j0" : (real) -> real;
in
  fun j0(x : real) = call(x)
end;
(* Bessel function of second kind, order zero. y0(x : real) : real *)
local
  val call = _import "y0" : (real) -> real;
in
  fun y0(x : real) = call(x)
end;
(* Bessel function of order one. j1(x : real) : real *)
local
  val call = _import "j1" : (real) -> real;
in
  fun j1(x : real) = call(x)
end;
(* Bessel function of second kind of order one. y1(x : real) : real *)
local
  val call = _import "y1" : (real) -> real;
in
  fun y1(x : real) = call(x)
end;
(* Bessel function of integer order. jn(n : int, x : real) : real *)
local
  val call = _import "jn" : (int, real) -> real;
in
  fun jn(n : int, x : real) = call(n, x)
end;
(* Bessel function of noninteger order. jv(v : real, x : real) : real *)
local
  val call = _import "jv" : (real, real) -> real;
in
  fun jv(v : real, x : real) = call(v, x)
end;
(* Modified Bessel function, third kind, order zero. k0(x : real) : real *)
local
  val call = _import "k0" : (real) -> real;
in
  fun k0(x : real) = call(x)
end;
(* Modified Bessel function, third kind, order zero, exponentially scaled. k0e(x : real) : real *)
local
  val call = _import "k0e" : (real) -> real;
in
  fun k0e(x : real) = call(x)
end;
(* Modified Bessel function, third kind, order one. k1(x : real) : real *)
local
  val call = _import "k1" : (real) -> real;
in
  fun k1(x : real) = call(x)
end;
(* Modified Bessel function, third kind, order one, exponentially scaled. k1e(x : real) : real *)
local
  val call = _import "k1e" : (real) -> real;
in
  fun k1e(x : real) = call(x)
end;
(* Modified Bessel function, third kind, integer order. kn(n : int, x : real) : real *)
local
  val call = _import "kn" : (int, real) -> real;
in
  fun kn(n : int, x : real) = call(n, x)
end;
(* Natural logarithm. log(x : real) : real *)
local
  val call = _import "md_log" : (real) -> real;
in
  fun log(x : real) = call(x)
end;
(* Common logarithm. log10(x : real) : real *)
local
  val call = _import "md_log10" : (real) -> real;
in
  fun log10(x : real) = call(x)
end;
(* Base 2 logarithm. log2(x : real) : real *)
local
  val call = _import "md_log2" : (real) -> real;
in
  fun log2(x : real) = call(x)
end;
(* Negative binomial distribution. nbdtr(k : int, n : int, p : real) : real *)
local
  val call = _import "nbdtr" : (int, int, real) -> real;
in
  fun nbdtr(k : int, n : int, p : real) = call(k, n, p)
end;
(* Complemented negative binomial distribution. nbdtrc(k : int, n : int, p : real) : real *)
local
  val call = _import "nbdtrc" : (int, int, real) -> real;
in
  fun nbdtrc(k : int, n : int, p : real) = call(k, n, p)
end;
(* Functional inverse of negative binomial distribution. nbdtri(k : int, n : int, y : real) : real *)
local
  val call = _import "nbdtri" : (int, int, real) -> real;
in
  fun nbdtri(k : int, n : int, y : real) = call(k, n, y)
end;
(* Normal distribution function. ndtr(x : real) : real *)
local
  val call = _import "ndtr" : (real) -> real;
in
  fun ndtr(x : real) = call(x)
end;
(* Error function. erf(x : real) : real *)
local
  val call = _import "md_erf" : (real) -> real;
in
  fun erf(x : real) = call(x)
end;
(* Complementary error function. erfc(x : real) : real *)
local
  val call = _import "md_erfc" : (real) -> real;
in
  fun erfc(x : real) = call(x)
end;
(* Inverse of Normal distribution function. ndtri(x : real) : real *)
local
  val call = _import "ndtri" : (real) -> real;
in
  fun ndtri(x : real) = call(x)
end;
(* Poisson distribution function. pdtr(k : int, m : real) : real *)
local
  val call = _import "pdtr" : (int, real) -> real;
in
  fun pdtr(k : int, m : real) = call(k, m)
end;
(* Complemented Poisson distribution. pdtrc(k : int, m : real) : real *)
local
  val call = _import "pdtrc" : (int, real) -> real;
in
  fun pdtrc(k : int, m : real) = call(k, m)
end;
(* Inverse Poisson distribution. pdtri(k : int, y : real) : real *)
local
  val call = _import "pdtri" : (int, real) -> real;
in
  fun pdtri(k : int, y : real) = call(k, y)
end;
(* Evaluate polynomial. polevl(x : real, coef : real Vector.vector) : real *)
local
  val call = _import "polevl" : (real, real Vector.vector, int) -> real;
in
  fun polevl(x : real, coef : real Vector.vector) = call(x, coef, Vector.length coef)
end;
(* Power function. pow(x : real, y : real) : real *)
local
  val call = _import "md_pow" : (real, real) -> real;
in
  fun pow(x : real, y : real) = call(x, y)
end;
(* Real raised to integer power. powi(x : real, n : int) : real *)
local
  val call = _import "md_powi" : (real, int) -> real;
in
  fun powi(x : real, n : int) = call(x, n)
end;
(* Psi (digamma) function. psi(x : real) : real *)
local
  val call = _import "psi" : (real) -> real;
in
  fun psi(x : real) = call(x)
end;
(* Reciprocal gamma function. rgamma(x : real) : real *)
local
  val call = _import "rgamma" : (real) -> real;
in
  fun rgamma(x : real) = call(x)
end;
(* Round double to nearest or even integer valued double. round(x : real) : real *)
local
  val call = _import "md_round" : (real) -> real;
in
  fun round(x : real) = call(x)
end;
(* Hyperbolic sine and cosine integrals. shichi(x : real) : (real, real, real) *)
local
  val call = _import "shichi" : (real, real ref, real ref) -> real;
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
  val call = _import "sici" : (real, real ref, real ref) -> real;
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
  val call = _import "md_sin" : (real) -> real;
in
  fun sin(x : real) = call(x)
end;
(* Circular cosine. cos(x : real) : real *)
local
  val call = _import "md_cos" : (real) -> real;
in
  fun cos(x : real) = call(x)
end;
(* Circular sine of angle in degrees. sindg(x : real) : real *)
local
  val call = _import "md_sindg" : (real) -> real;
in
  fun sindg(x : real) = call(x)
end;
(* Circular cosine of angle in degrees. cosdg(x : real) : real *)
local
  val call = _import "cosdg" : (real) -> real;
in
  fun cosdg(x : real) = call(x)
end;
(* Hyperbolic sine. sinh(x : real) : real *)
local
  val call = _import "md_sinh" : (real) -> real;
in
  fun sinh(x : real) = call(x)
end;
(* Dilogarithm. spence(x : real) : real *)
local
  val call = _import "spence" : (real) -> real;
in
  fun spence(x : real) = call(x)
end;
(* Square root. sqrt(x : real) : real *)
local
  val call = _import "sqrt" : (real) -> real;
in
  fun sqrt(x : real) = call(x)
end;
(* Student's t distribution. stdtr(k : int, t : real) : real *)
local
  val call = _import "stdtr" : (int, real) -> real;
in
  fun stdtr(k : int, t : real) = call(k, t)
end;
(* Functional inverse of Student's t distribution. stdtri(k : int, p : real) : real *)
local
  val call = _import "stdtri" : (int, real) -> real;
in
  fun stdtri(k : int, p : real) = call(k, p)
end;
(* Struve function. struve(v : int, x : real) : real *)
local
  val call = _import "struve" : (int, real) -> real;
in
  fun struve(v : int, x : real) = call(v, x)
end;
(* Circular tangent. tan(x : real) : real *)
local
  val call = _import "md_tan" : (real) -> real;
in
  fun tan(x : real) = call(x)
end;
(* Circular cotangent. cot(x : real) : real *)
local
  val call = _import "cot" : (real) -> real;
in
  fun cot(x : real) = call(x)
end;
(* Circular tangent of argument in degrees. tandg(x : real) : real *)
local
  val call = _import "tandg" : (real) -> real;
in
  fun tandg(x : real) = call(x)
end;
(* Circular cotangent of argument in degrees. cotdg(x : real) : real *)
local
  val call = _import "cotdg" : (real) -> real;
in
  fun cotdg(x : real) = call(x)
end;
(* Hyperbolic tangent. tanh(x : real) : real *)
local
  val call = _import "md_tanh" : (real) -> real;
in
  fun tanh(x : real) = call(x)
end;
(* Bessel function of second kind of integer order. yn(n : int, x : real) : real *)
local
  val call = _import "yn" : (int, real) -> real;
in
  fun yn(n : int, x : real) = call(n, x)
end;
(* Riemann zetac function. zetac(x : real) : real *)
local
  val call = _import "zetac" : (real) -> real;
in
  fun zetac(x : real) = call(x)
end;

end;
