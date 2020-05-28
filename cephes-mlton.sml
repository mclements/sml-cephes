structure Cephes :> CEPHES = struct
type cmplx = real * real;
(* 2**-53 *)
local
  val addr = _address "MACHEP" : MLton.Pointer.t;
in
  val MACHEP = MLton.Pointer.getReal64(addr, 0)
end;
(* log(2**1024) *)
local
  val addr = _address "MAXLOG" : MLton.Pointer.t;
in
  val MAXLOG = MLton.Pointer.getReal64(addr, 0)
end;
(* log(2**-1022) *)
local
  val addr = _address "MINLOG" : MLton.Pointer.t;
in
  val MINLOG = MLton.Pointer.getReal64(addr, 0)
end;
(* 2**1024 *)
local
  val addr = _address "MAXNUM" : MLton.Pointer.t;
in
  val MAXNUM = MLton.Pointer.getReal64(addr, 0)
end;
(* pi *)
local
  val addr = _address "PI" : MLton.Pointer.t;
in
  val PI = MLton.Pointer.getReal64(addr, 0)
end;
(* pi/2 *)
local
  val addr = _address "PIO2" : MLton.Pointer.t;
in
  val PIO2 = MLton.Pointer.getReal64(addr, 0)
end;
(* pi/4 *)
local
  val addr = _address "PIO4" : MLton.Pointer.t;
in
  val PIO4 = MLton.Pointer.getReal64(addr, 0)
end;
(* sqrt(2) *)
local
  val addr = _address "SQRT2" : MLton.Pointer.t;
in
  val SQRT2 = MLton.Pointer.getReal64(addr, 0)
end;
(* sqrt(2)/2 *)
local
  val addr = _address "SQRTH" : MLton.Pointer.t;
in
  val SQRTH = MLton.Pointer.getReal64(addr, 0)
end;
(* 1/log(2) *)
local
  val addr = _address "LOG2E" : MLton.Pointer.t;
in
  val LOG2E = MLton.Pointer.getReal64(addr, 0)
end;
(* sqrt(2/pi) *)
local
  val addr = _address "SQ2OPI" : MLton.Pointer.t;
in
  val SQ2OPI = MLton.Pointer.getReal64(addr, 0)
end;
(* log(2) *)
local
  val addr = _address "LOGE2" : MLton.Pointer.t;
in
  val LOGE2 = MLton.Pointer.getReal64(addr, 0)
end;
(* log(2)/2 *)
local
  val addr = _address "LOGSQ2" : MLton.Pointer.t;
in
  val LOGSQ2 = MLton.Pointer.getReal64(addr, 0)
end;
(* 2*pi/4 *)
local
  val addr = _address "THPIO4" : MLton.Pointer.t;
in
  val THPIO4 = MLton.Pointer.getReal64(addr, 0)
end;
(* 2/pi *)
local
  val addr = _address "TWOOPI" : MLton.Pointer.t;
in
  val TWOOPI = MLton.Pointer.getReal64(addr, 0)
end;
(* Inverse hyperbolic cosine. acosh(x : real) : real *)
local
  val call = _import "md_acosh" public: real -> real;
in
  fun acosh(x : real) = call(x)
end;
(* Airy function. airy(x : real) : (int, real, real, real, real) *)
local
  val call = _import "airy" public: real * real ref * real ref * real ref * real ref -> int;
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
  val call = _import "md_asin" public: real -> real;
in
  fun asin(x : real) = call(x)
end;
(* Inverse circular cosine. acos(x : real) : real *)
local
  val call = _import "md_acos" public: real -> real;
in
  fun acos(x : real) = call(x)
end;
(* Inverse hyperbolic sine. asinh(x : real) : real *)
local
  val call = _import "md_asinh" public: real -> real;
in
  fun asinh(x : real) = call(x)
end;
(* Inverse circular tangent (arctangent). atan(x : real) : real *)
local
  val call = _import "md_atan" public: real -> real;
in
  fun atan(x : real) = call(x)
end;
(* Quadrant correct inverse circular tangent. atan2(y : real, x : real) : real *)
local
  val call = _import "md_atan2" public: real * real -> real;
in
  fun atan2(y : real, x : real) = call(y, x)
end;
(* Inverse hyperbolic tangent. atanh(x : real) : real *)
local
  val call = _import "md_atanh" public: real -> real;
in
  fun atanh(x : real) = call(x)
end;
(* Binomial distribution. bdtr(k : int, n : int, p : real) : real *)
local
  val call = _import "bdtr" public: int * int * real -> real;
in
  fun bdtr(k : int, n : int, p : real) = call(k, n, p)
end;
(* Complemented binomial distribution. bdtrc(k : int, n : int, p : real) : real *)
local
  val call = _import "bdtrc" public: int * int * real -> real;
in
  fun bdtrc(k : int, n : int, p : real) = call(k, n, p)
end;
(* Inverse binomial distribution. bdtri(k : int, n : int, y : real) : real *)
local
  val call = _import "bdtri" public: int * int * real -> real;
in
  fun bdtri(k : int, n : int, y : real) = call(k, n, y)
end;
(* Beta function. beta(a : real, b : real) : real *)
local
  val call = _import "beta" public: real * real -> real;
in
  fun beta(a : real, b : real) = call(a, b)
end;
(* Beta distribution. btdtr(a : real, b : real, x : real) : real *)
local
  val call = _import "btdtr" public: real * real * real -> real;
in
  fun btdtr(a : real, b : real, x : real) = call(a, b, x)
end;
(* Cube root. cbrt(x : real) : real *)
local
  val call = _import "md_cbrt" public: real -> real;
in
  fun cbrt(x : real) = call(x)
end;
(* Evaluate Chebyshev series. chbevl(x : real, coef : real Vector.vector) : real *)
local
  val call = _import "chbevl" public: real * real Vector.vector * int -> real;
in
  fun chbevl(x : real, coef : real Vector.vector) = call(x, coef, Vector.length coef)
end;
(* Chi-square distribution. chdtr(df : real, x : real) : real *)
local
  val call = _import "chdtr" public: real * real -> real;
in
  fun chdtr(df : real, x : real) = call(df, x)
end;
(* Complemented Chi-square distribution. chdtrc(df : real, x : real) : real *)
local
  val call = _import "chdtrc" public: real * real -> real;
in
  fun chdtrc(df : real, x : real) = call(df, x)
end;
(* Inverse of complemented Chi-square distribution. chdtri(df : real, x : real) : real *)
local
  val call = _import "chdtri" public: real * real -> real;
in
  fun chdtri(df : real, x : real) = call(df, x)
end;
(* Complex natural logarithm. clog(x : cmplx) : cmplx *)
local
  val call = _import "md_clog" public: cmplx ref * cmplx ref -> unit;
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
  val call = _import "md_cexp" public: cmplx ref * cmplx ref -> unit;
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
  val call = _import "md_csin" public: cmplx ref * cmplx ref -> unit;
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
  val call = _import "md_ccos" public: cmplx ref * cmplx ref -> unit;
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
  val call = _import "md_ctan" public: cmplx ref * cmplx ref -> unit;
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
  val call = _import "ccot" public: cmplx ref * cmplx ref -> unit;
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
  val call = _import "md_casin" public: cmplx ref * cmplx ref -> unit;
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
  val call = _import "md_cacos" public: cmplx ref * cmplx ref -> unit;
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
  val call = _import "md_catan" public: cmplx ref * cmplx ref -> unit;
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
  val call = _import "md_csinh" public: cmplx ref * cmplx ref -> unit;
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
  val call = _import "md_ccosh" public: cmplx ref * cmplx ref -> unit;
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
  val call = _import "md_cacosh" public: cmplx ref * cmplx ref -> unit;
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
  val call = _import "md_ctanh" public: cmplx ref * cmplx ref -> unit;
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
  val call = _import "md_catanh" public: cmplx ref * cmplx ref -> unit;
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
  val call = _import "md_cpow" public: cmplx ref * cmplx ref * cmplx ref -> unit;
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
  val call = _import "cadd" public: cmplx ref * cmplx ref * cmplx ref -> unit;
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
  val call = _import "csub" public: cmplx ref * cmplx ref * cmplx ref -> unit;
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
  val call = _import "cmul" public: cmplx ref * cmplx ref * cmplx ref -> unit;
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
  val call = _import "cdiv" public: cmplx ref * cmplx ref * cmplx ref -> unit;
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
  val call = _import "md_cabs" public: cmplx ref * cmplx ref -> unit;
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
  val call = _import "md_csqrt" public: cmplx ref * cmplx ref -> unit;
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
  val call = _import "md_cosh" public: real -> real;
in
  fun cosh(x : real) = call(x)
end;
(* Dawson's integral. dawsn(x : real) : real *)
local
  val call = _import "dawsn" public: real -> real;
in
  fun dawsn(x : real) = call(x)
end;
(* Pseudorandom number generator. drand() : real *)
local
  val call = _import "drand" public: real ref -> unit;
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
  val call = _import "ei" public: real -> real;
in
  fun ei(x : real) = call(x)
end;
(* Incomplete elliptic integral of the second kind. ellie(phi : real, m : real) : real *)
local
  val call = _import "ellie" public: real * real -> real;
in
  fun ellie(phi : real, m : real) = call(phi, m)
end;
(* Incomplete elliptic integral of the first kind. ellik(phi : real, m : real) : real *)
local
  val call = _import "ellik" public: real * real -> real;
in
  fun ellik(phi : real, m : real) = call(phi, m)
end;
(* Complete elliptic integral of the second kind. ellpe(m1 : real) : real *)
local
  val call = _import "ellpe" public: real -> real;
in
  fun ellpe(m1 : real) = call(m1)
end;
(* Jacobian Elliptic Functions. ellpj(u : real, m : real) : (int, real, real, real, real) *)
local
  val call = _import "ellpj" public: real * real * real ref * real ref * real ref * real ref -> int;
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
  val call = _import "ellpk" public: real -> real;
in
  fun ellpk(m1 : real) = call(m1)
end;
(* Exponential function. exp(x : real) : real *)
local
  val call = _import "md_exp" public: real -> real;
in
  fun exp(x : real) = call(x)
end;
(* Base 10 exponential function. exp10(x : real) : real *)
local
  val call = _import "md_exp10" public: real -> real;
in
  fun exp10(x : real) = call(x)
end;
(* Base 2 exponential function. exp2(x : real) : real *)
local
  val call = _import "md_exp2" public: real -> real;
in
  fun exp2(x : real) = call(x)
end;
(* Exponential integral En. expn(n : int, x : real) : real *)
local
  val call = _import "md_expn" public: int * real -> real;
in
  fun expn(n : int, x : real) = call(n, x)
end;
(* Exponential of squared argument. expx2(x : real, sign : int) : real *)
local
  val call = _import "expx2" public: real * int -> real;
in
  fun expx2(x : real, sign : int) = call(x, sign)
end;
(* Absolute value. fabs(x : real) : real *)
local
  val call = _import "md_fabs" public: real -> real;
in
  fun fabs(x : real) = call(x)
end;
(* Factorial function. fac(i : int) : real *)
local
  val call = _import "fac" public: int -> real;
in
  fun fac(i : int) = call(i)
end;
(* F distribution. fdtr(df1 : int, df2 : int, x : real) : real *)
local
  val call = _import "fdtr" public: int * int * real -> real;
in
  fun fdtr(df1 : int, df2 : int, x : real) = call(df1, df2, x)
end;
(* Complemented F distribution. fdtrc(df1 : int, df2 : int, x : real) : real *)
local
  val call = _import "fdtrc" public: int * int * real -> real;
in
  fun fdtrc(df1 : int, df2 : int, x : real) = call(df1, df2, x)
end;
(* Inverse of complemented F distribution. fdtri(df1 : int, df2 : int, p : real) : real *)
local
  val call = _import "fdtri" public: int * int * real -> real;
in
  fun fdtri(df1 : int, df2 : int, p : real) = call(df1, df2, p)
end;
(* Floor function. floor(x : real) : real *)
local
  val call = _import "md_floor" public: real -> real;
in
  fun floor(x : real) = call(x)
end;
(* Ceiling function. ceil(x : real) : real *)
local
  val call = _import "md_ceil" public: real -> real;
in
  fun ceil(x : real) = call(x)
end;
(* frexp. frexp(x : real, expnt : int) : (real, int) *)
local
  val call = _import "frexp" public: real * int ref -> real;
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
  val call = _import "ldexp" public: real * int -> real;
in
  fun ldexp(x : real, n : int) = call(x, n)
end;
(* Fresnel integral. fresnl(x : real) : (real, real) *)
local
  val call = _import "fresnl" public: real * real ref * real ref -> unit;
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
  val call = _import "md_gamma" public: real -> real;
in
  fun gamma(x : real) = call(x)
end;
(* Natural logarithm of gamma function. lgam(x : real) : real *)
local
  val call = _import "lgam" public: real -> real;
in
  fun lgam(x : real) = call(x)
end;
(* Gamma distribution function. gdtr(a : real, b : real, x : real) : real *)
local
  val call = _import "gdtr" public: real * real * real -> real;
in
  fun gdtr(a : real, b : real, x : real) = call(a, b, x)
end;
(* Complemented gamma distribution function. gdtrc(a : real, b : real, x : real) : real *)
local
  val call = _import "gdtrc" public: real * real * real -> real;
in
  fun gdtrc(a : real, b : real, x : real) = call(a, b, x)
end;
(* Gauss hypergeometric function _2F_1. hyp2f1(a : real, b : real, c : real, x : real) : real *)
local
  val call = _import "hyp2f1" public: real * real * real * real -> real;
in
  fun hyp2f1(a : real, b : real, c : real, x : real) = call(a, b, c, x)
end;
(* Confluent hypergeometric function. hyperg(a : real, b : real, x : real) : real *)
local
  val call = _import "hyperg" public: real * real * real -> real;
in
  fun hyperg(a : real, b : real, x : real) = call(a, b, x)
end;
(* Modified Bessel function of order zero. i0(x : real) : real *)
local
  val call = _import "i0" public: real -> real;
in
  fun i0(x : real) = call(x)
end;
(* Modified Bessel function of order zero, exponentially scaled. i0e(x : real) : real *)
local
  val call = _import "i0e" public: real -> real;
in
  fun i0e(x : real) = call(x)
end;
(* Modified Bessel function of order one. i1(x : real) : real *)
local
  val call = _import "i1" public: real -> real;
in
  fun i1(x : real) = call(x)
end;
(* Modified Bessel function of order one, exponentially scaled. i1e(x : real) : real *)
local
  val call = _import "i1e" public: real -> real;
in
  fun i1e(x : real) = call(x)
end;
(* Incomplete gamma integral. igam(a : real, x : real) : real *)
local
  val call = _import "igam" public: real * real -> real;
in
  fun igam(a : real, x : real) = call(a, x)
end;
(* Complemented incomplete gamma integral. igamc(a : real, x : real) : real *)
local
  val call = _import "igamc" public: real * real -> real;
in
  fun igamc(a : real, x : real) = call(a, x)
end;
(* Inverse of complemented incomplete gamma integral. igami(a : real, p : real) : real *)
local
  val call = _import "igami" public: real * real -> real;
in
  fun igami(a : real, p : real) = call(a, p)
end;
(* Incomplete beta integral. incbet(a : real, b : real, x : real) : real *)
local
  val call = _import "incbet" public: real * real * real -> real;
in
  fun incbet(a : real, b : real, x : real) = call(a, b, x)
end;
(* Inverse of incomplete beta integral. incbi(a : real, b : real, y : real) : real *)
local
  val call = _import "incbi" public: real * real * real -> real;
in
  fun incbi(a : real, b : real, y : real) = call(a, b, y)
end;
(* signbit. signbit(x : real) : int *)
local
  val call = _import "signbit" public: real -> int;
in
  fun signbit(x : real) = call(x)
end;
(* isnan. isnan(x : real) : int *)
local
  val call = _import "isnan" public: real -> int;
in
  fun isnan(x : real) = call(x)
end;
(* isfinite. isfinite(x : real) : int *)
local
  val call = _import "isfinite" public: real -> int;
in
  fun isfinite(x : real) = call(x)
end;
(* Modified Bessel function of noninteger order. iv(v : real, x : real) : real *)
local
  val call = _import "iv" public: real * real -> real;
in
  fun iv(v : real, x : real) = call(v, x)
end;
(* Bessel function of order zero. j0(x : real) : real *)
local
  val call = _import "j0" public: real -> real;
in
  fun j0(x : real) = call(x)
end;
(* Bessel function of second kind, order zero. y0(x : real) : real *)
local
  val call = _import "y0" public: real -> real;
in
  fun y0(x : real) = call(x)
end;
(* Bessel function of order one. j1(x : real) : real *)
local
  val call = _import "j1" public: real -> real;
in
  fun j1(x : real) = call(x)
end;
(* Bessel function of second kind of order one. y1(x : real) : real *)
local
  val call = _import "y1" public: real -> real;
in
  fun y1(x : real) = call(x)
end;
(* Bessel function of integer order. jn(n : int, x : real) : real *)
local
  val call = _import "jn" public: int * real -> real;
in
  fun jn(n : int, x : real) = call(n, x)
end;
(* Bessel function of noninteger order. jv(v : real, x : real) : real *)
local
  val call = _import "jv" public: real * real -> real;
in
  fun jv(v : real, x : real) = call(v, x)
end;
(* Modified Bessel function, third kind, order zero. k0(x : real) : real *)
local
  val call = _import "k0" public: real -> real;
in
  fun k0(x : real) = call(x)
end;
(* Modified Bessel function, third kind, order zero, exponentially scaled. k0e(x : real) : real *)
local
  val call = _import "k0e" public: real -> real;
in
  fun k0e(x : real) = call(x)
end;
(* Modified Bessel function, third kind, order one. k1(x : real) : real *)
local
  val call = _import "k1" public: real -> real;
in
  fun k1(x : real) = call(x)
end;
(* Modified Bessel function, third kind, order one, exponentially scaled. k1e(x : real) : real *)
local
  val call = _import "k1e" public: real -> real;
in
  fun k1e(x : real) = call(x)
end;
(* Modified Bessel function, third kind, integer order. kn(n : int, x : real) : real *)
local
  val call = _import "kn" public: int * real -> real;
in
  fun kn(n : int, x : real) = call(n, x)
end;
(* Natural logarithm. log(x : real) : real *)
local
  val call = _import "md_log" public: real -> real;
in
  fun log(x : real) = call(x)
end;
(* Common logarithm. log10(x : real) : real *)
local
  val call = _import "md_log10" public: real -> real;
in
  fun log10(x : real) = call(x)
end;
(* Base 2 logarithm. log2(x : real) : real *)
local
  val call = _import "md_log2" public: real -> real;
in
  fun log2(x : real) = call(x)
end;
(* Negative binomial distribution. nbdtr(k : int, n : int, p : real) : real *)
local
  val call = _import "nbdtr" public: int * int * real -> real;
in
  fun nbdtr(k : int, n : int, p : real) = call(k, n, p)
end;
(* Complemented negative binomial distribution. nbdtrc(k : int, n : int, p : real) : real *)
local
  val call = _import "nbdtrc" public: int * int * real -> real;
in
  fun nbdtrc(k : int, n : int, p : real) = call(k, n, p)
end;
(* Functional inverse of negative binomial distribution. nbdtri(k : int, n : int, y : real) : real *)
local
  val call = _import "nbdtri" public: int * int * real -> real;
in
  fun nbdtri(k : int, n : int, y : real) = call(k, n, y)
end;
(* Normal distribution function. ndtr(x : real) : real *)
local
  val call = _import "ndtr" public: real -> real;
in
  fun ndtr(x : real) = call(x)
end;
(* Error function. erf(x : real) : real *)
local
  val call = _import "md_erf" public: real -> real;
in
  fun erf(x : real) = call(x)
end;
(* Complementary error function. erfc(x : real) : real *)
local
  val call = _import "md_erfc" public: real -> real;
in
  fun erfc(x : real) = call(x)
end;
(* Inverse of Normal distribution function. ndtri(x : real) : real *)
local
  val call = _import "ndtri" public: real -> real;
in
  fun ndtri(x : real) = call(x)
end;
(* Poisson distribution function. pdtr(k : int, m : real) : real *)
local
  val call = _import "pdtr" public: int * real -> real;
in
  fun pdtr(k : int, m : real) = call(k, m)
end;
(* Complemented Poisson distribution. pdtrc(k : int, m : real) : real *)
local
  val call = _import "pdtrc" public: int * real -> real;
in
  fun pdtrc(k : int, m : real) = call(k, m)
end;
(* Inverse Poisson distribution. pdtri(k : int, y : real) : real *)
local
  val call = _import "pdtri" public: int * real -> real;
in
  fun pdtri(k : int, y : real) = call(k, y)
end;
(* Evaluate polynomial. polevl(x : real, coef : real Vector.vector) : real *)
local
  val call = _import "polevl" public: real * real Vector.vector * int -> real;
in
  fun polevl(x : real, coef : real Vector.vector) = call(x, coef, Vector.length coef)
end;
(* Power function. pow(x : real, y : real) : real *)
local
  val call = _import "md_pow" public: real * real -> real;
in
  fun pow(x : real, y : real) = call(x, y)
end;
(* Real raised to integer power. powi(x : real, n : int) : real *)
local
  val call = _import "md_powi" public: real * int -> real;
in
  fun powi(x : real, n : int) = call(x, n)
end;
(* Psi (digamma) function. psi(x : real) : real *)
local
  val call = _import "psi" public: real -> real;
in
  fun psi(x : real) = call(x)
end;
(* Reciprocal gamma function. rgamma(x : real) : real *)
local
  val call = _import "rgamma" public: real -> real;
in
  fun rgamma(x : real) = call(x)
end;
(* Round double to nearest or even integer valued double. round(x : real) : real *)
local
  val call = _import "md_round" public: real -> real;
in
  fun round(x : real) = call(x)
end;
(* Hyperbolic sine and cosine integrals. shichi(x : real) : (real, real, real) *)
local
  val call = _import "shichi" public: real * real ref * real ref -> real;
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
  val call = _import "sici" public: real * real ref * real ref -> real;
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
  val call = _import "md_sin" public: real -> real;
in
  fun sin(x : real) = call(x)
end;
(* Circular cosine. cos(x : real) : real *)
local
  val call = _import "md_cos" public: real -> real;
in
  fun cos(x : real) = call(x)
end;
(* Circular sine of angle in degrees. sindg(x : real) : real *)
local
  val call = _import "md_sindg" public: real -> real;
in
  fun sindg(x : real) = call(x)
end;
(* Circular cosine of angle in degrees. cosdg(x : real) : real *)
local
  val call = _import "cosdg" public: real -> real;
in
  fun cosdg(x : real) = call(x)
end;
(* Hyperbolic sine. sinh(x : real) : real *)
local
  val call = _import "md_sinh" public: real -> real;
in
  fun sinh(x : real) = call(x)
end;
(* Dilogarithm. spence(x : real) : real *)
local
  val call = _import "spence" public: real -> real;
in
  fun spence(x : real) = call(x)
end;
(* Square root. sqrt(x : real) : real *)
local
  val call = _import "sqrt" public: real -> real;
in
  fun sqrt(x : real) = call(x)
end;
(* Student's t distribution. stdtr(k : int, t : real) : real *)
local
  val call = _import "stdtr" public: int * real -> real;
in
  fun stdtr(k : int, t : real) = call(k, t)
end;
(* Functional inverse of Student's t distribution. stdtri(k : int, p : real) : real *)
local
  val call = _import "stdtri" public: int * real -> real;
in
  fun stdtri(k : int, p : real) = call(k, p)
end;
(* Struve function. struve(v : int, x : real) : real *)
local
  val call = _import "struve" public: int * real -> real;
in
  fun struve(v : int, x : real) = call(v, x)
end;
(* Circular tangent. tan(x : real) : real *)
local
  val call = _import "md_tan" public: real -> real;
in
  fun tan(x : real) = call(x)
end;
(* Circular cotangent. cot(x : real) : real *)
local
  val call = _import "cot" public: real -> real;
in
  fun cot(x : real) = call(x)
end;
(* Circular tangent of argument in degrees. tandg(x : real) : real *)
local
  val call = _import "tandg" public: real -> real;
in
  fun tandg(x : real) = call(x)
end;
(* Circular cotangent of argument in degrees. cotdg(x : real) : real *)
local
  val call = _import "cotdg" public: real -> real;
in
  fun cotdg(x : real) = call(x)
end;
(* Hyperbolic tangent. tanh(x : real) : real *)
local
  val call = _import "md_tanh" public: real -> real;
in
  fun tanh(x : real) = call(x)
end;
(* Bessel function of second kind of integer order. yn(n : int, x : real) : real *)
local
  val call = _import "yn" public: int * real -> real;
in
  fun yn(n : int, x : real) = call(n, x)
end;
(* Riemann zetac function. zetac(x : real) : real *)
local
  val call = _import "zetac" public: real -> real;
in
  fun zetac(x : real) = call(x)
end;

end;
