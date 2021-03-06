signature CEPHES = sig
type cmplx = real * real;
(* 2**-53 *)
val MACHEP : real;
(* log(2**1024) *)
val MAXLOG : real;
(* log(2**-1022) *)
val MINLOG : real;
(* 2**1024 *)
val MAXNUM : real;
(* pi *)
val PI : real;
(* pi/2 *)
val PIO2 : real;
(* pi/4 *)
val PIO4 : real;
(* sqrt(2) *)
val SQRT2 : real;
(* sqrt(2)/2 *)
val SQRTH : real;
(* 1/log(2) *)
val LOG2E : real;
(* sqrt(2/pi) *)
val SQ2OPI : real;
(* log(2) *)
val LOGE2 : real;
(* log(2)/2 *)
val LOGSQ2 : real;
(* 2*pi/4 *)
val THPIO4 : real;
(* 2/pi *)
val TWOOPI : real;
(* Inverse hyperbolic cosine. acosh(x : real) : real *)
val acosh : real -> real
(* Airy function. airy(x : real) : (int, real, real, real, real) *)
val airy : real -> int * real * real * real * real
(* Inverse circular sine. asin(x : real) : real *)
val asin : real -> real
(* Inverse circular cosine. acos(x : real) : real *)
val acos : real -> real
(* Inverse hyperbolic sine. asinh(x : real) : real *)
val asinh : real -> real
(* Inverse circular tangent (arctangent). atan(x : real) : real *)
val atan : real -> real
(* Quadrant correct inverse circular tangent. atan2(y : real, x : real) : real *)
val atan2 : real * real -> real
(* Inverse hyperbolic tangent. atanh(x : real) : real *)
val atanh : real -> real
(* Binomial distribution. bdtr(k : int, n : int, p : real) : real *)
val bdtr : int * int * real -> real
(* Complemented binomial distribution. bdtrc(k : int, n : int, p : real) : real *)
val bdtrc : int * int * real -> real
(* Inverse binomial distribution. bdtri(k : int, n : int, y : real) : real *)
val bdtri : int * int * real -> real
(* Beta function. beta(a : real, b : real) : real *)
val beta : real * real -> real
(* Beta distribution. btdtr(a : real, b : real, x : real) : real *)
val btdtr : real * real * real -> real
(* Cube root. cbrt(x : real) : real *)
val cbrt : real -> real
(* Evaluate Chebyshev series. chbevl(x : real, coef : real Vector.vector) : real *)
val chbevl : real * real Vector.vector -> real
(* Chi-square distribution. chdtr(df : real, x : real) : real *)
val chdtr : real * real -> real
(* Complemented Chi-square distribution. chdtrc(df : real, x : real) : real *)
val chdtrc : real * real -> real
(* Inverse of complemented Chi-square distribution. chdtri(df : real, x : real) : real *)
val chdtri : real * real -> real
(* Complex natural logarithm. clog(x : cmplx) : cmplx *)
val clog : cmplx -> cmplx
(* Complex exponential function. cexp(x : cmplx) : cmplx *)
val cexp : cmplx -> cmplx
(* Complex circular sine. csin(x : cmplx) : cmplx *)
val csin : cmplx -> cmplx
(* Complex circular cosine. ccos(x : cmplx) : cmplx *)
val ccos : cmplx -> cmplx
(* Complex circular tangent. ctan(x : cmplx) : cmplx *)
val ctan : cmplx -> cmplx
(* Complex circular cotangent. ccot(x : cmplx) : cmplx *)
val ccot : cmplx -> cmplx
(* Complex circular arc sine. casin(x : cmplx) : cmplx *)
val casin : cmplx -> cmplx
(* Complex circular arc cosine. cacos(x : cmplx) : cmplx *)
val cacos : cmplx -> cmplx
(* Complex circular arc tangent. catan(x : cmplx) : cmplx *)
val catan : cmplx -> cmplx
(* Complex hyperbolic sine. csinh(x : cmplx) : cmplx *)
val csinh : cmplx -> cmplx
(* Complex hyperbolic cosine. ccosh(x : cmplx) : cmplx *)
val ccosh : cmplx -> cmplx
(* Complex inverse hyperbolic cosine. cacosh(x : cmplx) : cmplx *)
val cacosh : cmplx -> cmplx
(* Complex hyperbolic tangent. ctanh(x : cmplx) : cmplx *)
val ctanh : cmplx -> cmplx
(* Complex inverse hyperbolic tangent. catanh(x : cmplx) : cmplx *)
val catanh : cmplx -> cmplx
(* Complex power function. cpow(x : cmplx, y : cmplx) : cmplx *)
val cpow : cmplx * cmplx -> cmplx
(* Complex addition. cadd(x : cmplx, y : cmplx) : cmplx *)
val cadd : cmplx * cmplx -> cmplx
(* Complex subtraction. csub(x : cmplx, y : cmplx) : cmplx *)
val csub : cmplx * cmplx -> cmplx
(* Complex multiplication. cmul(x : cmplx, y : cmplx) : cmplx *)
val cmul : cmplx * cmplx -> cmplx
(* Complex division. cdiv(x : cmplx, y : cmplx) : cmplx *)
val cdiv : cmplx * cmplx -> cmplx
(* Complex absolute value. cabs(x : cmplx) : cmplx *)
val cabs : cmplx -> cmplx
(* Complex square root. csqrt(x : cmplx) : cmplx *)
val csqrt : cmplx -> cmplx
(* Hyperbolic cosine. cosh(x : real) : real *)
val cosh : real -> real
(* Dawson's integral. dawsn(x : real) : real *)
val dawsn : real -> real
(* Pseudorandom number generator. drand() : real *)
val drand : unit -> real
(* Exponential integral. ei(x : real) : real *)
val ei : real -> real
(* Incomplete elliptic integral of the second kind. ellie(phi : real, m : real) : real *)
val ellie : real * real -> real
(* Incomplete elliptic integral of the first kind. ellik(phi : real, m : real) : real *)
val ellik : real * real -> real
(* Complete elliptic integral of the second kind. ellpe(m1 : real) : real *)
val ellpe : real -> real
(* Jacobian Elliptic Functions. ellpj(u : real, m : real) : (int, real, real, real, real) *)
val ellpj : real * real -> int * real * real * real * real
(* Complete elliptic integral of the first kind. ellpk(m1 : real) : real *)
val ellpk : real -> real
(* Exponential function. exp(x : real) : real *)
val exp : real -> real
(* Base 10 exponential function. exp10(x : real) : real *)
val exp10 : real -> real
(* Base 2 exponential function. exp2(x : real) : real *)
val exp2 : real -> real
(* Exponential integral En. expn(n : int, x : real) : real *)
val expn : int * real -> real
(* Exponential of squared argument. expx2(x : real, sign : int) : real *)
val expx2 : real * int -> real
(* Absolute value. fabs(x : real) : real *)
val fabs : real -> real
(* Factorial function. fac(i : int) : real *)
val fac : int -> real
(* F distribution. fdtr(df1 : int, df2 : int, x : real) : real *)
val fdtr : int * int * real -> real
(* Complemented F distribution. fdtrc(df1 : int, df2 : int, x : real) : real *)
val fdtrc : int * int * real -> real
(* Inverse of complemented F distribution. fdtri(df1 : int, df2 : int, p : real) : real *)
val fdtri : int * int * real -> real
(* Floor function. floor(x : real) : real *)
val floor : real -> real
(* Ceiling function. ceil(x : real) : real *)
val ceil : real -> real
(* frexp. frexp(x : real, expnt : int) : (real, int) *)
val frexp : real * int -> real * int
(* ldexp. ldexp(x : real, n : int) : real *)
val ldexp : real * int -> real
(* Fresnel integral. fresnl(x : real) : (real, real) *)
val fresnl : real -> real * real
(* Gamma function. gamma(x : real) : real *)
val gamma : real -> real
(* Natural logarithm of gamma function. lgam(x : real) : real *)
val lgam : real -> real
(* Gamma distribution function. gdtr(a : real, b : real, x : real) : real *)
val gdtr : real * real * real -> real
(* Complemented gamma distribution function. gdtrc(a : real, b : real, x : real) : real *)
val gdtrc : real * real * real -> real
(* Gauss hypergeometric function _2F_1. hyp2f1(a : real, b : real, c : real, x : real) : real *)
val hyp2f1 : real * real * real * real -> real
(* Confluent hypergeometric function. hyperg(a : real, b : real, x : real) : real *)
val hyperg : real * real * real -> real
(* Modified Bessel function of order zero. i0(x : real) : real *)
val i0 : real -> real
(* Modified Bessel function of order zero, exponentially scaled. i0e(x : real) : real *)
val i0e : real -> real
(* Modified Bessel function of order one. i1(x : real) : real *)
val i1 : real -> real
(* Modified Bessel function of order one, exponentially scaled. i1e(x : real) : real *)
val i1e : real -> real
(* Incomplete gamma integral. igam(a : real, x : real) : real *)
val igam : real * real -> real
(* Complemented incomplete gamma integral. igamc(a : real, x : real) : real *)
val igamc : real * real -> real
(* Inverse of complemented incomplete gamma integral. igami(a : real, p : real) : real *)
val igami : real * real -> real
(* Incomplete beta integral. incbet(a : real, b : real, x : real) : real *)
val incbet : real * real * real -> real
(* Inverse of incomplete beta integral. incbi(a : real, b : real, y : real) : real *)
val incbi : real * real * real -> real
(* signbit. signbit(x : real) : int *)
val signbit : real -> int
(* isnan. isnan(x : real) : int *)
val isnan : real -> int
(* isfinite. isfinite(x : real) : int *)
val isfinite : real -> int
(* Modified Bessel function of noninteger order. iv(v : real, x : real) : real *)
val iv : real * real -> real
(* Bessel function of order zero. j0(x : real) : real *)
val j0 : real -> real
(* Bessel function of second kind, order zero. y0(x : real) : real *)
val y0 : real -> real
(* Bessel function of order one. j1(x : real) : real *)
val j1 : real -> real
(* Bessel function of second kind of order one. y1(x : real) : real *)
val y1 : real -> real
(* Bessel function of integer order. jn(n : int, x : real) : real *)
val jn : int * real -> real
(* Bessel function of noninteger order. jv(v : real, x : real) : real *)
val jv : real * real -> real
(* Modified Bessel function, third kind, order zero. k0(x : real) : real *)
val k0 : real -> real
(* Modified Bessel function, third kind, order zero, exponentially scaled. k0e(x : real) : real *)
val k0e : real -> real
(* Modified Bessel function, third kind, order one. k1(x : real) : real *)
val k1 : real -> real
(* Modified Bessel function, third kind, order one, exponentially scaled. k1e(x : real) : real *)
val k1e : real -> real
(* Modified Bessel function, third kind, integer order. kn(n : int, x : real) : real *)
val kn : int * real -> real
(* Natural logarithm. log(x : real) : real *)
val log : real -> real
(* Common logarithm. log10(x : real) : real *)
val log10 : real -> real
(* Base 2 logarithm. log2(x : real) : real *)
val log2 : real -> real
(* Negative binomial distribution. nbdtr(k : int, n : int, p : real) : real *)
val nbdtr : int * int * real -> real
(* Complemented negative binomial distribution. nbdtrc(k : int, n : int, p : real) : real *)
val nbdtrc : int * int * real -> real
(* Functional inverse of negative binomial distribution. nbdtri(k : int, n : int, y : real) : real *)
val nbdtri : int * int * real -> real
(* Normal distribution function. ndtr(x : real) : real *)
val ndtr : real -> real
(* Error function. erf(x : real) : real *)
val erf : real -> real
(* Complementary error function. erfc(x : real) : real *)
val erfc : real -> real
(* Inverse of Normal distribution function. ndtri(x : real) : real *)
val ndtri : real -> real
(* Poisson distribution function. pdtr(k : int, m : real) : real *)
val pdtr : int * real -> real
(* Complemented Poisson distribution. pdtrc(k : int, m : real) : real *)
val pdtrc : int * real -> real
(* Inverse Poisson distribution. pdtri(k : int, y : real) : real *)
val pdtri : int * real -> real
(* Evaluate polynomial. polevl(x : real, coef : real Vector.vector) : real *)
val polevl : real * real Vector.vector -> real
(* Power function. pow(x : real, y : real) : real *)
val pow : real * real -> real
(* Real raised to integer power. powi(x : real, n : int) : real *)
val powi : real * int -> real
(* Psi (digamma) function. psi(x : real) : real *)
val psi : real -> real
(* Reciprocal gamma function. rgamma(x : real) : real *)
val rgamma : real -> real
(* Round double to nearest or even integer valued double. round(x : real) : real *)
val round : real -> real
(* Hyperbolic sine and cosine integrals. shichi(x : real) : (real, real, real) *)
val shichi : real -> real * real * real
(* Sine and cosine integrals. sici(x : real) : (real, real, real) *)
val sici : real -> real * real * real
(* Circular sine. sin(x : real) : real *)
val sin : real -> real
(* Circular cosine. cos(x : real) : real *)
val cos : real -> real
(* Circular sine of angle in degrees. sindg(x : real) : real *)
val sindg : real -> real
(* Circular cosine of angle in degrees. cosdg(x : real) : real *)
val cosdg : real -> real
(* Hyperbolic sine. sinh(x : real) : real *)
val sinh : real -> real
(* Dilogarithm. spence(x : real) : real *)
val spence : real -> real
(* Square root. sqrt(x : real) : real *)
val sqrt : real -> real
(* Student's t distribution. stdtr(k : int, t : real) : real *)
val stdtr : int * real -> real
(* Functional inverse of Student's t distribution. stdtri(k : int, p : real) : real *)
val stdtri : int * real -> real
(* Struve function. struve(v : int, x : real) : real *)
val struve : int * real -> real
(* Circular tangent. tan(x : real) : real *)
val tan : real -> real
(* Circular cotangent. cot(x : real) : real *)
val cot : real -> real
(* Circular tangent of argument in degrees. tandg(x : real) : real *)
val tandg : real -> real
(* Circular cotangent of argument in degrees. cotdg(x : real) : real *)
val cotdg : real -> real
(* Hyperbolic tangent. tanh(x : real) : real *)
val tanh : real -> real
(* Bessel function of second kind of integer order. yn(n : int, x : real) : real *)
val yn : int * real -> real
(* Riemann zetac function. zetac(x : real) : real *)
val zetac : real -> real

end;
