fun main() =
    let
	val x = Rmath.qnorm(0.975,0.0,1.0,1,0);
	val _ = print(Real.toString(x) ^"\n")
	val (x,s) = Rmath.lgammafn_sign(3.0)
	val _ = print(Real.toString(x) ^"\n")
	val prob = Array.fromList [0.1,0.2,0.7]
	val y = Rmath.rmultinom(1000,prob)
	val _ = print(Int.toString(Array.sub(y,0)) ^"\n")
	val _ = print(Int.toString(Array.sub(y,1)) ^"\n")
	val _ = print(Int.toString(Array.sub(y,2)) ^"\n")
 	val x = Rmath.poisson_test(10.0,12.0,1.0, Rmath.TwoSided)
	val _ = print(Real.toString(x) ^"\n")
	val x = Rmath.poisson_test(10.0,8.0,1.0, Rmath.TwoSided)
	val _ = print(Real.toString(x) ^"\n")
    in
	()
    end
