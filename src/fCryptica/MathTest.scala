package fCryptica

import fCryptica.math._
import fCryptica.util._
import scala.tools.nsc._
import scala.util.Random
import interpreter.AbstractFileClassLoader

object MathTest {
	def main(args: Array[String]) = {
		val a = BigInt(0)
		val vec0 = Vektor(a+1, a+2)
		val vec1 = Vektor(a+1, a+1)
		println("vec0 * vec1 = " + (vec0 * vec1))

		val mat = Matrix(vec0, vec1)
		println("mat = \n" + mat)
		println("mat.determinant = " + mat.determinant)

		val pol1 = Polynomial("x*y + x")
		val pol2 = Polynomial("10*x*y")

		val pol = Polynomial("x+x^2+y+1")
		val po1 = pol.set('x,2).set('y,2)
		println(po1)

		val vecpoly1 = Vektor(pol1,pol1,pol1,pol1)
		val vecpoly2 = Vektor(pol2,pol2,pol2,pol2)
		println("vecpoly1 + vecpoly2" + (vecpoly1 - vecpoly1))

		println("pol1 = " + pol1)
		println("pol2 = " + pol2)
		println("pol1 + pol2 = " + (pol1 + pol2))
		println("pol1 * pol2 = " + (pol1 * pol2))
		println(vecpoly1.toPolynomial('X))

		val rat = Rational(1, 2)
		println("rat = " + rat)

		val com = Complex(1, 2)
		println("com = " + com)

		val p1 = Polynomial("2*x^2 + 3*x*y + 4")
		val p2 = Polynomial("x + y")
		Polynomial.init('x)
		println(p1/p2)

val compa = Complex(1,2)
val compb = Complex(3,5)
println(compa)
println(compb)
println(compa.gcd(compb))
val compc = compa.modInverse(compb)
println(compc)
println(compa * compc % compb)
	}
}
