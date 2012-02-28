package fCryptica.crypto

import scala.util._
import fCryptica.security._
import fCryptica.math._

object MultivatePolynomial {
	def KeyGen(random: Random) = {
		// パラメタ
		val q = 2						// 体 Fq
		val n = 16						// 鍵の個数
		val t = 2						// t個分割
		val d = 3						// 適切な正整数
		val J = 4						// 分配値 (N > J)
		val GX = Polynomial("X^2+X+1")	// X^2+X+1 [既約]

		val A = Matrix.probableNonSingular(n, q, scala.util.Random)
		val x = Vektor('x, n)
		val y = A * x
		val N = n / t
		val Y = y.split(t)
		val Z = Y
		for(i <- 0 to Y.size - 1) {
			Polynomial.init('X)
			Z(i) = Y(i).toPolynomial('X)^d mod GX
		}
//		...
		val Γ = new Array[VektorPoly](Z.size)
		for(i <- 0 to J - 1) {
			Γ(i) = Z(i)
		}
		for(i <- J to N - 1) {
//			Γ(i) = Z(i) + Polynomial(t-1, y(1).getSymbol(), random)
		}
//		return null
	}

	def main(args: Array[String]) = {

		KeyGen(Random)

		println("test")
	}
}