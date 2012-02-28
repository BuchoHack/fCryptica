package fCryptica.math

import scala.collection.mutable._
import scala.util.Random

object Matrix {
	type F[T] = (T, T) => T
	type R[T] = ArrayBuffer[T]
	type M[T] = ArrayBuffer[R[T]]

	def apply[T: Numeric](l: VektorNum[T]*): MatrixNum[T] = {
		val mat = ArrayBuffer[ArrayBuffer[T]]()
		for(i <- 0 to l.size - 1) {
			val list = ArrayBuffer[T]()
			for(j <- 0 to l(i).size - 1) {
				list += l(i)(j)
			}
			mat += list
		}
		return new MatrixNum[T](mat.transpose)
	}
	def apply(l: VektorPoly*): MatrixPoly = {
		val mat = ArrayBuffer[ArrayBuffer[Polynomial]]()
		for(i <- 0 to l.size - 1) {
			val list = ArrayBuffer[Polynomial]()
			for(j <- 0 to l(i).size - 1) {
				list += l(i)(j)
			}
			mat += list
		}
		return new MatrixPoly(mat.transpose)
	}

	// n次単位行列
	def identity(n: Int): MatrixNum[BigInt] = {
		val mat = ArrayBuffer[ArrayBuffer[BigInt]]()
		for(i <- 0 to n - 1) {
			val list = ArrayBuffer[BigInt]()
			for(j <- 0 to n - 1) {
				list += (if(i==j) 1 else 0)
			}
			mat += list
		}
		return new MatrixNum[BigInt](mat)
	}

	// ランダム冪零行列の生成
	def probableNilpotent(n: Int, modular: BigInt, rnd: Random): MatrixNum[BigInt] = {
		val mat = ArrayBuffer[ArrayBuffer[BigInt]]()
		for(i <- 0 to n - 1) {
			val list = ArrayBuffer[BigInt]()
			for(j <- 0 to n - 1) {
				list += (if(i<j) BigInt(modular.bitLength, rnd).mod(modular) else 0)
			}
			mat += list
		}
		return new MatrixNum[BigInt](mat)
	}

	// ランダム正則行列の生成
	def probableRegular(n: Int, modular: Int, rnd: Random) = probableNonSingular(n, modular, rnd)
	def probableInvertible(n: Int, modular: Int, rnd: Random) = probableNonSingular(n, modular, rnd)
	def probableNonSingular(n: Int, modular: Int, rnd: Random): MatrixNum[BigInt] = {
		return identity(n) + probableNilpotent(n: Int, modular: BigInt, rnd: Random)
	}
}
