package fCryptica.math

import scala.collection.mutable._

object Vektor {
	type F[T] = (T, T) => T
	type R[T] = ArrayBuffer[T]

	def apply[T: Numeric](l: T*): VektorNum[T] = {
		val list = ArrayBuffer[T]()
		for(i <- 0 to l.size - 1) list += l(i)
		return new VektorNum[T](list)
	}

	def apply(l: Polynomial*): VektorPoly = {
		val list = ArrayBuffer[Polynomial]()
		for(i <- 0 to l.size - 1) list += l(i)
		return new VektorPoly(list)
	}

	def apply(symbol: Symbol, n: Int): VektorPoly = {
		val list = ArrayBuffer[Polynomial]()
		for(i <- 1 to n) {
			list += Polynomial(Term(Pair(Symbol(symbol.name + i),1)))
		}
		return new VektorPoly(list)
	}

	def apply(poly: Polynomial, n: Int): VektorPoly = {
		val polySymbol = poly.getSymbol.head
		val list = ArrayBuffer[Polynomial]()
		for(i <- 1 to n) {
			list += poly.set(polySymbol, Symbol(polySymbol.name + i))
		}
		return new VektorPoly(list)
	}

	// 0ベクトル
	def zero[T](n: Int)(implicit num: Numeric[T]): VektorNum[T] = {
		val list = ArrayBuffer[T]()
		for(i <- 0 to n - 1) list += num.zero
		return new VektorNum[T](list)
	}
	// 1ベクトル
	def one[T](n: Int)(implicit num: Numeric[T]): VektorNum[T] = {
		val list = ArrayBuffer[T]()
		for(i <- 0 to n - 1) list += num.one
		return new VektorNum[T](list)
	}
}
