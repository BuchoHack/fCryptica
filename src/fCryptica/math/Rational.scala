package fCryptica.math

import scala.math.BigInt;
import Numeric.Implicits._

object Rational {
	def apply[T:Numeric](num: T, den: T): Rational[T] = new Rational(num, den)
}

class Rational[T: Numeric](t: Tuple2[T, T]) {
	val numer = t._1
	val denom = t._2

	/* 0判定 */
	def isZero(implicit num: Integral[T]): Boolean = {
		import num._
		numer == zero
	}

	/* 無限判定 */
	def isInfinity(implicit num: Integral[T]): Boolean = {
		import num._
		denom == zero
	}

	/* 約分 */
	def reduce(t: Tuple2[T, T])(implicit num: Integral[T]): Rational[T] = {
		import num._
		if (this.isZero || this.isInfinity) return this
		else {
			val g = gcd(t._1, t._2)
			if (g == 1) return this
			else return Rational(t._1 / g, t._2 / g)
		}
	}

	/* 近似 */
	def getApproximation(implicit num: Integral[T]): T = {
		import num._
		if (this.isZero || this.isInfinity) throw new IllegalArgumentException("denominator must not zero")
		else numer / denom
	}

	/* 逆数 */
	def invert: Rational[T] = Rational(denom, numer)

	/* 負数 */
	def unary_- : Rational[T] = Rational(-numer, denom)
	def negate: Rational[T] = -this

	/* 加算 */
	def + (that: Rational[T])(implicit num: Integral[T]): Rational[T] =
		reduce(this.numer * that.denom + this.denom * that.numer, this.denom * that.denom)
	/* 減算 */
	def - (that: Rational[T])(implicit num: Integral[T]): Rational[T] =
		reduce(this.numer * that.denom - this.denom * that.numer, this.denom * that.denom)
	/* 乗算 */
	def * (that: Rational[T])(implicit num: Integral[T]): Rational[T] = {
		import num._
		if (this.isZero || this.isInfinity) return this
		if (that.isZero || that.isInfinity) return that
		return this * that.numer / that.denom
	}
	def * (that: T)(implicit num: Integral[T]): Rational[T] = {
		import num._
		if (that == zero) return new Rational(zero, zero)
		if (isZero || isInfinity || that == one) return this
		var tmp = that
		var tmpNum = numer
		var tmpDen = denom
		if (tmp < zero && tmpNum < zero) {
			tmp = tmp.abs
			tmpNum = tmpNum.abs
		} else if (tmp < zero && tmpDen < zero) {
			tmp = tmp.abs
			tmpDen = tmpDen.abs
		}
		if (tmpDen % tmp == zero) tmpDen = tmpDen / tmp
		else tmpNum = tmpNum * tmp
		return new Rational(tmpNum, tmpDen)
	}
	/* 除算 */
	def / (that: Rational[T])(implicit num: Integral[T]): Rational[T] = {
		import num._
		this * that.invert
	}
	def / (that: T): Rational[T] = new Rational(numer, denom * that)

	private def gcd(m: T, n: T)(implicit num: Integral[T]): T = {
		import num._
		if (n == 0) m else gcd(n, m % n)
	}
	private def lcm(m: T, n: T)(implicit num: Integral[T]): T = {
		import num._
		m * n / gcd(m, n)
	}

	override def toString() = "" + numer + "/" + denom
}
