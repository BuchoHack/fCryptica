package fCryptica.math

import scala.math.BigInt;
import Numeric.Implicits._

object Complex {
	def apply[T:Numeric](re: T, im: T): Complex[T] = new Complex(re, im)
}

class Complex[T: Numeric](t: Tuple2[T, T]) {
	val re = t._1
	val im = t._2

	// 負数
	def unary_- : Complex[T] = Complex(-re, -im)

	// 加算
	def + (that: Complex[T]): Complex[T] = Complex(this.re + that.re, this.im + that.im)

	// 減算
	def - (that: Complex[T]): Complex[T] = Complex(this.re - that.re, this.im - that.im)

	// 乗算
	def * (that: Complex[T]): Complex[T] =
		Complex(this.re * that.re - this.im * that.im, this.re * that.im + this.im * that.re)

	// 除算
	def / (that: Complex[T])(implicit num: Integral[T]): Complex[T] = {
		import num._
		Complex((this.re * that.re + this.im * that.im) / that.norm(), (this.im * that.re - this.re * that.im) / that.norm())
	}

	// 剰余
	def mod (that: Complex[T])(implicit num: Integral[T]): Complex[T] = this % that
	def % (that: Complex[T])(implicit num: Integral[T]): Complex[T] = this - (this / that) * that

	// ノルム
	def norm (): T = (this.re * this.re) + (this.im * this.im)

	// 最大公約数
	def gcd (that: Complex[T])(implicit num: Integral[T]): T = {
		import num._
		def run(a: T, b: T): T = {
			if(b==0) return a
			else return run(b, a % b)
		}
		run(this.norm(), that.norm())
	}

    // 法nにおける乗法逆元
	def modInverse(n: Complex[T])(implicit num: Integral[T]): Complex[T] = {
		import num._
		var a = this
		var i = n
		var v = Complex(zero, zero)
		var d = Complex(one, one)
		while(a.norm > zero) {
			val t = i / a
			var x = a
			a = i % x
			i = x
			x = d
			d = v - t * x
			v = x
		}
		v = v % n
		if(v.re < zero) v = (v + n) % n
		return v
	}

	// 標準出力
	override def toString() = "" + re + " + " + im + "i"
}
