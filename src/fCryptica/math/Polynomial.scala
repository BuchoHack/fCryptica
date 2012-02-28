package fCryptica.math

import scala.util.Random

object Polynomial {
	val p = new PolynomialParser{}
	import p._

	private var dictionaly = Symbol("")
	def init(sym: Symbol) = dictionaly = sym

	implicit def intWrapper(c: Int) = Polynomial(BigInt(c))
	implicit def BigintWrapper(c: BigInt) = Polynomial(c)
	def apply(c: BigInt) = new Polynomial(Set(Term(c)))
	def apply(factor: Pair[Symbol, Int]) = new Polynomial(Set(Term(factor)))
	def apply(terms: Term*) = new Polynomial(Set() ++ terms)
	def apply(str: String) = parseAll(sum, str).get

	// 係数に変数を持つランダム多項式の生成
	def apply(n: Int, x: Set[Symbol], rnd: Random): Polynomial = {
		var ts = Set[Term]()
		for(i <- 0 to n - 1) {
			val fs = x.foreach(s => Pair(s, i) )
			Term
//			ts +=
		}
//		new Polynomial()
		Polynomial(ts)
	}
}

case class Polynomial(terms: Set[Term]) {
	assert(terms.size > 0)

//	private var dictionaly = Symbol("")

	def unary_- : Polynomial = {
		var ts = Set[Term]()
		for(i <- terms) ts += -i
		Polynomial(ts)
	}

	/* 演算 */
	// 加算
	def + (that: Polynomial): Polynomial = {
		var px = this
		that.terms.foreach { term => px = px + term }
		px.simplify
	}
	// 減算
	def - (that: Polynomial): Polynomial = {
		var px = this
		(-that).terms.foreach { term => px = px + term }
		px.simplify
	}
	// 乗算
	def * (that: Polynomial): Polynomial = terms.size match {
		case 0 => BigInt(0)
		case _ => {
			var ts = Set[Term]()
			for (a <- terms; b <- that.terms)
				ts += a * b
			Polynomial(ts)
		}
	}

	// 除算・剰余
	def / (that: Polynomial): Polynomial = getQR(that, Polynomial.dictionaly)._1
	def / (that: Polynomial, x: Symbol): Polynomial = getQR(that, x)._1
	def % (that: Polynomial): Polynomial = getQR(that, Polynomial.dictionaly)._2
	def % (that: Polynomial, x: Symbol): Polynomial = getQR(that, x)._2
	def mod (that: Polynomial): Polynomial = getQR(that, Polynomial.dictionaly)._2
	def mod (that: Polynomial, x: Symbol): Polynomial = getQR(that, x)._2
	private def getQR (that: Polynomial, x: Symbol): Pair[Polynomial, Polynomial] = terms.size match {
		case 0 => Pair(Polynomial(0), Polynomial(0))
		case _ => {
			var q = Polynomial(0)
			var r = this
			while(r.getDegree(x) >= that.getDegree(x)) {
				val ts = r.getCoef(x)
				val deg = r.getDegree(x) - that.getDegree(x)

				val t = if(deg >= 0) Polynomial(ts.head / that.getCoef(x).head) else Polynomial(0)
				r = r - t * that
				q = q + t
			}
			return Pair(q, r)
		}
	}
	// 冪乗展開
	def ^ (n: Int): Polynomial = terms.size match {
		case 0 => BigInt(0)
		case _ => {
			var px = Polynomial(1)
			for(i <- 0 to n - 1)
				px = px * this
			px.simplify
		}
	}
	// 係数の有限体mod
	def mod (n: BigInt): Polynomial = {
		var px = Polynomial(0)
		terms.foreach { term => px += term.mod(n) }
		px.simplify
	}

	// 指定した変数の最大次数を取得
	def getDegree(x: Symbol): Int ={
		var max = 0
		terms.foreach (term => if(max < term.getDegree(x)) max = term.getDegree(x))
		return max
	}
	// 指定した変数の最大次数の集合を取得
	def getCoef(x: Symbol): Set[Term] = {
		var ts = Set[Term]()
		terms.foreach (term => if(getDegree(x) != 0 && getDegree(x) == term.getDegree(x)) ts += term)
		return ts
	}

	// 変数に値を割り当てる
	def set(sym: Symbol, c: BigInt): Polynomial = {
		var px = Polynomial(0)
		terms.foreach { term => px += term.set(sym, c) }
		px.simplify
	}

	// 変数に別の変数割り当てる
	def set(sym1: Symbol, sym2: Symbol): Polynomial = {
		var px = Polynomial(0)
		terms.foreach { term => px += term.set(sym1, sym2) }
		px.simplify
	}

	// 変数一覧の取得
	def getSymbol(): Set[Symbol] = {
		var list = Set[Symbol]()
		terms.foreach { term => list = list ++ term.factors.keySet }
		list
	}

	def simplify: Polynomial = {
		val px = Polynomial(terms.map(_.simplify))
		val ts = px.terms.filter(_ != Term(0))
		ts.size match {
			case 0 => BigInt(0)
			case _ => Polynomial(ts.map(_.simplify))
		}
	}

	override def toString = terms.size match {
		case 0 => 0.toString
		case _ => terms.map(_.toString).mkString(" + ")
	}

	private def + (term: Term): Polynomial = {
		if (terms.filter(_.factors == term.factors).isEmpty)
			Polynomial(terms + term)
		else
			Polynomial(terms.map(t => if (t.factors == term.factors) t + term else t))
	}
}
