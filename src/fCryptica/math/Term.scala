package fCryptica.math

import Numeric.Implicits._

object Term {
	implicit def IntWrapper(c: Int) = Term(BigInt(c))
	implicit def BigIntWrapper(c: BigInt) = Term(c)
	def apply(c: BigInt, factors: Pair[Symbol, Int]*): Term = new Term(c, Map() ++ factors)
	def apply(factors: Pair[Symbol, Int]*): Term = new Term(1, Map() ++ factors)
}

case class Term(c: BigInt, factors: Map[Symbol, Int]) {
	def + (that: Term): Term = Term(c + that.c, factors)

	def unary_- : Term = {
		Term(-c, factors)
	}

	def * (that: Term): Term = {
		var fs = factors
		factors.keySet ++ that.factors.keySet foreach { name =>
			val e = factors.getOrElse(name, 0) + that.factors.getOrElse(name, 0)
			fs += name -> e
		}
		Term(c * that.c, fs)
	}

	def / (that: Term): Term = {
		var fs = factors
		factors.keySet ++ that.factors.keySet foreach { name =>
			val e = factors.getOrElse(name, 0) - that.factors.getOrElse(name, 0)
			fs += name -> e
		}
		Term(c / that.c, fs)
	}

	def mod (n: BigInt): Term = {
		Term(c.mod(n), factors)
	}

	// 指定した変数の次数を取得
	def getDegree(x: Symbol): Int ={
		return factors.getOrElse(x, 0)
	}

	// 変数に値を割り当てる
	def set(sym: Symbol, c: BigInt): Term = {
		if(factors.keySet(sym) == true) {
			var value = BigInt(1)
			for(i <- 0 to factors(sym) - 1) value *= c
			Term(this.c * value, factors - sym)
		}
		else this
	}

	// 変数に別の変数割り当てる
	def set(sym1: Symbol, sym2: Symbol): Term = {
		if(factors.keySet(sym1) == true) {
			factors.updated(sym2, factors(sym1))
			Term(this.c, factors.updated(sym2, factors(sym1)) - sym1)
		}
		else this
	}

	// 変数一覧の取得
	def getSymbol(): Set[Symbol] = {
		factors.keySet
	}

	def simplify: Term = c.toInt match {
		case 0 => BigInt(0)
		case _ => Term(c, factors.filter(_._2 != 0))
	}

	override def hashCode = c.toInt match {
		case 0 => 0.hashCode
		case _ => super.hashCode
	}

	override def equals(that: Any) = that match {
		case that: Term =>
			(c == 0 && that.c == 0) || (c == that.c && factors == that.factors)
		case _ =>
			false
	}

	override def toString = {
		implicit def pairWrapper(factor: Pair[Symbol, Int]) = new {
			def format = factor._2 match {
				case 1 => factor._1.toString.replace("'", "")
				case _ => factor._1.toString.replace("'", "") + "^" + factor._2
			}
		}
		factors.size match {
			case 0 => c.toString
			case _ => c.toInt match {
				case 1 => factors.map(_.format).mkString("・")
				case _ => "" + c + "・" + factors.map(_.format).mkString("・")
			}
		}
	}
}