package fCryptica.math

import scala.collection.mutable._
import scala.math.BigInt;
import scala.math.Numeric;
import Numeric.Implicits._

case class VektorPoly(v: Vektor.R[Polynomial]) {
	def this() = this(ArrayBuffer[Polynomial]())
	var elms = v
	var order = this.elms.size

	// 多項式と多項式の演算用
	private def checkOrder(that: VektorPoly): Unit =
		if (this.order != that.order) throw new IllegalArgumentException("operands must have same order")
	private def rop(that: VektorPoly)(op: Vektor.F[Polynomial]): VektorPoly =
		new VektorPoly((this.elms zip that.elms) map (t => op(t._1, t._2)))

	// 多項式とスカラーの演算用
	private def checkOrder[T: Numeric](that: VektorNum[T]): Unit =
		if (this.order != that.order) throw new IllegalArgumentException("operands must have same order")
	private def rop[T: Numeric](that: VektorNum[T])(op: Vektor.F[Polynomial]): VektorPoly =
		new VektorPoly((this.elms zip that.elms) map (t => op(t._1, BigInt(t._2.toString()))))

	/* 演算 */
	// 加算
	def + (that: VektorPoly): VektorPoly = {
		this.checkOrder(that)
		this.rop(that)(_ + _)
	}
	// 減算
	def - (that: VektorPoly): VektorPoly = {
		checkOrder(that)
		this.rop(that)(_ - _)
	}
	// 内積
	def * (that: VektorPoly): Polynomial = {
		checkOrder(that)
		this.rop(that)(_ * _).elms.reduceLeft(_ + _)
	}
	// スカラー乗算
	def * (that: BigInt): VektorPoly = new VektorPoly(this.elms map (r => r * that))
	// 冪乗
	def ^ (that: Int): VektorPoly = new VektorPoly(this.elms map (r => r ^ that))
	// 除算
	def / (that: Polynomial): VektorPoly = new VektorPoly(this.elms map (r => r / that))
	// 剰余
	def % (that: Polynomial): VektorPoly = new VektorPoly(this.elms map (r => r % that))
	def mod (that: Polynomial): VektorPoly = new VektorPoly(this.elms map (r => r mod that))

	// 変数に値を割り当てる
	def set(sym: Symbol, c: BigInt): VektorPoly = {
		val list = ArrayBuffer[Polynomial]()
		for(i <- 0 to this.size - 1) {
			list += this(i).set(sym, c)
		}
		return new VektorPoly(list)
	}

	// ベクトルの多項式表現
	def toPolynomial(x: Symbol): VektorPoly = {
		val list = ArrayBuffer[Polynomial]()
		for(i <- 0 to this.size - 1){
			list += this(i) * Polynomial(Term(Pair(x,this.size-i-1)))
		}
		return new VektorPoly(list)
	}

	/* 要素の有無 */
	def isEmpty (): Boolean = elms.isEmpty

	/* 要素数の取得 */
	def size (): Int = elms.size

	/* 条件pを満たす要素数の取得 */
	def count (p: Polynomial => Boolean): Int = elms.count(p)

	/* 要素を先頭に追加 */
	def addHead (that: Polynomial): Unit = that +=: elms
	/* 要素を末尾に追加 */
	def addLast (that: Polynomial): Unit = elms += that
	/* 要素の取得 */
	def apply (that: Int): Polynomial = elms.apply(that)
	/* 要素の変更 */
	def update (i: Int, that: Polynomial) = elms.update(i, that)

	/* 部分ベクトルの取得 */
	// 末尾を除いた残りの部分ベクトルを返す
	def init (): VektorPoly = new VektorPoly(this.elms.init)
	// 先頭を除いた残りの部分ベクトルを返す
	def tail (): VektorPoly = new VektorPoly(this.elms.tail)
	// 先頭n個の部分ベクトルを返す
	def take (n: Int): VektorPoly = new VektorPoly(this.elms.take(n))
	// 先頭n個を除いた部分ベクトルを返す
	def drop (n: Int): VektorPoly = new VektorPoly(this.elms.drop(n))
	// 末尾n個の部分ベクトルを返す
	def takeRight (n: Int): VektorPoly = new VektorPoly(this.elms.takeRight(n))
	// 末尾n個を除いた部分ベクトルを返す
	def dropRight (n: Int): VektorPoly = new VektorPoly(this.elms.dropRight(n))
	// 添字がfrom以上until以下の要素の部分ベクトルを返す
	def slice (from: Int, until: Int): VektorPoly = new VektorPoly(this.elms.slice(from, until+1))

	// ベクトルの指定サイズ分割
	def split (size: Int): Array[VektorPoly] = {
		val n = if(this.size % size == 0) this.size / size else this.size / size + 1
		val array = new Array[VektorPoly](n)
		for(i <- 0 to n - 1) {
			array(i) = this.slice(i * size, i * size + size)
		}
		return array
	}

	override def toString() = (this.elms.mkString("[", ",", "]"))
}
