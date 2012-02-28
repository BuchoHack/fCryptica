package fCryptica.math

import scala.collection.mutable._
import Numeric.Implicits._

class VektorNum[T: Numeric](v: Vektor.R[T]) {
	def this() = this(ArrayBuffer[T]())
	val elms = v
	val order = this.elms.size

	// スカラーとスカラーの演算用
	private def checkOrder(that: VektorNum[T]): Unit =
		if (this.order != that.order) throw new IllegalArgumentException("operands must have same order")
	private def rop(that: VektorNum[T])(op: Vektor.F[T]): VektorNum[T] =
		new VektorNum[T]((this.elms zip that.elms) map (t => op(t._1, t._2)))

	// スカラーと多項式の演算用
	private def checkOrder(that: VektorPoly): Unit =
		if (this.order != that.order) throw new IllegalArgumentException("operands must have same order")
	private def rop(that: VektorPoly)(op: Vektor.F[Polynomial]): VektorPoly =
		new VektorPoly((this.elms zip that.elms) map (t => op(BigInt(t._1.toString()), t._2)))

	/* 演算 */
	// 加算
	def + (that: VektorNum[T]): VektorNum[T] = {
		this.checkOrder(that)
		this.rop(that)(_ + _)
	}
	// 減算
	def - (that: VektorNum[T]): VektorNum[T] = {
		checkOrder(that)
		this.rop(that)(_ - _)
	}
	// スカラー乗算
	def * (that: T): VektorNum[T] = new VektorNum[T](this.elms map (r => r * that))
	// スカラー除算
	def / (that: T)(implicit num: Integral[T]): VektorNum[T] = {
		import num._
		new VektorNum[T](this.elms map (r => r / that))
	}
	// スカラー剰余
	def % (that: T)(implicit num: Integral[T]): VektorNum[T] = {
		import num._
		new VektorNum[T](this.elms map (r => r % that))
	}
	// 内積
	def * (that: VektorNum[T]): T = {
		checkOrder(that)
		this.rop(that)(_ * _).elms.reduceLeft(_ + _)
	}
	// 冪乗
	def ^ (that: Int): VektorNum[T] = {
		var vec = Vektor.one(this.size)
		for(i <- 0 to that - 1) vec = vec.rop(this)(_ * _)
		return vec
	}

	// ベクトルの多項式表現
	def toPolynomial(x: Symbol): VektorPoly = {
		val list = ArrayBuffer[Polynomial]()
		for(i <- 0 to this.size - 1){
			list += BigInt(this(i).toString()) * Polynomial(Term(Pair(x,this.size-i-1)))
		}
		return new VektorPoly(list)
	}

	/* 要素の有無 */
	def isEmpty (): Boolean = elms.isEmpty

	/* 要素数の取得 */
	def size (): Int = elms.size

	/* 条件pを満たす要素数の取得 */
	def count (p: T => Boolean): Int = elms.count(p)

	/* 要素を先頭に追加 */
	def addHead (that: T): Unit = that +=: elms
	/* 要素を末尾に追加 */
	def addLast (that: T): Unit = elms += that
	/* 要素の取得 */
	def apply (that: Int): T = elms.apply(that)
	/* 要素の変更 */
	def update (i: Int, that: T) = elms.update(i, that)

	/* 部分ベクトルの取得 */
	// 末尾を除いた残りの部分ベクトルを返す
	def init (): VektorNum[T] = new VektorNum[T](this.elms.init)
	// 先頭を除いた残りの部分ベクトルを返す
	def tail (): VektorNum[T] = new VektorNum[T](this.elms.tail)
	// 先頭n個の部分ベクトルを返す
	def take (n: Int): VektorNum[T] = new VektorNum[T](this.elms.take(n))
	// 先頭n個を除いた部分ベクトルを返す
	def drop (n: Int): VektorNum[T] = new VektorNum[T](this.elms.drop(n))
	// 末尾n個の部分ベクトルを返す
	def takeRight (n: Int): VektorNum[T] = new VektorNum[T](this.elms.takeRight(n))
	// 末尾n個を除いた部分ベクトルを返す
	def dropRight (n: Int): VektorNum[T] = new VektorNum[T](this.elms.dropRight(n))
	// 添字がfrom以上until以下の要素の部分ベクトルを返す
	def slice (from: Int, until: Int): VektorNum[T] = new VektorNum[T](this.elms.slice(from, until+1))

	// ベクトルの指定サイズ分割
	def split (size: Int): Array[VektorNum[T]] = {
		val n = if(this.size % size == 0) this.size / size else this.size / size + 1
		val array = new Array[VektorNum[T]](n)
		for(i <- 0 to n - 1) {
			array(i) = this.slice(i * size, i * size + size)
		}
		return array
	}

	/* 最大値 */
	def max: T = this.elms max
	/* 最小値 */
	def min: T = this.elms min

	def sum: T = this.elms.sum

	override def toString() = this.elms.mkString("[", ",", "]")
}
