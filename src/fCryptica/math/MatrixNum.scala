package fCryptica.math

import Numeric.Implicits._

class MatrixNum[T: Numeric](m: Matrix.M[T]) {
	val elms = m

	// スカラーとスカラーの演算用
	private def rop(rthis: Matrix.R[T], rthat: Matrix.R[T])(op: Matrix.F[T]): Matrix.R[T] =
		(rthis zip rthat) map (t => op(t._1, t._2))
	private def mop(that: MatrixNum[T])(op: Matrix.F[T]): MatrixNum[T] =
		new MatrixNum((this.elms zip that.elms) map (t => rop(t._1, t._2)(op(_, _))))
	private def dotProduct(a: Matrix.R[T], b: Matrix.R[T]): T =
		(rop(a, b)(_ * _)).reduceLeft(_ + _)

	// スカラーと多項式の演算用
	private def rop[T: Numeric](rthis: Matrix.R[T], rthat: Matrix.R[Polynomial])(op: Matrix.F[Polynomial]): Matrix.R[Polynomial] =
		(rthis zip rthat) map (t => op(BigInt(t._1.toString()), t._2))
	private def mop[T: Numeric](that: MatrixPoly)(op: Matrix.F[Polynomial]): MatrixPoly =
		new MatrixPoly((this.elms zip that.elms) map (t => rop(t._1, t._2)(op(_, _))))
	private def dotProduct[T: Numeric](a: Matrix.R[T], b: Matrix.R[Polynomial]): Polynomial =
		(rop(a, b)(_ * _)).reduceLeft(_ + _)

	def getLineSize(): Int = this.elms(0).size
	def getColumnSize(): Int = this.elms.size

	/* 加算 */
	def + (that: MatrixNum[T]): MatrixNum[T] = this.mop(that)(_ + _)

	/* 減算 */
	def - (that: MatrixNum[T]): MatrixNum[T] = this.mop(that)(_ - _)

	/* 乗算 */
	// スカラー行列 * スカラー行列
	def * (that: MatrixNum[T]): MatrixNum[T] = new MatrixNum((for (rthis <- this.elms) yield (for (cthat <- that.elms.transpose) yield dotProduct(rthis, cthat))))
	// スカラー行列 * 多項式行列
	def * (that: MatrixPoly): MatrixPoly = (that.transpose * this.transpose).transpose
	// スカラー行列 * 多項式ベクトル
	def * (that: VektorPoly): VektorPoly = new VektorPoly(for (rthis <- this.elms) yield dotProduct(rthis, that.elms))

	/* スカラー乗算 */
	def * (that: T): MatrixNum[T] = new MatrixNum(this.elms map (m => m.map(r => r * that)))
	/* スカラー除算 */
	def / (that: T)(implicit num: Integral[T]): MatrixNum[T] = {
		import num._
		new MatrixNum(this.elms map (m => m.map(r => r / that)))
	}
	/* スカラー剰余 */
	def % (that: T)(implicit num: Integral[T]): MatrixNum[T] = {
		import num._
		new MatrixNum(this.elms map (m => m.map(r => r % that)))
	}

	/* modular */
	def mod (n: T)(implicit num: Integral[T]): MatrixNum[T] = {
		import num._
		new MatrixNum[T](this.elms map (t => t map (t => t % n)))
	}

	/* 転置行列 */
	def transpose = new MatrixNum[T](this.elms.transpose)

	/* 行列式 */
	def determinant(implicit num: Integral[T]): T = this.elms.size match {
		case 1 => this.elms.head.head
		case _ => {
			import num._
			val line = getLineSize
			val column = getColumnSize
			var array = new Array[Rational[T]](line * column)
			for (i <- 0 to line - 1)
				for (j <- 0 to column - 1)
					array(i * line + j) = new Rational((elms.apply(i).apply(j), one))
			var value = new Rational(one, one)
			for (i <- 0 to line - 1) {
				var pivot = array(i * line + i)
				value = value * pivot
				for (j <- 0 to line - 1) array(i * line + j) = array(i * line + j) / pivot
				for (k <- i + 1 to line - 1) {
					var erase = array(k * line + i)
					for (j <- i to line - 1) array(k * line + j) = array(k * line + j) - erase * array(i * line + j)
				}
			}
			value.getApproximation
		}
	}

	def col(n: Int): VektorNum[T] = null
	def row(n: Int): VektorNum[T] = null

	/* 最大値 */
	def max: T = this.elms map (m => m max) max
	/* 最小値 */
	def min: T = this.elms map (m => m min) min

	override def toString() = (this.elms map (_.mkString("[", ",", "]"))).mkString("", "\n", "\n")
}
