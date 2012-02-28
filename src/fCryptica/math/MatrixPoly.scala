package fCryptica.math

import scala.collection.mutable._
import scala.math.BigInt;
import scala.math.Numeric;
import Numeric.Implicits._

class MatrixPoly(m: Matrix.M[Polynomial]) {
	val elms = m

	// 多項式と多項式の演算用
	private def rop(rthis: Matrix.R[Polynomial], rthat: Matrix.R[Polynomial])(op: Matrix.F[Polynomial]): Matrix.R[Polynomial] =
		(rthis zip rthat) map (t => op(t._1, t._2))
	private def mop(that: MatrixPoly)(op: Matrix.F[Polynomial]): MatrixPoly =
		new MatrixPoly((this.elms zip that.elms) map (t => rop(t._1, t._2)(op(_, _))))
	private def dotProduct(a: Matrix.R[Polynomial], b: Matrix.R[Polynomial]): Polynomial =
		(rop(a, b)(_ * _)).reduceLeft(_ + _)

	// 多項式とスカラーの演算用
	private def rop[T: Numeric](rthis: Matrix.R[Polynomial], rthat: Matrix.R[T])(op: Matrix.F[Polynomial]): Matrix.R[Polynomial] =
		(rthis zip rthat) map (t => op(t._1, BigInt(t._2.toString())))
	private def mop[T: Numeric](that: MatrixNum[T])(op: Matrix.F[Polynomial]): MatrixPoly =
		new MatrixPoly((this.elms zip that.elms) map (t => rop(t._1, t._2)(op(_, _))))
	private def dotProduct[T: Numeric](a: Matrix.R[Polynomial], b: Matrix.R[T]): Polynomial =
		(rop(a, b)(_ * _)).reduceLeft(_ + _)

	def getLineSize(): Int = this.elms(0).size
	def getColumnSize(): Int = this.elms.size

	/* 加算 */
	def + (that: MatrixPoly): MatrixPoly = this.mop(that)(_ + _)
	def + [T: Numeric](that: MatrixNum[T]): MatrixPoly = this.mop(that)(_ + _)

	/* 減算 */
	def - (that: MatrixPoly): MatrixPoly = this.mop(that)(_ - _)
	def - [T: Numeric](that: MatrixNum[T]): MatrixPoly = this.mop(that)(_ - _)

	/* 乗算 */
	// 多項式行列 * 多項式行列
	def * (that: MatrixPoly): MatrixPoly = new MatrixPoly(for (rthis <- this.elms) yield (for (cthat <- that.elms.transpose) yield dotProduct(rthis, cthat)))
	// 多項式行列 * 多項式ベクトル
	def * (that: VektorPoly): VektorPoly = new VektorPoly(for (rthis <- this.elms) yield dotProduct(rthis, that.elms))
	// 多項式行列 * スカラー行列
	def * [T: Numeric](that: MatrixNum[T]): MatrixPoly = new MatrixPoly(for (rthis <- this.elms) yield (for (cthat <- that.elms.transpose) yield dotProduct(rthis, cthat)))
	// 多項式行列 * スカラーベクトル
	def * [T: Numeric](that: VektorNum[T]): VektorPoly = new VektorPoly(for (rthis <- this.elms) yield dotProduct(rthis, that.elms))

	/* スカラー乗算 */
	def * (that: Polynomial): MatrixPoly = new MatrixPoly(this.elms map (m => m.map(r => r * that)))

	/* modular */
	def mod (n: BigInt): MatrixPoly = {
		new MatrixPoly(this.elms map (t => t map (t => t.mod(n))))
	}

	/* 転置行列 */
	def transpose = new MatrixPoly(this.elms.transpose)

	def col(n: Int): VektorPoly = null
	def row(n: Int): VektorPoly = null

	override def toString() = (this.elms map (_.mkString("[", ",", "]"))).mkString("", "\n", "\n")
}
