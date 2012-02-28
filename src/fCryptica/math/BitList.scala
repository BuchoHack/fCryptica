package fCryptica.math

import scala.collection.mutable._

object BitList {
	/* BigIntをbit配列に変換する */
	def fromBigInt(n: BigInt): ListBuffer[BigInt] = {
		val bits = ListBuffer[BigInt]()
		for(i <- 0 to n.bitLength - 1) ((n >> i) & 1) +=: bits
		return bits
	}
}