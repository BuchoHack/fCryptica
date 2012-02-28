package fCryptica.crypto

import scala.util._
import fCryptica.security._
import fCryptica.math._

object Knapsack {
	def KeyGen(n: Int, seed: Int, random: Random): KeyPair = {
		val A = new VektorNum[BigInt]()
		val a1 = BigInt(seed, random) + 1				// a1 > 1
		A addLast a1
		val a2 = BigInt(seed, random) + 1 + A(0)		// a2 > a1
		A addLast a2
		for(i <- 2 to n - 1) {
			// an > an-1 * (an-2 + an-3 + ... + a1)
			val ai = BigInt(seed, random) + 1 + A(i-1) * (A.sum - A(i - 1))
			A addLast ai
		}
		// p > an * (an-1 + an-2 + ... + a1)
		val p = BigInt(seed, random) + 1 + A(n - 1) * (A.sum - A(n - 1))
		// alpha <- gcd(alpha, p) == 1
		var alpha = BigInt(0)
		while(p.gcd(alpha) != 1) alpha = BigInt(p.bitLength, random)
		// bi = alpha * ai
		val B = A * alpha
//		println(Random.shuffle(B.elms))
		return new KeyPair(new PublicKey(B), new PrivateKey(A,p,alpha))
	}

	def Enc(publickey: PublicKey, plain: BigInt, random: Random): BigInt = {
		val B = publickey.getPublicSequence()
		val N = B.size
		val test = BigInt(10)
//		val msg = BitList.fromBigInt(test)	// ビット配列に変換
		val msg = scala.collection.mutable.ArrayBuffer(1,1,1,0,0)
		val message = new VektorNum[Int](msg)		// ベクトルとして扱う

		val buf = new VektorNum[BigInt]()

		var one = 0							// message:1の要素の個数
		for(i <- 0 to N - 1) {
			if(message(i) == BigInt(1)) one += 1
			buf addLast (message(i) * B(i))
		}

		if(one < 2) {
			System.out.println("message error")
			return null
		} else {
			val separate = (random.nextInt(one / 2) + 1) * 2	// 分割数

			val separatetmp = new VektorNum[BigInt]()
			for(i <- 0 to separate) {
				var tmprand = random.nextInt(buf.size())
				do {
					separatetmp addLast (buf(tmprand))
					buf.update(tmprand,BigInt(0))
					tmprand = random.nextInt(buf.size())
				} while(buf(tmprand) != 0)
			}
			for(i <- 0 to N - 1) {
				val separaterand = random.nextInt(separate);
				separatetmp.update(separaterand, separatetmp(separaterand) + (buf(i)));
			}
			var encrypt = BigInt(0)
			for(i <- 0 to separate / 2) {
				var separaterand = 0
				while(separaterand <= i - 1) separaterand = random.nextInt(separate)
				encrypt += separatetmp(i) * (separatetmp(separaterand))
			}
			return encrypt
		}
	}
	def Dec(privatekey: PrivateKey, plain: Array[Byte]): Array[Byte] = {
		return null
	}

	class PublicKey(B: VektorNum[BigInt]) {
		def	getAlgorithm(): String = "Knapsack"
		def getEncoded(): Array[Byte] = null
		def getFormat(): String = null
		def getPublicSequence(): VektorNum[BigInt] = B
	}

	class PrivateKey(A: VektorNum[BigInt], p: BigInt, alpha: BigInt) {
		def	getAlgorithm(): String = "Knapsack"
		def getEncoded(): Array[Byte] = null
		def getFormat(): String = null
		def getPrivateSequence(): VektorNum[BigInt] = A
		def getModule(): BigInt = p
		def getAlpha(): BigInt = alpha
	}

	class KeyPair(PK: PublicKey, SK: PrivateKey) {
		def getPK() = PK
		def getSK() = SK
	}

	def main(args: Array[String]) = {
		val random = SecureRandom.getInstance("SHA1PRNG")
		val keyPair = KeyGen(5, 2, random)
		val publickey = keyPair.getPK()
		val privatekey = keyPair.getSK()

		val encoded = Enc(publickey, 2, random)
		println("encoded = "+encoded)
	}
}
