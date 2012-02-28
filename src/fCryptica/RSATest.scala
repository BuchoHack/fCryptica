package fCryptica

import fCryptica.crypto._
import fCryptica.security._

object RSATest {
	def main(args: Array[String]) = {
		val random = SecureRandom.getInstance("SHA1PRNG")
		val keyPair = RSA.KeyGenerate(512, BigInt(65537), random)

		val PK = keyPair.getPK()
		val SK = keyPair.getSK()

		val binary = "hello, world".getBytes
		println("plain   = " + new String(binary))
		val encoded = RSA.Enc(PK, binary)
		println("encoded = " + new String(encoded))
		val decoded = RSA.Dec(SK, encoded)
		println("decoded = " + new String(decoded))
	}
}
