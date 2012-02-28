package fCryptica.crypto

import scala.util.Random
import fCryptica.security._

object RSA {
	def KeyGenerate(KEYBIT_LEN: Int, E: BigInt): KeyPair = {
		val random = SecureRandom.getInstance("SHA1PRNG")
		return KeyGenerate(KEYBIT_LEN, E, random)
	}

	def KeyGenerate(KEYBIT_LEN: Int, E: BigInt, random: Random): KeyPair = {
		val p = BigInt.probablePrime(KEYBIT_LEN, random)
		val q = BigInt.probablePrime(KEYBIT_LEN, random)
		val n = p * q
		val e = E
		val d = e.modInverse((p - 1) * (q - 1))
		return new KeyPair(new PublicKey(e,n), new PrivateKey(d,n))
	}

	def Enc(publickey: PublicKey, plain: Array[Byte]): Array[Byte] = {
		val plain_buf = BigInt(plain)
		val exponent = publickey.getPublicExponent()
		val modulus = publickey.getModulus()
		return plain_buf.modPow(exponent, modulus).toByteArray
	}

	def Enc(privatekey: PrivateKey, plain: Array[Byte]): Array[Byte] = {
		val plain_buf = BigInt(plain)
		val exponent = privatekey.getPrivateExponent()
		val modulus = privatekey.getModulus()
		return plain_buf.modPow(exponent, modulus).toByteArray
	}

	def Dec(privatekey: PrivateKey, plain: Array[Byte]): Array[Byte] = {
		val plain_buf = BigInt(plain)
		val exponent = privatekey.getPrivateExponent()
		val modulus = privatekey.getModulus()
		return plain_buf.modPow(exponent, modulus).toByteArray
	}

	def Dec(publickey: PublicKey, plain: Array[Byte]): Array[Byte] = {
		val plain_buf = BigInt(plain)
		val exponent = publickey.getPublicExponent()
		val modulus = publickey.getModulus()
		return plain_buf.modPow(exponent, modulus).toByteArray
	}

	class PublicKey(e: BigInt, n: BigInt) {
		def getModulus(): BigInt = n
		def getPublicExponent(): BigInt = e
	}

	class PrivateKey(d: BigInt, n: BigInt) {
		def getModulus(): BigInt = n
		def getPrivateExponent(): BigInt = d
	}

	class KeyPair(PK: PublicKey, SK: PrivateKey) {
		def getPK() = PK
		def getSK() = SK
	}
}
