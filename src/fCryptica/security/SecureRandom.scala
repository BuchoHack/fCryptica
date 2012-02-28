package fCryptica.security

import scala.util.Random

object SecureRandom {
	def getInstance(algorithm: String): SecureRandom = {
		new SecureRandom(java.security.SecureRandom.getInstance(algorithm))
	}

	def getInstance(algorithm: String, provider: String): SecureRandom = {
		new SecureRandom(java.security.SecureRandom.getInstance(algorithm, provider))
	}
	def getSeed(numBytes: Int): Array[Byte] = {
		java.security.SecureRandom.getSeed(numBytes)
	}
}

class SecureRandom(self: java.security.SecureRandom) extends Random {
	/**
	 * Constructs a secure random number generator (RNG) implementing the
	 * default random number algorithm.
	 */
	def this() = {
		this(new java.security.SecureRandom())
	}

	/**
	 * Constructs a secure random number generator (RNG) implementing the
	 * default random number algorithm.
	 * The SecureRandom instance is seeded with the specified seed bytes.
	 */
	def this(seed: Array[Byte]) = {
		this(new java.security.SecureRandom(seed))
	}

	def getProvider(): java.security.Provider = {
		self.getProvider
	}

	def getAlgorithm(): String = {
		self.getAlgorithm
	}

	override def setSeed(seed: Long): Unit = {
		self.setSeed(seed)
	}

	override def nextBytes(bytes: Array[Byte]) = {
		self.nextBytes(bytes)
	}

	def generateSeed(numBytes: Int): Array[Byte] = {
		self.generateSeed(numBytes)
	}
}