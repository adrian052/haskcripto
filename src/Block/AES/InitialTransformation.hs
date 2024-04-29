module Block.AES.InitialTransformation where
import Block.Matrix (BitMatrix, xorBitMatrix)

initialTransformation :: BitMatrix -> BitMatrix -> BitMatrix
initialTransformation = xorBitMatrix