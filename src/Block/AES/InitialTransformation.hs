module Block.AES.InitialTransformation where

import Matrix (BitMatrix, xorBitMatrix)

initialTransformation :: BitMatrix -> BitMatrix -> BitMatrix
initialTransformation = xorBitMatrix