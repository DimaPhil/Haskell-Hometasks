module Ch where

--ch x iterations считает гиперболический косинус от x (cosh x). Для вычисления суммируются первые iterations членов ряда Тейлора.
ch :: Double -> Int -> Double
ch x iters = processTeilor x 1 1 0 iters where
  processTeilor xs step lastMember lastAnswer iterations
    | iterations == 0 = lastAnswer + 1
    | otherwise       = processTeilor x (step + 1) newMember newAnswer (iterations - 1)
    where
      newMember = lastMember * xs * xs / (2 * step) / (2 * step - 1)
      newAnswer = lastAnswer + newMember
