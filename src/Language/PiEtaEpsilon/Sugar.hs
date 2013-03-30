module Language.PiEtaEpsilon.Sugar where
import Language.PiEtaEpsilon.Grammar

-- 22C5 -- \cdot
(@.) = TCompose
(@+) = TSum
(@*) = TTimes
(@=) = TId