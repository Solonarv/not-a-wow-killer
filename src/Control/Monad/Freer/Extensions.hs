{-# LANGUAGE
    TypeOperators,
    DataKinds,
    FlexibleContexts,
    GADTs,
    TemplateHaskell
    #-}

module Control.Monad.Freer.Extensions where

import Control.Monad.Freer
import Control.Monad.Freer.Internal
import Data.Open.Union
import Data.FTCQueue

swapEff :: Eff (a ': b ': r) w -> Eff (b ': a ': r) w
swapEff (Val x) = Val x
swapEff (E u q) = case decomp u of
    Right conA     -> E     (weaken (inj conA)) (swapArrs q)
    Left r     -> case decomp r of
        Right conB -> E             (inj conB)   (swapArrs q)
        Left  r'   -> E (weaken (weaken r'   )) (swapArrs q)

swapArr :: Arr (a ': b ': r) v w -> Arr (b ': a ': r) v w
swapArr k = (swapEff . k)

swapArrs :: Arrs (a ': b ': r) v w -> Arrs (b ': a ': r) v w
swapArrs q = case tviewl q of
    TOne k -> tsingleton (swapArr k)
    k :| q -> (tsingleton $ swapArr k) >< (swapArrs q)