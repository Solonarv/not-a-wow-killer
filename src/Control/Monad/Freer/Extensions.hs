{-# LANGUAGE
    TypeOperators,
    DataKinds,
    FlexibleContexts,
    GADTs,
    TemplateHaskell,
    RankNTypes
    #-}

module Control.Monad.Freer.Extensions (
    mapEffU, mapArrsU, swapUnion, unionUnder, weaken
    ) where

import Control.Monad.Freer
import Control.Monad.Freer.Internal
import Data.Open.Union
import Data.FTCQueue

mapEffU :: (forall w. Union r w -> Union r' w) -> (forall w. Eff r w -> Eff r' w)
mapEffU nat (Val x) = Val x
mapEffU nat (E u q) = E (nat u) (mapArrsU nat q)

mapArrsU :: (forall w. Union r w -> Union r' w) -> (forall w. Arrs r v w -> Arrs r' v w)
mapArrsU nat q = case tviewl q of
    TOne k  -> tsingleton (mapEffU nat . k)
    k :| q' -> tsingleton (mapEffU nat . k) >< mapArrsU nat q'

swapUnion :: Union (a ': b ': r) w -> Union (b ': a ': r) w
swapUnion u = case decomp u of
    Right a -> weaken (inj a)
    Left u' -> case decomp u' of
        Right b -> inj b
        Left r  -> weaken (weaken r)

unionUnder :: (forall w. Union r w -> Union r' w) -> (forall w. Union (a ': r) w -> Union (a ': r') w)
unionUnder nat u = case decomp u of
    Right a -> inj a
    Left r  -> weaken (nat r)

-- swapEff :: Eff (a ': b ': r) w -> Eff (b ': a ': r) w
-- swapEff (Val x) = Val x
-- swapEff (E u q) = case decomp u of
--     Right conA     -> E     (weaken (inj conA)) (swapArrs q)
--     Left r     -> case decomp r of
--         Right conB -> E             (inj conB)   (swapArrs q)
--         Left  r'   -> E (weaken (weaken r'   )) (swapArrs q)
-- 
-- swapArr :: Arr (a ': b ': r) v w -> Arr (b ': a ': r) v w
-- swapArr k = (swapEff . k)
-- 
-- swapArrs :: Arrs (a ': b ': r) v w -> Arrs (b ': a ': r) v w
-- swapArrs q = case tviewl q of
--     TOne k -> tsingleton (swapArr k)
--     k :| q -> (tsingleton $ swapArr k) >< (swapArrs q)