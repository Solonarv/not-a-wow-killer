{-# LANGUAGE
    TypeOperators,
    DataKinds,
    GADTs,
    FlexibleContexts
    #-}
module NotAWoWKiller.SimpleSimulator where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Control.Lens
import Control.Monad.Freer
import Control.Monad.Freer.Internal
import Control.Monad.Freer.State

import NotAWoWKiller.Model

data World = World {
    _worldUnits :: IntMap Unit
    }

worldUnits :: Lens' World (IntMap Unit)
worldUnits f w = fmap (\us -> w {_worldUnits = us}) (f $ _worldUnits w)

runReadWorld :: World -> UnitID -> Eff (ReadWorld ': r)  w -> Eff r w
runReadWorld world caster (Val a) = return a
runReadWorld world caster (E u q) = case decomp u of
    Right GetCaster      -> runReadWorld world caster $ qApp q caster
    Right GetTarget      -> runReadWorld world caster $ qApp q Nothing
    Right (UnitInfo uid) -> runReadWorld world caster $ qApp q (world ^. worldUnits . at uid)
    Left u'              -> E u' (tsingleton (\x -> runReadWorld world caster (qApp q x)))

runReadWriteWorld :: Member (State World) r => UnitID -> Maybe UnitID -> Eff (ReadWorld ': WriteWorld ': r) w -> Eff r w
runReadWriteWorld caster target (Val a) = return a
runReadWriteWorld caster target (E u q) = case decomp u of
    Right GetCaster      -> runReadWriteWorld caster target (qApp q caster)
    Right GetTarget      -> runReadWriteWorld caster target (qApp q target)
    Right (UnitInfo uid) -> runReadWriteWorld caster target $ (view (worldUnits . at uid) <$> get) >>= qApp q
    Left u'              -> case decomp u' of
        Right (DamageUnit uid dmg) -> runReadWriteWorld caster target $ do
            modify $ worldUnits . at uid . _Just %~ damagedBy dmg
            qApp q dmg
        Right (ApplyModifier uid mod args duration) -> runReadWriteWorld caster target $ do
            let unitL = worldUnits . at uid
            tar' <- view unitL <$> get
            case tar' of
                Just tar -> do
                    let modid = tar ^. unitNextModID
                    --modify $ unitL . _Just . unitNextModID +~ 1
                    let modinst = ModifierInstance {
                        _miOwner = uid,
                        _miCaster = caster,
                        _miModifier = mod,
                        _miModArguments = args,
                        _miDuration = duration,
                        _miRemainingDuration = duration,
                        _miID = modid
                        }
                    --modify $ unitL . _Just . unitActiveEffects . at modid ?~ modinst
                    return ()
                Nothing -> return ()
            qApp q ()
        Right (RemoveModifier uid modinst) -> runReadWriteWorld caster target $ do
            unit' <- view (worldUnits . at uid) <$> get
            case unit' of
                Just unit -> do
                    let hasMod = isn't _Nothing $ unit ^. unitActiveEffects . at (modinst ^. miID)
                    --modify $ worldUnits . at uid . _Just . unitActiveEffects . at (modinst ^. miID) .~ Nothing
                    qApp q hasMod
                Nothing -> qApp q False
            