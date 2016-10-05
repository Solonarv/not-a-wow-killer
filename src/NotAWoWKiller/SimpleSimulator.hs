{-# LANGUAGE
    TypeOperators,
    DataKinds,
    GADTs,
    FlexibleContexts
    #-}
module NotAWoWKiller.SimpleSimulator where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Default

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

import Control.Lens
import Control.Monad.Freer
import Control.Monad.Freer.Internal
import Control.Monad.Freer.State

import Control.Monad.Freer.Extensions
import NotAWoWKiller.Model

data World = World {
    _worldUnits :: IntMap Unit
    }

instance Default World where def = World IntMap.empty

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

addState :: Eff (a ': b ': r) w -> Eff (a ': b ': State s ': r) w
addState = mapEffU $ unionUnder (unionUnder weaken)

-- Spiders (a.k.a. mutable global state) beyond this point

globalWorld :: IORef World
globalWorld = unsafePerformIO (newIORef def)
{-# NOINLINE globalWorld #-}

setGlobalWorld :: World -> IO ()
setGlobalWorld w = writeIORef globalWorld w

getGlobalWorld :: IO World
getGlobalWorld = readIORef globalWorld

resetGlobalWorld :: IO ()
resetGlobalWorld = writeIORef globalWorld def

globalCaster :: IORef UnitID
globalCaster = unsafePerformIO (newIORef 0)
{-# NOINLINE globalCaster #-}

setGlobalCaster :: UnitID -> IO ()
setGlobalCaster uid = writeIORef globalCaster uid

getGlobalCaster :: IO UnitID
getGlobalCaster = readIORef globalCaster

globalTarget :: IORef (Maybe UnitID)
globalTarget = unsafePerformIO (newIORef Nothing)
{-# NOINLINE globalTarget #-}

setGlobalTarget :: UnitID -> IO ()
setGlobalTarget uid = writeIORef globalTarget (Just uid)

clearGlobalTarget :: IO ()
clearGlobalTarget = writeIORef globalTarget Nothing

getGlobalTarget :: IO (Maybe UnitID)
getGlobalTarget = readIORef globalTarget

rWorld :: Eff '[ReadWorld] w -> IO w
rWorld comp = do
    caster <- getGlobalCaster
    world <- getGlobalWorld
    return $ run $ runReadWorld world caster comp

rwWorld :: Eff '[ReadWorld, WriteWorld] w -> IO w
rwWorld comp = do
    caster <- getGlobalCaster
    target <- getGlobalTarget
    world <- getGlobalWorld
    let scomp = runReadWriteWorld caster target (addState comp)
    let (res, nw) = run $ runState scomp world
    setGlobalWorld world 
    return res

