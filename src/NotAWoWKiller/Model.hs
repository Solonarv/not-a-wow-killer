{-# LANGUAGE
    TemplateHaskell,
    GADTs,
    KindSignatures,
    DataKinds,
    FlexibleInstances,
    FlexibleContexts,
    RankNTypes,
    ImpredicativeTypes,
    OverloadedStrings
    #-}

module NotAWoWKiller.Model where

import Data.Text
import Data.Default
import Control.Lens
import Control.Monad.Freer
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map

data Cost = Cost {
    _costMana   :: Double,
    _costRage   :: Double,
    _costEnergy :: Double,
    _costHealth :: Double
    } deriving (Eq, Ord, Show)

instance Default Cost where def = Cost 0 0 0 0

costMana, costRage, costEnergy, costHealth :: Lens' Cost Double
costMana = lens (_costMana) (\c i -> c {_costMana = i})
costEnergy = lens (_costEnergy) (\c i -> c {_costEnergy = i})
costRage = lens (_costRage) (\c i -> c {_costRage = i})
costHealth = lens (_costHealth) (\c i -> c {_costHealth = i})

data Modifier = Modifier {
        _modOnApply     :: ModifierInstance -> Eff '[ReadWorld, WriteWorld] Bool,
        _modTickRate    :: ModifierInstance -> Eff '[ReadWorld] Double,
        _modOnTick      :: ModifierInstance -> Eff '[ReadWorld, WriteWorld] Bool,
        _modOnRemove    :: ModifierInstance -> () -> Eff '[ReadWorld, WriteWorld] Bool,
        _modName        :: ModifierInstance -> Text,
        _modDescription :: ModifierInstance -> Text
    }

instance Default Modifier where
    def = Modifier {
        _modOnApply = const (return True),
        _modTickRate = const (return 1),
        _modOnTick = const (return True),
        _modOnRemove = const (\_ -> return True),
        _modName = const "",
        _modDescription = const ""
        }

-- | All the Modifier fields have the same structure, this abbreviates the lens types for them
type ModLens r = Lens' Modifier (ModifierInstance -> r)

modOnApply           :: ModLens (Eff '[ReadWorld, WriteWorld] Bool)
modOnApply     f mod =  fmap (\x -> mod {_modOnApply = x}) (f $ _modOnApply mod)
modTickRate          :: ModLens (Eff '[ReadWorld] Double)
modTickRate    f mod =  fmap (\x -> mod {_modTickRate = x}) (f $ _modTickRate mod)
modOnTick            :: ModLens (Eff '[ReadWorld, WriteWorld] Bool)
modOnTick      f mod =  fmap (\x -> mod {_modOnTick = x}) (f $ _modOnTick mod)
modOnRemove          :: ModLens (() -> Eff '[ReadWorld, WriteWorld] Bool)
modOnRemove    f mod =  fmap (\x -> mod {_modOnRemove = x}) (f $ _modOnRemove mod)
modName              :: ModLens Text
modName        f mod =  fmap (\x -> mod {_modName = x}) (f $ _modName mod)
modDescription       :: ModLens Text
modDescription f mod =  fmap (\x -> mod {_modDescription = x}) (f $ _modDescription mod)


data BaseSchool = Physical
            | Cold
            | Fire
            | Nature
            | Arcane
            | Earth
            | Lightning
            | Wind
            | Light
            | Shadow
            | Chaos
        deriving (Eq, Ord, Show)

newtype School = School {_schoolParts :: Set BaseSchool} deriving Eq

instance Show School where show = show . Set.toList . _schoolParts
instance Default School where def = School Set.empty

singleSchool :: BaseSchool -> School
singleSchool = School . Set.singleton

physical  = singleSchool Physical
cold      = singleSchool Cold
fire      = singleSchool Fire
nature    = singleSchool Nature
arcane    = singleSchool Arcane
earth     = singleSchool Earth
lightning = singleSchool Lightning
wind      = singleSchool Wind
light     = singleSchool Light
shadow    = singleSchool Shadow
chaos     = singleSchool Chaos

data Resource (recovery :: Ordering) typ = Resource {
    _resCurrent :: typ,
    _resMax :: typ
    } deriving (Eq, Ord)

instance Default typ => Default (Resource ord typ) where def = Resource def def

resMax, resCurrent :: Lens' (Resource rc t) t
resMax     = lens (_resMax) (\r t -> r { _resMax = t })
resCurrent = lens (_resCurrent) (\r t -> r { _resCurrent = t })

class ResourceRecovery a where recovery :: Getter a Ordering
instance ResourceRecovery (Resource 'LT t) where recovery = to $ const LT
instance ResourceRecovery (Resource 'EQ t) where recovery = to $ const EQ
instance ResourceRecovery (Resource 'GT t) where recovery = to $ const GT

instance Show typ => Show (Resource 'LT typ) where show res = "Resource @LT " ++ show (res ^. recovery) ++ ' ': show (_resCurrent res) ++ ' ': show (_resMax res)
instance Show typ => Show (Resource 'EQ typ) where show res = "Resource @EQ " ++ show (res ^. recovery) ++ ' ': show (_resCurrent res) ++ ' ': show (_resMax res)
instance Show typ => Show (Resource 'GT typ) where show res = "Resource @GT " ++ show (res ^. recovery) ++ ' ': show (_resCurrent res) ++ ' ': show (_resMax res)
-- Fallthrough instance so GHC doesn't complain. This never actually gets used.
instance Show typ => Show (Resource ord typ) where show _ = error "This never happens."

type Health = Resource 'EQ Double
type Mana   = Resource 'EQ Int
type Energy = Resource 'GT Int
type Rage   = Resource 'LT Int

type UnitID = Int
data Unit = Unit {
    _unitHealth        :: Health,
    _unitMana          :: Mana,
    _unitEnergy        :: Energy,
    _unitRage          :: Rage,
    _unitActiveEffects :: IntMap ModifierInstance,
    _unitID            :: UnitID,
    _unitNextModID     :: ModInstanceID
    }

instance Default Unit where def = Unit def def def def IntMap.empty def def

unitHealth        :: Lens' Unit Health
unitHealth        =  lens _unitHealth (\u h -> u { _unitHealth = h})
unitMana          :: Lens' Unit Mana
unitMana          =  lens _unitMana (\u m -> u { _unitMana = m})
unitEnergy        :: Lens' Unit Energy
unitEnergy        =  lens _unitEnergy (\u e -> u {_unitEnergy = e})
unitRage          :: Lens' Unit Rage
unitRage          =  lens _unitRage (\u r -> u {_unitRage = r})
unitActiveEffects :: Lens' Unit (IntMap ModifierInstance)
unitActiveEffects =  lens _unitActiveEffects (\u ae -> u {_unitActiveEffects = ae})
unitID            :: Getter Unit Int
unitID            =  to _unitID
unitNextModID     :: Lens' Unit Int
unitNextModID     =  lens _unitNextModID (\u nmi -> u {_unitNextModID = nmi})

data ReadWorld a where
    --GetStat :: Unit -> StatDesc -> ReadWorld Stat
    --HasModifier :: Unit -> Modifier args -> ReadWorld [ModifierInstance args]
    UnitInfo  :: UnitID -> ReadWorld (Maybe Unit)
    GetCaster :: ReadWorld UnitID
    GetTarget :: ReadWorld (Maybe UnitID)

-- getStat :: Member ReadWorld r => Unit -> StatDesc -> Eff r Stat
-- getStat u sd = send $ GetStat u sd

-- hasModifier :: Member ReadWorld r => Unit -> Modifier a -> Eff r [ModifierInstance a]
-- hasModifier u m = send $ HasModifier u m

unitInfo :: Member ReadWorld r => UnitID -> Eff r (Maybe Unit)
unitInfo = send . UnitInfo

getCaster :: Member ReadWorld r => Eff r UnitID
getCaster = send GetCaster

getTarget :: Member ReadWorld r => Eff r (Maybe UnitID)
getTarget = send GetTarget

data WriteWorld a where
    DamageUnit :: UnitID -> Damage -> WriteWorld Damage
    ApplyModifier :: UnitID -> Modifier -> ModArgs -> Double -> WriteWorld ()
    RemoveModifier :: UnitID -> ModifierInstance -> WriteWorld Bool

damageUnit :: Member WriteWorld r => UnitID -> Damage -> Eff r Damage
damageUnit u d = send $ DamageUnit u d

applyModifier :: Member WriteWorld r => UnitID -> Modifier -> ModArgs -> Double -> Eff r ()
applyModifier target mod args duration = send $ ApplyModifier target mod args duration

removeModifier :: Member WriteWorld r => UnitID -> ModifierInstance -> Eff r Bool
removeModifier u mi = send $ RemoveModifier u mi

data Skill = Skill {
    _skillPassives :: [Modifier],
    _skillCost :: Eff '[ReadWorld] Cost,
    _skillCastingSchool :: School,
    _skillName :: Text,
    _skillEffects :: Eff '[ReadWorld, WriteWorld] ()
    }

instance Default Skill where def = Skill [] (return def) def "Default Skill" (return ())

skillPassives      :: Lens' Skill [Modifier]
skillPassives      =  lens _skillPassives (\skill p -> skill {_skillPassives = p})
skillCost          :: Lens' Skill (Eff '[ReadWorld] Cost)
skillCost          =  lens _skillCost (\skill p -> skill {_skillCost = p})
skillCastingSchool :: Lens' Skill School
skillCastingSchool =  lens _skillCastingSchool (\skill p -> skill {_skillCastingSchool = p})
skillName          :: Lens' Skill Text
skillName          =  lens _skillName (\skill p -> skill {_skillName = p})
skillEffects       :: Lens' Skill (Eff '[ReadWorld, WriteWorld] ())
skillEffects       =  lens _skillEffects (\skill p -> skill {_skillEffects = p})

data ModArgs = ModArgs {
    _modArgsInt :: Map String Integer,
    _modArgsDbl :: Map String Double,
    _modArgsTxt :: Map String Text,
    _modArgsUID :: Map String UnitID
    }

instance Default ModArgs where def = ModArgs def def def def

modArgsIntM :: Lens' ModArgs (Map String Integer)
modArgsDblM :: Lens' ModArgs (Map String Double )
modArgsTxtM :: Lens' ModArgs (Map String Text   )
modArgsUIDM :: Lens' ModArgs (Map String UnitID )
modArgsIntM f ma = fmap (\x -> ma { _modArgsInt = x }) (f $ _modArgsInt ma)
modArgsDblM f ma = fmap (\x -> ma { _modArgsDbl = x }) (f $ _modArgsDbl ma)
modArgsTxtM f ma = fmap (\x -> ma { _modArgsTxt = x }) (f $ _modArgsTxt ma)
modArgsUIDM f ma = fmap (\x -> ma { _modArgsUID = x }) (f $ _modArgsUID ma)

class IsModArg a where modArg :: String -> Lens' ModArgs (Maybe a)
instance IsModArg Integer where modArg s = modArgsIntM . at s
instance IsModArg Double  where modArg s = modArgsDblM . at s
instance IsModArg Text    where modArg s = modArgsTxtM . at s
instance IsModArg UnitID  where modArg s = modArgsUIDM . at s

instModArg :: IsModArg a => String -> Lens' ModifierInstance (Maybe a)
instModArg s = miModArguments . modArg s

type ModInstanceID = Int
data ModifierInstance = ModifierInstance {
    _miOwner  :: UnitID,
    _miCaster :: UnitID,
    _miModifier :: Modifier,
    _miModArguments :: ModArgs,
    _miDuration :: Double,
    _miRemainingDuration :: Double,
    _miID :: ModInstanceID
    }

miOwner, miCaster :: Lens' ModifierInstance UnitID
miOwner  f mi = fmap (\u -> mi { _miOwner  = u }) (f $ _miOwner  mi)
miCaster f mi = fmap (\u -> mi { _miCaster = u }) (f $ _miCaster mi)

miModifier :: Getter ModifierInstance Modifier
miModifier = to _miModifier

miModArguments :: Lens' ModifierInstance ModArgs
miModArguments f mi = fmap (\u -> mi { _miModArguments = u }) (f $ _miModArguments mi)

miDuration, miRemainingDuration :: Lens' ModifierInstance Double
miDuration          f mi = fmap (\dur -> mi { _miDuration          = dur }) (f $ _miDuration          mi)
miRemainingDuration f mi = fmap (\dur -> mi { _miRemainingDuration = dur }) (f $ _miRemainingDuration mi)

miID :: Getter ModifierInstance Int
miID = to _miID

data Stat = Stat { _statDesc :: StatDesc, _statVal :: Int } deriving (Eq, Show)
data StatDesc = SDStrength
              | SDDexterity
              | SDIntelligence
              | SDResistance School
              | SDResource SDRVal SDRResource
          deriving (Eq, Show)
data SDRVal = SDVCurrent | SDVMaximum | SDVRegen deriving (Eq, Ord, Show)
data SDRResource = Health | Mana | Energy | Rage deriving (Eq, Ord, Show)

data Damage = Damage {
    _dmgAmount :: Double,
    _dmgSchool :: School
    } deriving (Eq, Show)

instance Default Damage where def = Damage def def

-- Generated by `makeLenses ''Damage`
dmgAmount :: Lens' Damage Double
dmgAmount f_aagx (Damage x_aagy x_aagz)
  = fmap (\ y_aagA -> Damage y_aagA x_aagz) (f_aagx x_aagy)
{-# INLINE dmgAmount #-}
dmgSchool :: Lens' Damage School
dmgSchool f_aagB (Damage x_aagC x_aagD)
  = fmap (\ y_aagE -> Damage x_aagC y_aagE) (f_aagB x_aagD)
{-# INLINE dmgSchool #-}

damagedBy :: Damage -> Unit -> Unit
damagedBy dmg u = u & unitHealth . resCurrent %~ max 0 . (\x -> x - (dmg^.dmgAmount))