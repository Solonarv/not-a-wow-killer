{-# LANGUAGE
    TemplateHaskell,
    GADTs,
    KindSignatures,
    DataKinds,
    FlexibleInstances,
    FlexibleContexts
    #-}

module NotAWoWKiller.Model where

import Data.Text
import Control.Lens
import Control.Monad.Freer
import Data.Set (Set)
import qualified Data.Set as Set

data Cost = Cost {
    _cost_mana :: Int,
    _cost_rage :: Int,
    _cost_energy :: Int
} deriving (Eq, Ord, Show)

cost_mana, cost_rage, cost_energy :: Lens' Cost Int
cost_mana = lens (_cost_mana) (\c i -> c {_cost_mana = i})
cost_energy = lens (_cost_energy) (\c i -> c {_cost_energy = i})
cost_rage = lens (_cost_rage) (\c i -> c {_cost_rage = i})

data Modifier = Modifier -- To be filled in later
    deriving (Eq, Show)

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

type School = Set BaseSchool

singleSchool :: BaseSchool -> School
singleSchool = Set.singleton

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
    _res_max :: typ,
    _res_current :: typ
    }

res_max, res_current :: Lens' (Resource rc t) t
res_max = lens (_res_max) (\r t -> r { _res_max = t })
res_current = lens (_res_current) (\r t -> r { _res_current = t })

class ResourceRecovery a where recovery :: Getter a Ordering
instance ResourceRecovery (Resource 'LT t) where recovery = to $ const LT
instance ResourceRecovery (Resource 'EQ t) where recovery = to $ const EQ
instance ResourceRecovery (Resource 'GT t) where recovery = to $ const GT

type Health = Resource 'EQ Double
type Mana   = Resource 'EQ Int
type Energy = Resource 'GT Int
type Rage   = Resource 'LT Int

data Unit = Unit {
    _unit_health        :: Health,
    _unit_mana          :: Mana,
    _unit_energy        :: Energy,
    _unit_rage          :: Rage,
    _unit_activeEffects :: [Modifier]
    }

unit_health        :: Lens' Unit Health
unit_health = lens _unit_health (\u h -> u { _unit_health = h})
unit_mana          :: Lens' Unit Mana
unit_mana = lens _unit_mana (\u m -> u { _unit_mana = m})
unit_energy        :: Lens' Unit Energy
unit_energy = lens _unit_energy (\u e -> u {_unit_energy = e})
unit_rage          :: Lens' Unit Rage
unit_rage = lens _unit_rage (\u r -> u {_unit_rage = r})
unit_activeEffects :: Lens' Unit [Modifier]
unit_activeEffects = lens _unit_activeEffects (\u ae -> u {_unit_activeEffects = ae})

data ReadWorld a where
    GetStat :: Unit -> StatDesc -> ReadWorld Stat
    HasModifier :: Unit -> Modifier -> ReadWorld [ModifierInstance]
    GetCaster :: ReadWorld Unit
    GetTarget :: ReadWorld Unit

getStat :: Member ReadWorld r => Unit -> StatDesc -> Eff r Stat
getStat u sd = send $ GetStat u sd

hasModifier :: Member ReadWorld r => Unit -> Modifier -> Eff r [ModifierInstance]
hasModifier u m = send $ HasModifier u m

getCaster :: Member ReadWorld r => Eff r Unit
getCaster = send GetCaster

getTarget :: Member ReadWorld r => Eff r Unit
getTarget = send GetTarget

data WriteWorld a where
    DamageUnit :: Unit -> Damage -> WriteWorld Damage
    ApplyModifier :: Unit -> (Unit -> ModifierInstance) -> WriteWorld ()
    RemoveModifier :: Unit -> ModifierInstance -> WriteWorld Bool

damageUnit :: Member WriteWorld r => Unit -> Damage -> Eff r Damage
damageUnit u d = send $ DamageUnit u d

applyModifier :: Member WriteWorld r => Unit -> (Unit -> ModifierInstance) -> Eff r ()
applyModifier u mkMI = send $ ApplyModifier u mkMI

removeModifier :: Member WriteWorld r => Unit -> ModifierInstance -> Eff r Bool
removeModifier u mi = send $ RemoveModifier u mi

data Skill = Skill {
    _skill_passives :: [Modifier],
    _skill_cost :: Eff '[ReadWorld] Cost,
    _skill_castingSchool :: School,
    _skill_name :: Text,
    _skill_effects :: Eff '[ReadWorld, WriteWorld] ()
    }

skill_passives :: Lens' Skill [Modifier]
skill_passives = lens _skill_passives (\skill p -> skill {_skill_passives = p})
skill_cost :: Lens' Skill (Eff '[ReadWorld] Cost)
skill_cost = lens _skill_cost (\skill p -> skill {_skill_cost = p})
skill_castingSchool :: Lens' Skill School
skill_castingSchool = lens _skill_castingSchool (\skill p -> skill {_skill_castingSchool = p})
skill_name :: Lens' Skill Text
skill_name = lens _skill_name (\skill p -> skill {_skill_name = p})
skill_effects :: Lens' Skill (Eff '[ReadWorld, WriteWorld] ())
skill_effects = lens _skill_effects (\skill p -> skill {_skill_effects = p})

data ModifierInstance = ModifierInstance deriving (Eq, Show)
data Stat = Stat         deriving (Eq, Show)
data StatDesc = StatDesc deriving (Eq, Show)
data Damage = Damage {
    _dmg_amount :: Double,
    _dmg_school :: School
    } deriving (Eq, Show)

-- Generated by `makeLenses ''Damage`
dmg_amount :: Lens' Damage Double
dmg_amount f_aagx (Damage x_aagy x_aagz)
  = fmap (\ y_aagA -> Damage y_aagA x_aagz) (f_aagx x_aagy)
{-# INLINE dmg_amount #-}
dmg_school :: Lens' Damage School
dmg_school f_aagB (Damage x_aagC x_aagD)
  = fmap (\ y_aagE -> Damage x_aagC y_aagE) (f_aagB x_aagD)
{-# INLINE dmg_school #-}