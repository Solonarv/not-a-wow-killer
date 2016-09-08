module NotAWoWKiller.Model.School where

import Data.Set (Set)
import qualified Data.Set as Set

import Control.Lens

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

newtype School = School { schoolComponents :: Set BaseSchool }
