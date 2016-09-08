module NotAWoWKiller.Model.Skill where

import Data.Text

import NotAWoWKiller.Model.Modifier
import NotAWoWKiller.Model.Cost
import NotAWoWKiller.Model.School

data Skill = Skill {
    passive :: [Modifier],
    cost :: Cost,
    castingSchool :: [School],
    name :: Text
    }
