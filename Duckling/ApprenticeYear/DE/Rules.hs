-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.ApprenticeYear.DE.Rules
  ( rules ) where

import Data.Text (Text)
import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Types
import Duckling.Regex.Types
import Duckling.ApprenticeYear.Helpers
import Duckling.Numeral.Helpers (isPositive)
import qualified Duckling.ApprenticeYear.Types as TApprenticeYear
import qualified Duckling.Numeral.Types as TNumeral

apprenticeyears :: [(Text, String, TApprenticeYear.Unit)]
apprenticeyears = [ ("<apprentice_year> apprentice_year", "L(J|ehrjahr[e]?)", TApprenticeYear.Lehrjahr)
      , ("<apprentice_year> apprentice_year", "A(J|usbildungsjahr[e]?)", TApprenticeYear.Lehrjahr)
      , ("<apprentice_year> apprentice_year", "(Studienjahr[e]?)", TApprenticeYear.Lehrjahr)
      , ("<apprentice_year> apprentice_year", "(Semester)", TApprenticeYear.Lehrjahr)]

rulesApprenticeYears :: [Rule]
rulesApprenticeYears = map go apprenticeyears
  where
    go :: (Text, String, TApprenticeYear.Unit) -> Rule
    go (name, regexPattern, u) = Rule
      { name = name
      , pattern =
        [ regex regexPattern
        ]
      , prod = \_ -> Just . Token ApprenticeYear $ unitOnly u
      }

rulePrecision :: Rule
rulePrecision = Rule
  { name = "about <apprentice_year>"
  , pattern =
    [ regex "\\~|(ganz )?genau|präzise|(in )?etwa|ungefähr|um( die)?|fast"
    , dimension ApprenticeYear
    ]
    , prod = \case
      (_:token:_) -> Just token
      _ -> Nothing
  }

ruleIntervalBetweenNumeral :: Rule
ruleIntervalBetweenNumeral = Rule
  { name = "between|from <numeral> and|to <apprentice_year>"
  , pattern =
    [ regex "zwischen|von"
    , Predicate isPositive
    , regex "und|bis( zu)?"
    , Predicate isSimpleApprenticeYear
    ]
  , prod = \case
      (_:
       Token Numeral TNumeral.NumeralData{TNumeral.value = from}:
       _:
       Token ApprenticeYear TApprenticeYear.ApprenticeYearData{TApprenticeYear.value = Just to
                                  , TApprenticeYear.unit = Just u}:
       _) | from < to ->
        Just . Token ApprenticeYear . withInterval (from, to) $ unitOnly u
      _ -> Nothing
  }

rules :: [Rule]
rules = [ rulePrecision
        , ruleIntervalBetweenNumeral
        ]
        ++ rulesApprenticeYears

