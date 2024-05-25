-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.ApprenticeYear.Rules
  ( rules
  ) where

import Data.String
import Prelude
import Data.String

import Duckling.Dimensions.Types
import Duckling.Types
import Duckling.Regex.Types
import Duckling.ApprenticeYear.Helpers
import Duckling.Numeral.Helpers (isPositive)
import Duckling.Ordinal.Helpers (isSimpleOrdinal)
import qualified Duckling.ApprenticeYear.Types as TApprenticeYear
import qualified Duckling.Numeral.Types as TNumeral
import qualified Duckling.Ordinal.Types as TOrdinal

ruleNumeralAsApprenticeYear :: Rule
ruleNumeralAsApprenticeYear = Rule
  { name = "number as apprentice_year"
  , pattern =
    [ Predicate isPositive
    ]
  , prod = \case
    (Token Numeral TNumeral.NumeralData {TNumeral.value = v}:
     _) ->
      Just . Token ApprenticeYear $ valueOnly v
    _ -> Nothing
  }

ruleNumeralApprenticeYears :: Rule
ruleNumeralApprenticeYears = Rule
  { name = "<number> <apprentice_year>"
  , pattern =
    [ Predicate isPositive
    , Predicate isUnitOnly
    ]
  , prod = \case
    (Token Numeral TNumeral.NumeralData{TNumeral.value = v}:
     Token ApprenticeYear TApprenticeYear.ApprenticeYearData{TApprenticeYear.unit = Just u}:
     _) ->
      Just . Token ApprenticeYear $ apprentice_year u v
    _ -> Nothing
  }

ruleNumeralPointApprenticeYears :: Rule
ruleNumeralPointApprenticeYears = Rule
  { name = "<ordinal> <apprentice_year>"
  , pattern =
    [ Predicate isSimpleOrdinal
    , Predicate isUnitOnly
    ]
  , prod = \case
    (Token Ordinal TOrdinal.OrdinalData{TOrdinal.value = v}:
     Token ApprenticeYear TApprenticeYear.ApprenticeYearData{TApprenticeYear.unit = Just u}:
     _) ->
      Just . Token ApprenticeYear $ apprentice_year u (fromIntegral v)
    _ -> Nothing
  }

ruleIntervalNumeralDash :: Rule
ruleIntervalNumeralDash = Rule
  { name = "<numeral> - <apprentice_year>"
  , pattern =
    [ Predicate isPositive
    , regex "\\-"
    , Predicate isSimpleApprenticeYear
    ]
  , prod = \case
      (Token Numeral TNumeral.NumeralData{TNumeral.value = from}:
       _:
       Token ApprenticeYear TApprenticeYear.ApprenticeYearData{TApprenticeYear.value = Just to
                                  , TApprenticeYear.unit = Just u}:
       _) | from < to ->
        Just . Token ApprenticeYear . withInterval (from, to) $ unitOnly u
      _ -> Nothing
  }

ruleIntervalDash :: Rule
ruleIntervalDash = Rule
  { name = "<apprentice_year> - <apprentice_year>"
  , pattern =
    [ Predicate isSimpleApprenticeYear
    , regex "\\-"
    , Predicate isSimpleApprenticeYear
    ]
  , prod = \case
      (Token ApprenticeYear TApprenticeYear.ApprenticeYearData{TApprenticeYear.value = Just from
                                  , TApprenticeYear.unit = Just u1}:
       _:
       Token ApprenticeYear TApprenticeYear.ApprenticeYearData{TApprenticeYear.value = Just to
                                  , TApprenticeYear.unit = Just u2}:
       _) | from < to && u1 == u2 ->
        Just . Token ApprenticeYear . withInterval (from, to) $ unitOnly u1
      _ -> Nothing
  }

rules :: [Rule]
rules = [ ruleNumeralAsApprenticeYear
        , ruleNumeralApprenticeYears
        , ruleNumeralPointApprenticeYears
        , ruleIntervalNumeralDash
        , ruleIntervalDash
        ]
