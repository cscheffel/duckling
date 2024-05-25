-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}


module Duckling.ApprenticeYear.Helpers
  ( isSimpleApprenticeYear
  , isUnitOnly
  , apprentice_year
  , unitOnly
  , valueOnly
  , withUnit
  , withValue
  , withInterval
  , withMin
  , withMax
  ) where

import Data.Text (Text)
import Prelude

import Duckling.Dimensions.Types
import Duckling.ApprenticeYear.Types (Unit(..), ApprenticeYearData(..))
import Duckling.Types
import qualified Duckling.ApprenticeYear.Types as TApprenticeYear

-- -----------------------------------------------------------------
-- Patterns

isSimpleApprenticeYear :: Predicate
isSimpleApprenticeYear (Token ApprenticeYear ApprenticeYearData {TApprenticeYear.value = Just _
                                        , TApprenticeYear.minValue = Nothing
                                        , TApprenticeYear.maxValue = Nothing}) = True
isSimpleApprenticeYear _ = False

isUnitOnly :: Predicate
isUnitOnly (Token ApprenticeYear ApprenticeYearData {TApprenticeYear.value = Nothing
                                    , TApprenticeYear.unit = Just _
                                    , TApprenticeYear.minValue = Nothing
                                    , TApprenticeYear.maxValue = Nothing}) = True
isUnitOnly _ = False

-- -----------------------------------------------------------------
-- Production

apprentice_year :: Unit -> Double -> ApprenticeYearData
apprentice_year u v = ApprenticeYearData {TApprenticeYear.unit = Just u
                        , TApprenticeYear.value = Just v
                        , TApprenticeYear.minValue = Nothing
                        , TApprenticeYear.maxValue = Nothing}

unitOnly :: Unit -> ApprenticeYearData
unitOnly u = ApprenticeYearData {TApprenticeYear.unit = Just u
                        , TApprenticeYear.value = Nothing
                        , TApprenticeYear.minValue = Nothing
                        , TApprenticeYear.maxValue = Nothing}

valueOnly :: Double -> ApprenticeYearData
valueOnly v = ApprenticeYearData {TApprenticeYear.unit = Nothing
                        , TApprenticeYear.value = Just v
                        , TApprenticeYear.minValue = Nothing
                        , TApprenticeYear.maxValue = Nothing}

withUnit :: Unit -> ApprenticeYearData -> ApprenticeYearData
withUnit u vd = vd {TApprenticeYear.unit = Just u}

withValue :: Double -> ApprenticeYearData -> ApprenticeYearData
withValue v vd = vd {TApprenticeYear.value = Just v}

withInterval :: (Double, Double) -> ApprenticeYearData -> ApprenticeYearData
withInterval (from, to) vd = vd {TApprenticeYear.value = Nothing
                                , TApprenticeYear.minValue = Just from
                                , TApprenticeYear.maxValue = Just to}

withMin :: Double -> ApprenticeYearData -> ApprenticeYearData
withMin from vd = vd {TApprenticeYear.minValue = Just from}

withMax :: Double -> ApprenticeYearData -> ApprenticeYearData
withMax to vd = vd {TApprenticeYear.maxValue = Just to}
