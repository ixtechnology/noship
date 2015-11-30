{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
module SharedTypes where

import Prelude
import Data.Data
import Fay.Yesod
import Data.Text

data Command = GetFib Int (Returns Text)
    deriving (Typeable, Data)

