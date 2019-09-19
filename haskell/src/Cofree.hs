module Cofree where

import           Control.Comonad.Cofree
import           Data.Functor.Const
import           Data.Functor.Identity

-- what the fuck is Cofree?


-- so we can make a shitty list with Maybe
weirdList :: Cofree Maybe Int
weirdList = 1 :< Just (2 :< Just (3 :< Nothing))

-- or an incredibly weird Pair with Const
pair :: Cofree (Const Int) String
pair = "horse" :< Const 1


