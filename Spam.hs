-- | Type classes and functions associated with marking data as spam
module Spam
( -- * Classes
Spam(..)
) where

import Control.Monad.Reader
import Database.HDBC
import Store

-- | A type which can be classified as spam (or not)
class (ToSQL a) => Spam a where
    -- | Mark the item as spam in the database
    mark :: (IConnection c) => Bool -> a -> ReaderT c IO Integer
    isSpam :: a -> Maybe Bool
