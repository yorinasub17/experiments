{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, OverloadedStrings,
  RecordWildCards, TemplateHaskell, TypeFamilies #-}
module Kakeibo.Server.Models.UserAcid where

import Control.Monad.Reader ( ask )
import Control.Monad.State ( get, put )
import Data.Acid ( Update, Query, makeAcidic )

import Data.Text ( Text )
import Data.Time ( UTCTime )
import Data.Data ( Data, Typeable )
import Data.IxSet ( Indexable(..), IxSet(..), Proxy, (@=), ixFun, ixSet, getOne )
import Data.SafeCopy ( SafeCopy, base, deriveSafeCopy )

import qualified Data.Text  as Text
import qualified Data.IxSet as IxSet

import Kakeibo.Server.Models.User
import Kakeibo.Server.Utils.Password

newtype Username = Username Text
    deriving (Eq, Ord, Data, Typeable, SafeCopy)

newtype APIToken = APIToken Text
    deriving (Eq, Ord, Data, Typeable, SafeCopy)

instance Indexable User where
    empty = ixSet
        [ ixFun $ \bp -> [ userId bp ]
        , ixFun $ \bp -> [ Username $ username bp ]
        , ixFun $ \bp -> [ APIToken $ apiToken bp ]
        , ixFun $ \bp -> [ dateJoined bp ]
        ]

data UserLog = UserLog
    { nextUserId :: UserId
    , users :: IxSet User
    } deriving (Data, Typeable)

initialUserLogState :: UserLog
initialUserLogState =
    UserLog { nextUserId = UserId 1
            , users = empty
            }

createUser :: UTCTime -> Text -> Text -> Text -> Text -> Update UserLog User
createUser newDateJoined newUsername newPassword newToken newEmail = do
    log@UserLog{..} <- get
    let user = User { userId = nextUserId
                    , username = newUsername
                    , apiToken = newToken
                    , password = hashPass newPassword
                    , email = newEmail
                    , dateJoined = newDateJoined
                    }
    put $ log { nextUserId = succ nextUserId
              , users = IxSet.insert user users
              }
    return user
 
allUsers :: Query UserLog [User]
allUsers = do
    UserLog{..} <- ask
    return $ IxSet.toDescList (IxSet.Proxy :: Proxy UTCTime) users

updateUserPass :: Text -> Text -> Update UserLog ()
updateUserPass username updatedPass = do
    log@UserLog{..} <- get
    let Just user = getOne $ users @= username
    let updatedUser = user { password = hashPass updatedPass }
    put $ log { users =
                  IxSet.updateIx (Username username) updatedUser users
              }

userById :: UserId -> Query UserLog (Maybe User)
userById uid = do
    UserLog {..} <- ask
    return $ getOne $ users @= uid

userByUsername :: Text -> Query UserLog (Maybe User)
userByUsername queriedUsername = do
    UserLog {..} <- ask
    return $ getOne $ users @= (Username queriedUsername)

userByAPIToken :: Text -> Query UserLog (Maybe User)
userByAPIToken queriedAPIToken = do
    UserLog {..} <- ask
    return $ getOne $ users @= (APIToken queriedAPIToken)

$(deriveSafeCopy 0 'base ''User)
$(deriveSafeCopy 0 'base ''UserLog)
$(makeAcidic ''UserLog
    [ 'createUser
    , 'updateUserPass
    , 'userById
    , 'userByUsername
    , 'userByAPIToken
    , 'allUsers
    ])
