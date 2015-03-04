{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Serf where

import Data.Proxy
import GHC.TypeLits

newtype UserId
  = UserId { userIdToString :: String }
  deriving (Eq, Ord, Show)

newtype Email
  = Email { emailToString :: String }
  deriving (Eq, Ord, Show)

data User
  = User UserId Email
  deriving (Eq, Ord, Show)

data Method b
  = Delete
  | Get
  | Patch b
  | Post b
  | Put b
  deriving (Eq, Ord, Show)

infixr 4 //
infix  1 ~>
infixr 0 :|

data (//) :: k1 -> k2 -> * where
  S :: a // b

data A :: k -> Symbol -> * where
  A :: A k s

data (:|) :: k1 -> k2 -> * where
  P :: a :| b

data (~>) :: k1 -> k2 -> * where
  R :: a ~> b

type family Impl (t :: k) (m :: * -> *) :: * where
  Impl (t ~> r) m   = ImplTo t (m r)
  Impl (t1 :| t2) m = Impl t1 m :| Impl t2 m
  Impl t m          = ImplTo t (m ())

type family ImplTo (t :: k) (r :: *) :: * where
  ImplTo Delete r                     = r
  ImplTo Get r                        = r
  ImplTo (Patch b) r                  = b -> r
  ImplTo Patch r                      = r
  ImplTo (Post b) r                   = b -> r
  ImplTo Post r                       = r
  ImplTo (Put b) r                    = b -> r
  ImplTo Put r                        = r

  ImplTo (s :: Symbol) r              = r
  ImplTo (A (p :: *) (s :: Symbol)) r = p -> r

  ImplTo (t1 // t2) r                 = ImplTo t1 (ImplTo t2 r)

type Api
  =   Get // "users" ~> [User]
  :|  Get // "users" // A UserId "userId" ~> User
  :|  Post User // "users" ~> User
  :|  Post // "requests"
