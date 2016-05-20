import Control.Monad.Resumption.Reactive
import Control.Monad.Identity
import Data.Word

type ReT = ReacT
--type I = Identity
type I = IO

nativeVhdl :: String -> a -> a
nativeVhdl _ x = x

data Bit = Zero | One deriving Show
--data W8  = W8 Bit Bit Bit Bit Bit Bit Bit Bit
type W8 = Word8

plusW8 :: W8 -> W8 -> W8
{-# INLINE plusW8 #-}
--plusW8 = nativeVhdl "plusW8" plusW8
plusW8 = nativeVhdl "plusW8" (+)

zeroW8 :: W8
--zeroW8 = W8 Zero Zero Zero Zero Zero Zero Zero Zero
zeroW8 = 0

oneW8 :: W8
--oneW8 = W8 Zero Zero Zero Zero Zero Zero Zero One
oneW8 = 1

data R = R_k W8 W8

start_pure :: Either () (W8,R)
start_pure = begin_pure

begin_pure :: Either () (W8,R)
begin_pure = loop_pure zeroW8 oneW8

loop_pure :: W8 -> W8 -> Either () (W8,R)
loop_pure n m = (Right (n,R_k n m))

k_pure :: W8 -> W8 -> Bit -> Either () (W8,R)
k_pure n m b = case b of
                 One  -> loop_pure n m
                 Zero -> loop_pure m (plusW8 n m)

init :: ReT Bit W8 I ()
init = do let res = start_pure
          case res of
            Left x      -> return x
            Right (o,r) -> do i <- signal o
                              loop r i

loop :: R -> Bit -> ReT Bit W8 I ()
loop r i = case r of
             R_k n m -> do
                          let res = k_pure n m i
                          case res of
                            Left x       -> return x
                            Right (o,r') -> do i' <- signal o
                                               loop r' i'