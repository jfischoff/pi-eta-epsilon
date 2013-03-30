-- boilerplate {{{1
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-} 
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Language.PiEtaEpsilon.Evaluator where
import Language.PiEtaEpsilon.Types
import Language.PiEtaEpsilon.Grammar
import Control.Applicative
import Control.Monad.Error
import Control.Monad.Logic
import Control.Monad.Identity
import Control.Monad.Trans.Identity
import Control.Monad.Trans
import Control.Unification
import Control.Unification.IntVar
import Data.Function
import Prelude hiding (Either(..), negate)
import qualified Prelude as P
import Data.Default
import Language.PiEtaEpsilon.Sugar

--TODO use lenses and quasiquoter patterns

-- types {{{1
-- UValue, Context, MachineState {{{2


data Context
   = Box
   | Fst  Context Term | Snd  Term Context
   | LSum Context Term | RSum Term Context
   | LProduct Context Term UValue | RProduct Term UValue Context
   deriving (Show)

instance Default Context where 
   def = Box

data MachineState = MachineState { 
     forward     :: Bool
   , descending  :: Bool
   , current     :: Term
   , output      :: UValue
   , context     :: Context
   } deriving (Show)

-- makeLenses ''MachineState

instance Default MachineState where
    def = MachineState {
           forward    = True
         , descending = True
         , current    = def
         , output     = def 
         , context    = def
        }

-- PEET {{{2
newtype PEET m a = PEET { unPEET :: IntBindingT ValueF (LogicT m) a }
type PEE = PEET Identity
deriving instance Functor     (PEET m)
deriving instance Monad       (PEET m)
deriving instance Applicative (PEET m)
deriving instance Alternative (PEET m)
deriving instance BindingMonad ValueF IntVar (PEET m)
instance MonadTrans PEET where lift m = PEET (lift (lift m))
instance MonadError (UnificationFailure ValueF IntVar) (PEET m) where
	throwError _ = empty -- throw away worlds where something doesn't unify
	catchError   = error "catchError undefined for the PEET monad"

runPEET :: Monad m => PEET m a -> m [a]
runPEET = observeAllT . evalIntBindingT . unPEET

runPEE :: PEE a -> [a]
runPEE = runIdentity . runPEET

-- evaluation {{{1
-- misc {{{2
newVariable :: BindingMonad t v m => m (UTerm t0 v)
newVariable = var <$> freeVar

-- The type signatures are scarier than the implementations.  The basic idea is
-- to assert that the v argument has the head form given by f, extract the
-- holes in the head given by f, and apply the new head form given by f' to the
-- values in the holes.  For example, "transform1 left right v" turns (Left v)
-- into (Right v) and fails on any value that definitely doesn't have "Left" at
-- the front.
transform0 f f' v = runIdentityT (v =:= f) >> return f'
transform1 f f' v = newVariable >>= \v' -> transform0 (f v') (f' v') v
transform2 f f' v = newVariable >>= \v' -> transform1 (f v') (f' v') v
transform3 f f' v = newVariable >>= \v' -> transform2 (f v') (f' v') v
transform4 f f' v = newVariable >>= \v' -> transform3 (f v') (f' v') v
transform5 f f' v = newVariable >>= \v' -> transform4 (f v') (f' v') v
transform6 f f' v = newVariable >>= \v' -> transform5 (f v') (f' v') v
transform7 f f' v = newVariable >>= \v' -> transform6 (f v') (f' v') v

tripleL, tripleR :: Particle a => a -> a -> a -> a
tripleL v1 v2 v3 = tuple (tuple v1 v2) v3
tripleR v1 v2 v3 = tuple v1 (tuple v2 v3)

initialize :: Term -> UValue -> MachineState
initialize t v = MachineState {
	forward = True,
	descending = True,
	current = t,
	output = v,
	context = Box
	}

isFinal :: MachineState -> Bool
isFinal (MachineState { forward = True, descending = False, context = Box }) = True
isFinal _ = False

-- evaluation of isomorphisms {{{2
evalIso :: Iso -> UValue -> PEET m UValue
evalIso e v = case e of
   [iso| # <=+=> |] -> transform1 right id v
   [iso| ' <=+=> |] -> transform1 id right v
   [iso| #  x+x  |] -> transform1 left right v 
                   <|> transform1 right left v
   [iso| '  x+x  |] -> transform1 left right v 
                   <|> transform1 right left v
   [iso| # |+|+| |] -> transform1  left           (left . left ) v
                   <|> transform1 (right . left ) (left . right) v
                   <|> transform1 (right . right)  right         v
   [iso| ' |+|+| |] -> transform1 (left . left )  left           v
                   <|> transform1 (left . right) (right . left ) v
                   <|> transform1  right         (right . right) v
   [iso| # -+<   |] -> error "the impossible happened: stepEval passed an eta+ off to evalIso"
   [iso| ' -+<   |] -> error "the impossible happened: stepEval passed an epsilon+ off to evalIso"
   [iso| # <=*=> |] -> transform1 (tuple unit) id v
   [iso| ' <=*=> |] -> transform1 id (tuple unit) v
   [iso| #  x*x  |] -> transform2 tuple (flip tuple) v
   [iso| '  x*x  |] -> transform2 tuple (flip tuple) v
   [iso| # |*|*| |] -> transform3 tripleR tripleL v
   [iso| ' |*|*| |] -> transform3 tripleL tripleR v
   [iso| # -*<   |] -> newVariable >>= \v' -> transform0 unit (tuple (reciprocate v') v') v
   [iso| ' -*<   |] -> newVariable >>= \v' -> transform0 (tuple (reciprocate v') v') unit v
   [iso| # ^0^   |] -> empty
   [iso| ' ^0^   |] -> empty
   [iso| # ^+^   |] -> 
          transform2 (\v1 v3 -> tuple (left  v1) v3) (\v1 v3 -> left  (tuple v1 v3)) v
      <|> transform2 (\v2 v3 -> tuple (right v2) v3) (\v2 v3 -> right (tuple v2 v3)) v
   [iso| ' ^+^   |] -> 
       transform2 (\v1 v3 -> left  (tuple v1 v3)) (\v1 v3 -> tuple (left  v1) v3) v
   <|> transform2 (\v2 v3 -> right (tuple v2 v3)) (\v2 v3 -> tuple (right v2) v3) v

-- evaluation of terms {{{2
stepEval :: MachineState -> PEET m MachineState
stepEval m@(MachineState { forward = True, descending = True }) = case current m of
	[term| < # -+< |] -> empty
	[term| < ' -+< |] -> do
		v <-     transform1 right (left . negate) (output m)
		     <|> transform1 (left . negate) right (output m)
		return m { output = v, forward = False }
	[term| < [: i :] |]  -> do
		v <- evalIso i (output m)
		return m { descending = False, output = v }
	[term| <=>                 |] -> return m { descending = False }
	[term| [: t1 :] ; [: t2 :] |] -> return m { current = t1, context = Fst (context m) t2 }
	[term| [: t1 :] + [: t2 :] |] -> transform1 left  (\v     -> m { current = t1, output = v , context = LSum (context m) t2        }) (output m)
	          <|> transform1 right (\v     -> m { current = t2, output = v , context = RSum t1 (context m)        }) (output m)
	[term| [: t1 :] * [: t2 :] |] -> transform2 tuple (\v1 v2 -> m { current = t1, output = v1, context = LProduct (context m) t2 v2 }) (output m)
stepEval m@(MachineState { forward = True, descending = False }) = case context m of
	Box -> empty
	Fst  cxt t -> return m { descending = True, current = t, context = Snd (current m) cxt }
	Snd  t cxt -> return m { current = t @. current m, context = cxt }
	LSum cxt t -> return m { current = current m @+ t, output = left  (output m), context = cxt }
	RSum t cxt -> return m { current = t @+ current m, output = right (output m), context = cxt }
	LProduct cxt t v -> return m { descending = True, current = t, output = v, context = RProduct (current m) (output m) cxt }
	RProduct t v cxt -> return m { current = t @* current m, output = tuple v (output m), context = cxt }
stepEval m@(MachineState { forward = False, descending = True }) = case context m of
	Box -> empty
	Fst  cxt t -> return m { current = current m @. t, context = cxt }
	Snd  t cxt -> return m { descending = False, current = t, context = Fst cxt (current m) }
	LSum cxt t -> return m { current = current m @+ t, output = left  (output m), context = cxt }
	RSum t cxt -> return m { current = t @+ current m, output = right (output m), context = cxt }
	LProduct cxt t v -> return m { current = current m @* t, output = tuple (output m) v, context = cxt }
	RProduct t v cxt -> return m { descending = False, current = t, output = v, context = LProduct cxt (current m) (output m) }
stepEval m@(MachineState { forward = False, descending = False }) = case current m of
	[term| < # -+<             |] -> do
		v <-     transform1 right (left . negate) (output m)
		     <|> transform1 (left . negate) right (output m)
		return m { output = v, forward = True }
	[term| < ' -+<             |] -> empty
	[term| < [: i :]         |] -> do
		v <- evalIso (adjointIso i) (output m)
		return m { descending = True, output = v }
	[term| <=>                 |] -> return m { descending = True }
	[term| [: t1 :] ; [: t2 :] |] -> return m { current = t2, context = Snd t1 (context m) }
	[term| [: t1 :] + [: t2 :] |] ->  transform1 left  (\v     -> m { current = t1, output = v , context = LSum (context m) t2        }) (output m)
	         <|> transform1 right (\v     -> m { current = t2, output = v , context = RSum t1 (context m)        }) (output m)
	[term| [: t1 :] * [: t2 :] |] ->  transform2 tuple (\v1 v2 -> m { current = t2, output = v2, context = RProduct t1 v1 (context m) }) (output m)

-- drivers {{{2
eval :: MachineState -> PEET m MachineState
eval m
	| isFinal m = freeze' (output m) >>= \v -> return m { output = v }
	| otherwise = stepEval m >>= eval
	where freeze' = runIdentityT . applyBindings

runEval :: MachineState -> [UValue]	
runEval = map output . runPEE . eval	
	
topLevelWithState :: MachineState -> Term -> UValue -> [UValue]
topLevelWithState m t v = runEval $ m { current = t, output = v }

topLevel :: Term -> UValue -> [UValue]
topLevel t v = runEval $ initialize t v

nSteps :: Term -> UValue -> Int -> [(MachineState, IntBindingState ValueF)]
nSteps t v n = observeAll . runIntBindingT . unPEET $ do
	m <- (iterate (stepEval >=>) return !! n) (initialize t v)
	v <- runIdentityT . applyBindings . output $ m
	return m { output = v }

