module Probability.Random (module Probability.Random,
                           module Range,
                           modifiable)
    where

import Range
import Parameters
import MCMC
import Data.JSON as J
import Effect
import Control.Monad.IO.Class -- for liftIO
import Data.IntMap (IntMap)
import Data.Array (Array)

data SamplingEvent

data AnnotatedDensity a where
    InEdge :: String -> b -> AnnotatedDensity ()
    PropertyEdge :: String -> b -> AnnotatedDensity ()
    ADBind :: AnnotatedDensity b -> (b -> AnnotatedDensity a) -> AnnotatedDensity a
    ADReturn :: a -> AnnotatedDensity a
    ProbFactor :: Double -> AnnotatedDensity ()

in_edge name node = InEdge name node
property name node = PropertyEdge name node


instance Functor AnnotatedDensity where
    fmap f x = ADBind x (\result -> ADReturn (f result))

instance Applicative AnnotatedDensity where
    pure  x = ADReturn x
    f <*> x = ADBind x (\x' -> ADBind f (\f' -> pure (f' x')))

instance Monad AnnotatedDensity where
    f >>= g = ADBind f g


-- Just get the densities out
-- No ProbFactor events yet.
get_densities :: AnnotatedDensity a -> a
get_densities (ADReturn x) = x
get_densities (ADBind f g) = let x = get_densities f in get_densities (g x)
get_densities (InEdge _ x) = ()
get_densities (PropertyEdge _ x) = ()


make_edges :: Effect -> AnnotatedDensity a -> IO a
make_edges event (ADReturn x) = return x
make_edges event (ADBind f g) = do x <- make_edges event f
                                   make_edges event (g x)
make_edges event (InEdge name node) = do register_in_edge node event name
                                         return ()
make_edges event (PropertyEdge name node) = do register_dist_property event node name
                                               return ()

-- Define the Distribution type
data Distribution a = Distribution String (a -> AnnotatedDensity [LogDouble]) (a->Double) (Random a) Range
dist_name (Distribution n _ _ _ _) = n
annotated_densities (Distribution _ ds _ _ _) = ds
densities dist x = get_densities $ annotated_densities dist x
density dist x = balanced_product (densities dist x)
quantile (Distribution _ _ q _ _) = q
sampler (Distribution _ _ _ s _) = s
distRange (Distribution _ _ _ _ r) = r

-- FIXME: We might need GADTS for
--   Independant :: (Random a, Random b) -> Random (a,b)
--   Observe :: b -> (Distribution b) -> Random ()
--   AddMove :: b -> Random ()

-- FIXME: the Random constructor here seems a bit weird.
--        presumably this indicates that its an IO action versus a Random a
--        but its only used inside Distribution...

data TKEffects a = SamplingRate Double (TKEffects a)
                 | TKLiftIO (Double -> IO a)
                 | TKReturn a
                 | forall b . TKBind (TKEffects b) (b -> TKEffects a)

instance Functor TKEffects where
    fmap f x = TKBind x (\result -> TKReturn (f result))

instance Applicative TKEffects where
    pure  x = TKReturn x
    f <*> x = TKBind x (\x' -> TKBind f (\f' -> pure (f' x')))

instance Monad TKEffects where
    return x = TKReturn x
    f >>= g  = TKBind f g

data Random a where
    RandomStructure :: (a->TKEffects b) -> (a -> (a -> IO ()) -> a) -> Random a -> Random a
    RanAtomic :: (a -> TKEffects b) -> IO a -> Random a
    Observe :: Distribution b -> b -> Random ()
    Lazy :: Random a -> Random a
    WithTKEffect :: Random a -> (a -> TKEffects b) -> Random a
    PerformTKEffect :: TKEffects a -> Random a
    RanLiftIO :: IO a -> Random a
    RanReturn :: a -> Random a
    RanBind :: Random b -> (b -> Random a) -> Random a
    RanMFix :: (a -> Random a) -> Random a
    RanDistribution :: Distribution a -> Random a
    RanSamplingRate :: Double -> Random a -> Random a
    RanExchangeable :: Random b -> Random (Random b)

instance Functor Random where
    fmap f r = RanBind r (return . f)

instance Applicative Random where
    pure  x = RanReturn x
    f <*> x = RanBind x (\x' -> RanBind f (\f' -> pure (f' x')))

instance Monad Random where
    f >>= g  = RanBind f g
    mfix f   = RanMFix f

observe dist datum = liftIO $ do
                       s <- register_dist_observe (dist_name dist)
                       register_out_edge s datum
                       density_terms <- make_edges s $ annotated_densities dist datum
                       sequence_ [register_likelihood s term | term <- density_terms]
x ~> dist = observe dist x
infix 0 ~>

instance MonadIO Random where
    liftIO = RanLiftIO

lazy = Lazy
infixl 2 `with_tk_effect`
with_tk_effect = WithTKEffect

do_nothing _ = return ()

run_strict :: Random a -> IO a
run_strict (RanBind f g) = do
  x <- run_strict f
  run_strict $ g x
run_strict (RanReturn v) = return v
run_strict (RanLiftIO a) = a
run_strict (RanSamplingRate _ a) = run_strict a
-- These are the lazily executed parts of the strict monad.
run_strict dist@(RanDistribution _) = run_lazy dist
run_strict e@(WithTKEffect _ _) = run_lazy e
run_strict (RanMFix f) = mfix (run_lazy . f)
run_strict (Lazy r) = unsafeInterleaveIO $ run_lazy r


add_move m = TKLiftIO $ (\rate -> register_transition_kernel rate m)

run_tk_effects :: Double -> TKEffects a -> IO a
run_tk_effects rate (TKBind f g) = do x <- run_tk_effects rate f
                                      run_tk_effects rate $ g x
run_tk_effects rate (TKReturn v) = return v
run_tk_effects rate (TKLiftIO action) = action rate
run_tk_effects rate (SamplingRate rate2 a) = run_tk_effects (rate*rate2) a
-- LiftIO and Print are only here for debugging purposes:
--  run_tk_effects alpha rate (LiftIO a) = a
--  run_tk_effects alpha rate (Print s) = putStrLn (show s)

run_lazy :: Random a -> IO a
run_lazy (RandomStructure _ _ a) = run_lazy a
run_lazy (RanAtomic _ a) = a
run_lazy (RanBind f g) = do
  x <- unsafeInterleaveIO $ run_lazy f
  run_lazy $ g x
run_lazy (RanReturn v) = return v
run_lazy (RanLiftIO a) = a
run_lazy (RanMFix f) = mfix (run_lazy.f)
run_lazy (RanSamplingRate _ a) = run_lazy a
-- Problem: distributions aren't part of the Random monad!
run_lazy (RanDistribution (Distribution _ _ _ a _)) = unsafeInterleaveIO $ run_lazy a
run_lazy (PerformTKEffect e) = run_tk_effects 1.0 e
run_lazy (WithTKEffect action _) = run_lazy action
run_lazy (Lazy a) = run_lazy a
run_lazy (Observe _ _) = error "run_lazy: observe"

random_modifiable dist = io_modifiable $ run_lazy dist

-- Also, shouldn't the modifiable function actually be some kind of monad, to prevent let x=modifiable 0;y=modifiable 0 from merging x and y?

run_strict' :: Double -> Random a -> IO a
run_strict' rate (RanBind f g) = do
  x <- run_strict' rate f
  run_strict' rate $ g x
run_strict' rate (RanReturn v) = return v
run_strict' rate (RanLiftIO io) = io
run_strict' rate (PerformTKEffect e) = run_tk_effects rate e
run_strict' rate (RanSamplingRate rate2 a) = run_strict' (rate*rate2) a
-- These are the lazily executed parts of the strict monad.
run_strict' rate dist@(RanDistribution _) = run_lazy' rate dist
run_strict' rate e@(WithTKEffect _ _) = run_lazy' rate e
run_strict' rate (RanMFix f) = mfix (run_lazy' rate . f)
run_strict' rate (Lazy r) = unsafeInterleaveIO $ run_lazy' rate r

-- NOTE: In order for (run_lazy') to actually be lazy, we need to avoid returning
--       SOMETHING `seq` result.  And this means that we need to frequently
--       intersperse unsafeInterleaveIO to avoid `seq`-ing on previous statements.

triggered_modifiable_structure :: ((forall a.a -> a) -> b -> b) -> (b -> c) -> b -> (b -> IO ()) -> b
triggered_modifiable_structure mod_structure force_structure value effect = triggered_x
    where raw_x       = mod_structure modifiable value
          effect'     = force_structure raw_x `seq` (unsafePerformIO $ effect raw_x)
          triggered_x = mod_structure (effect' `seq`) raw_x

apply_modifier :: (forall a.a -> a) -> b -> b
apply_modifier x y = x y

modifiable_structure :: b -> (b -> IO ()) -> b
modifiable_structure = triggered_modifiable_structure apply_modifier (const ())

sample_effect rate dist tk_effect x = do
  run_tk_effects rate $ tk_effect x
  s <- register_dist_sample (dist_name dist)
  density_terms <- make_edges s $ annotated_densities dist x
  sequence_ [register_prior s term | term <- density_terms]
  register_out_edge s x
  return ()


-- It seems like we could return raw_x in most cases, except the case of a tree.
-- But in the tree case, we could return triggered_x.

-- Note on unsafeInterleaveIO:
--       Simply using run_lazy does not guarantee that the result of run_lazy
--       will not be demanded.  (It guarantees that f >>= g that are INSIDE run_lazy
--       won't demand the result of the f).
--       We need to guard any IO operations with unsafeInterleaveIO if we
--       want to prevent their results from being demanded.
--
-- QUESTION: So, do we need to guard the execution of Distributions with unsafeInterleaveIO?
-- ANSWER: No.  If its not the last entry in a sequence, it will get unsafeInterleaveIO from
--         run_lazy' _ (IOAndPass _ _).
--         If it is run from run_strict' directly, then it is run with
--         unsafeInterleaveIO $ run_lazy', so we get an unsafeInterleaveIO from there.
--
run_lazy' :: Double -> Random a -> IO a
run_lazy' rate (RanLiftIO a) = a
run_lazy' rate (RanBind f g) = do
  x <- unsafeInterleaveIO $ run_lazy' rate f
  run_lazy' rate $ g x
run_lazy' rate (RanReturn v) = return v
run_lazy' rate (RanDistribution dist@(Distribution _ _ _ (RanAtomic tk_effect do_sample) range)) = do
  let x = io_modifiable do_sample
  effect <- sample_effect rate dist tk_effect x
  return (effect `seq` x)
run_lazy' rate (RanDistribution dist@(Distribution _ _ _ (RandomStructure tk_effect structure do_sample) range)) = do
 -- Note: unsafeInterleaveIO means that we will only execute this line if `value` is accessed.
  value <- unsafeInterleaveIO $ run_lazy do_sample
  return $ structure value (sample_effect rate dist tk_effect)

run_lazy' rate (RanDistribution (Distribution _ _ _ s _)) = run_lazy' rate s
run_lazy' rate (RanMFix f) = mfix ((run_lazy' rate).f)
run_lazy' rate (RanSamplingRate rate2 a) = run_lazy' (rate*rate2) a
run_lazy' rate (Lazy r) = run_lazy' rate r
run_lazy' rate (WithTKEffect action tk_effect) = unsafeInterleaveIO $ do
  result <- unsafeInterleaveIO $ run_lazy' rate action
  run_tk_effects rate $ tk_effect result
  return result

gen_model_no_alphabet m = run_strict' 1.0 m
mcmc = gen_model_no_alphabet

add_null_program_result :: IO a -> IO (Maybe b,a)
add_null_program_result p = do result <- p
                               return (Nothing,result)

-- Loggers: we can only log things with the ToJSON property
infix 1 %=%, %>%
name %=% value = (toJSONKey name, toJSON value)
prefix %>% subvalue = (toJSONKey $ prefix ++ "/", log_to_json subvalue)

log_to_json loggers = J.Object $ loggers

-- Define some helper functions
no_quantile name = error ("Distribution '"++name++"' has no quantile function")
make_densities density x = return [density x]
make_densities' densities x = return $ densities x
pair_apply f (x:y:t) = f x y : pair_apply f t
pair_apply _ t       = t

foldt f z []  = z
foldt f _ [x] = x
foldt f z xs  = foldt f z (pair_apply f xs)

balanced_product xs = foldt (*) 1 xs

class ForceFields a where
    force_fields :: a -> a

instance Foldable f => ForceFields (f a) where
    force_fields xs = foldr seq () xs `seq` xs

-- maybe I should rename this to (modifiable_list_n n f value) or something.
mapn n f xs = go 0 where
    go i | i==n      = []
         | otherwise = f (xs!!i):go (i+1)
