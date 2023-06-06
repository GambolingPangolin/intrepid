{-# LANGUAGE LambdaCase #-}

module Intrepid (
    AppT,

    -- * Building
    match,
    static,
    combine,
    combineMany,
    vmap,

    -- * Running
    runAppT,
    update,

    -- * Inspecting
    view,
) where

import Control.Applicative ((<|>))
import Control.Monad (unless)
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.List (foldl')

data AppF e v a = Match v (e -> Maybe a)

instance Functor (AppF e v) where
    fmap f (Match v g) = Match v (fmap f . g)

-- | The application monad.  This monad consumes a stream of events and produces a stream of views.
data AppT event view m a
    = Pure a
    | Lift (m (AppT event view m a))
    | Free (AppF event view (AppT event view m a))

instance (Functor m) => Functor (AppT e v m) where
    fmap f = \case
        Pure a -> Pure $ f a
        Lift ma -> Lift $ fmap f <$> ma
        Free ff -> Free $ fmap f <$> ff

instance (Applicative m) => Applicative (AppT e v m) where
    pure = Pure
    Pure f <*> y = f <$> y
    Lift mf <*> Pure x = Lift $ (<*> pure x) <$> mf
    Lift mf <*> Lift mx = Lift $ (<*>) <$> mf <*> mx
    ff <*> Free fx = Free $ (ff <*>) <$> fx
    Free ff <*> fx = Free $ (<*> fx) <$> ff

instance (Monad m) => Monad (AppT e v m) where
    return = pure
    Pure x >>= f = f x
    Lift mx >>= f = Lift $ (>>= f) <$> mx
    Free fx >>= f = Free $ (>>= f) <$> fx

instance MonadTrans (AppT e v) where
    lift = Lift . fmap Pure

-- | Handle a single event
update :: (Monad m) => AppT e v m a -> e -> m (AppT e v m a)
update app e = case app of
    Free (Match _ k) | Just nextApp <- k e -> pure nextApp
    Lift mx -> mx >>= (`update` e)
    _ -> pure app

-- | Simple framework for running an intrepid app
runAppT ::
    (Monad m) =>
    -- | Event polling action where 'Nothing' represents the end of the event stream
    m (Maybe e) ->
    -- | View handler
    (Maybe v -> m ()) ->
    -- | The app to run
    AppT e v m () ->
    -- | The app program in the base monad
    m ()
runAppT getEvent putView = run
  where
    run currApp = mapM_ (onEvent currApp) =<< getEvent
    onEvent currApp e = do
        (thisView, nextApp) <- view =<< update currApp e
        putView thisView
        unless (null thisView) $ run nextApp

{- | Attach an event listener to a view value.  The resulting application has
 the specified view and blocks until the match function returns 'Just'.
-}
match :: (Applicative m) => v -> (e -> Maybe a) -> AppT e v m a
match v handleE = Free $ Match v (fmap pure . handleE)

-- | Create an app with the given view that ignores all events
static :: (Applicative m) => v -> AppT e v m a
static v = match v $ const Nothing

-- | Modify all views within the application
vmap :: (Functor m) => (v1 -> v2) -> AppT e v1 m a -> AppT e v2 m a
vmap f app = case app of
    Free (Match v k) -> Free . Match (f v) $ fmap (vmap f) . k
    Lift mx -> Lift $ vmap f <$> mx
    Pure x -> Pure x

{- | Merge two applications by combining their views using the given operation.
 The application blocks until one of the input applications returns, biasing
 left.
-}
combine ::
    (Applicative m) =>
    (v -> v -> v) ->
    AppT e v m a ->
    AppT e v m b ->
    AppT e v m (Either a b)
combine _ (Pure x) _ = Pure $ Left x
combine _ _ (Pure y) = Pure $ Right y
combine binop (Lift mx) appB =
    Lift $ combine binop <$> mx <*> pure appB
combine binop appA (Lift my) =
    Lift $ combine binop appA <$> my
combine binop (Free (Match vA kA)) (Free (Match vB kB)) =
    Free $ Match (vA `binop` vB) k
  where
    k e = fmap Left <$> kA e <|> fmap Right <$> kB e

{- | Use the specified combining function to merge a collection of
 subapplications.  Combining an empty collection results in a static application
 with view @mergeFunction mempty@.
-}
combineMany :: (Applicative m) => ([v] -> v) -> [AppT e v m a] -> AppT e v m a
combineMany merge apps
    | x0 : xs <- vmap pure <$> apps = vmap merge $ foldl' step x0 xs
    | otherwise = static $ merge mempty
  where
    step accum x = either id id <$> combine (<>) accum x

-- | Run the application until a view is available, if possible.
view ::
    (Monad m) =>
    AppT e v m a ->
    -- | View and remaining application logic
    m (Maybe v, AppT e v m a)
view = \case
    app@(Free (Match v _)) -> pure (Just v, app)
    Lift mapp -> mapp >>= view
    app@Pure{} -> pure (Nothing, app)
