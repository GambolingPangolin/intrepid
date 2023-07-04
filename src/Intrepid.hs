{-# LANGUAGE LambdaCase #-}

module Intrepid (
    AppT,

    -- * Building
    match,
    attach,
    matchManyWith,
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
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (catMaybes)
import Data.These (These (..), these)

-- | The application monad.  This monad consumes a stream of events and produces a stream of views.
data AppT event view m a
    = Pure a
    | Lift (m (AppT event view m a))
    | Match view (event -> Maybe (AppT event view m a))

instance (Functor m) => Functor (AppT e v m) where
    fmap f = \case
        Pure a -> Pure $ f a
        Lift ma -> Lift $ fmap f <$> ma
        Match v next -> Match v $ (fmap . fmap) f . next

instance (Applicative m) => Applicative (AppT e v m) where
    pure = Pure
    Pure f <*> y = f <$> y
    Lift mf <*> ax = Lift $ (<*> ax) <$> mf
    Match v next <*> ax = Match v $ fmap (<*> ax) . next

instance (Monad m) => Monad (AppT e v m) where
    return = pure
    Pure x >>= f = f x
    Lift mx >>= f = Lift $ (>>= f) <$> mx
    Match v next >>= f = Match v $ fmap (>>= f) . next

instance MonadTrans (AppT e v) where
    lift = Lift . fmap Pure

instance (MonadIO m) => MonadIO (AppT e v m) where
    liftIO = lift . liftIO

-- | Handle a single event
update :: (Monad m) => AppT e v m a -> e -> m (AppT e v m a)
update app e = case app of
    Match _ k | Just nextApp <- k e -> pure nextApp
    Lift mx -> mx >>= (`update` e)
    _ -> pure app

-- | Simple framework for running an intrepid app
runAppT ::
    (Monad m) =>
    -- | Event polling action where 'Nothing' represents the end of the event stream
    m (Maybe e) ->
    -- | View handler.  'Nothing' represents the end of the view stream.
    (Maybe v -> m ()) ->
    -- | The app to run
    AppT e v m () ->
    -- | The app program in the base monad
    m ()
runAppT getEvent putView = run
  where
    run currApp = mapM_ (onEvent currApp) =<< getEvent
    onEvent currApp e =
        update currApp e >>= view >>= \case
            Right{} -> putView Nothing
            Left (v, nextApp) -> putView (Just v) >> run nextApp

{- | Attach an event listener to a view value.  The resulting application has
 the specified view and blocks until the match function returns 'Just'.
-}
match :: (Applicative m) => v -> (e -> Maybe a) -> AppT e v m a
match v handleE = Match v (fmap pure . handleE)

-- | Attach a new event handler to an existing application.
attach ::
    (Applicative m) =>
    AppT e v m a ->
    (e -> Maybe b) ->
    AppT e v m (These a b)
attach app kB = case app of
    Match v kA ->
        Match v $
            fmap
                ( \case
                    This next -> attach next kB
                    That b -> pure $ That b
                    These (Pure a) b -> pure $ These a b
                    These _ b -> pure $ That b
                )
                . theseEvents kA kB
    Lift mx -> Lift $ flip attach kB <$> mx
    Pure x -> Pure $ This x

-- | Combine many event filters with a binary operation
matchManyWith ::
    (a -> a -> a) ->
    [e -> Maybe a] ->
    e ->
    Maybe a
matchManyWith op = fmap (foldl' step Nothing . catMaybes) . sequenceA
  where
    step acc x = op <$> acc <*> pure x <|> Just x

theseEvents ::
    (e -> Maybe a) ->
    (e -> Maybe b) ->
    e ->
    Maybe (These a b)
theseEvents kA kB e =
    These <$> kA e <*> kB e
        <|> This <$> kA e
        <|> That <$> kB e

-- | Create an app with the given view that ignores all events
static :: (Applicative m) => v -> AppT e v m a
static v = match v $ const Nothing

-- | Modify all views within the application
vmap :: (Functor m) => (v1 -> v2) -> AppT e v1 m a -> AppT e v2 m a
vmap f app = case app of
    Match v k -> Match (f v) $ fmap (vmap f) . k
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
    AppT e v m (These a b)
combine op = \cases
    (Pure x) (Pure y) -> Pure $ These x y
    (Pure x) _ -> Pure $ This x
    _ (Pure y) -> Pure $ That y
    (Lift mx) appB ->
        Lift $ combine op <$> mx <*> pure appB
    appA (Lift my) ->
        Lift $ combine op appA <$> my
    appA@(Match vA kA) appB@(Match vB kB) ->
        Match (vA `op` vB) k
      where
        k e =
            combine op <$> kA e <*> kB e
                <|> combine op <$> kA e <*> pure appB
                <|> combine op appA <$> kB e

-- | Use the specified combining function to merge a collection of subapplications.
combineMany ::
    (Applicative m) =>
    -- | Combine views
    (v -> v -> v) ->
    -- | Combine values when two components produce events
    (a -> a -> a) ->
    NonEmpty (AppT e v m a) ->
    AppT e v m a
combineMany opV opA (x0 :| xs) = foldl' step x0 xs
  where
    step accum x = these id id opA <$> combine opV accum x

{- | Run the application until a view is available or the application exits with
a value.
-}
view ::
    (Monad m) =>
    AppT e v m a ->
    -- | View and remaining application logic
    m (Either (v, AppT e v m a) a)
view = \case
    app@(Match v _) -> pure $ Left (v, app)
    Lift mapp -> mapp >>= view
    Pure x -> pure $ Right x
