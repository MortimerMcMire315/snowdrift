{- REFACTOR NOTES

- This module should not depend on Import!

- In general (but not always), I am importing this module qualified for
  now as an explicit reminder. Eventually it should be unqualified like most
  internal modules.

- moneyInfo

SqlPersistT will hopefully go away and somehow refer to the main site.

Maybe should probably also go away, since the caller can call
(sequence . fmap) themselves. I actually set out to do that, but got
clobbered with a type mismatch between "DB" and "ReaderT SqlBackend
..." — types that are actually the same.

- Project and User

These will have mechanism-specific information, including mapping to
the master project's entities.

- fetchUserPledgesDB

Cut out with a chainsaw; please excuse the mess.

- userBalance

A bad name for a bad function, I would say. As usual, with everything else
here, it all needs to be redesigned into a consistent interface.

- incrementBalance

This function holds the business logic of a balance cap, but not of a
minimum deposit being 10 bucks. My rationale for the split is that the
former must be decided using internal information (like the index of an
array), but the latter is decided using user input. The idea is to push
verification closer to the source.

-}

module Mechanism where

import Import hiding (Project, User, Account)
import qualified Import as Fixme

import Control.Error
-- Probably shouldn't need Control.Exception; use whatever comes with
-- Control.Error.
import Control.Exception
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Resource (MonadThrow)
import Control.Monad.Trans.Writer.Strict (tell)
import Data.Monoid (Sum(..))
import Data.Time.Clock (addUTCTime)
import Data.Typeable (Typeable)
import qualified Control.Monad.Trans.Writer as Lazy
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Database.Persist as P

import Model.Currency
import WrappedValues

data Project = Project
data User = User

moneyInfo :: MonadIO m
          => Maybe (Entity Fixme.User)
          -> SqlPersistT m (Maybe (Milray, Milray))
moneyInfo = sequence . fmap go
  where
    go (Entity user_id user) = do
      (pledges, balance) <- goDB user_id user
      let pledged = getSum $ foldMap
              (\(project, pledge) ->
                  (Sum
                   . (projectShareValue (entityVal project) $*)
                   . fromIntegral
                   . pledgeFundedShares
                   . entityVal) pledge)
              pledges
      return (balance, pledged)
    goDB user_id user = do
        pledges :: [(Entity Fixme.Project, Entity Pledge)] <- select $ from $
            \(project `InnerJoin` pledge) -> do
                on_ $ pledge ^. PledgeProject ==. project ^. ProjectId
                where_ $ pledge ^. PledgeUser ==. val user_id
                return (project, pledge)
        Just account <- get (userAccount user)
        return
            ( pledges
            , accountBalance account)

fetchUserPledgesDB :: UserId -> DB [(Entity Fixme.Project, Project)]
fetchUserPledgesDB _user_id = return []

payoutHistory :: Fixme.Project
              -> UTCTime
              -> DB (Maybe (Milray, Milray, Milray))
payoutHistory project now = case projectLastPayday project of
    Nothing -> return Nothing
    Just last_payday -> do
        let extractRational = \case
                [Value (Just (r :: Rational))] -> r
                _                              -> 0
        -- This assumes there were transactions associated with the
        -- last payday
        last <- fmap extractRational $
            select $
            from $ \transaction -> do
            where_ $
                transaction ^. TransactionPayday ==. val (Just last_payday) &&.
                transaction ^. TransactionCredit ==. val (Just $ projectAccount project)
            return $ sum_ $ transaction ^. TransactionAmount

        year <- fmap extractRational $
            select $
            from $ \(transaction `InnerJoin` payday) -> do
            where_ $
                payday ^. PaydayDate >. val (addUTCTime (-365 * 24 * 60 * 60) now) &&.
                transaction ^. TransactionCredit ==. val (Just $ projectAccount project)
            on_ $ transaction ^. TransactionPayday ==. Fixme.just (payday ^. PaydayId)
            return $ sum_ $ transaction ^. TransactionAmount

        total <- fmap extractRational $
            select $
            from $ \transaction -> do
            where_ $ transaction ^. TransactionCredit ==. val (Just $ projectAccount project)
            return $ sum_ $ transaction ^. TransactionAmount

        return $ Just (Milray $ round last, Milray $ round year, Milray $ round total)

fetchUser :: Fixme.UserId -> DB User
fetchUser = const (pure User)

fetchProject :: Fixme.ProjectId -> DB Project
fetchProject = const (pure Project)

potentialPledge :: Fixme.UserId
                -> Fixme.ProjectId
                -> Int64
                -> DB  ( Maybe (Entity Pledge)
                       , Milray
                       , Milray
                       , Milray
                       , Milray
                       , Integer
                       )
potentialPledge user_id project_id new_user_shares = do
    pledges <- fetchProjectSharesDB project_id
    mpledge <- getBy $ UniquePledge user_id project_id
    other_shares <- fmap unwrapValues $ select $ from $ \p -> do
        where_ $ p ^. PledgeProject ==. val project_id
            &&. p ^. PledgeUser !=. val user_id
        return $ p ^. PledgeShares
    let old_user_shares = maybe 0 (pledgeShares . entityVal) mpledge

        numPatrons = toInteger $ length pledges

        new_project_shares = filter (>0) [new_user_shares] ++ other_shares

        old_project_shares = filter (>0) [old_user_shares] ++ other_shares

        new_share_value = projectComputeShareValue new_project_shares
        old_share_value = projectComputeShareValue old_project_shares

        new_user_amount = new_share_value $* fromIntegral new_user_shares
        old_user_amount = old_share_value $* fromIntegral old_user_shares

        new_project_amount =
            new_share_value $* fromIntegral (sum new_project_shares)
        old_project_amount =
            old_share_value $* fromIntegral (sum old_project_shares)

    return ( mpledge
           , old_user_amount
           , new_user_amount
           , old_project_amount
           , new_project_amount
           , numPatrons
           )

fetchProjectSharesDB :: ( MonadThrow m
                        , MonadIO m
                        , MonadBaseControl IO m
                        , MonadLogger m
                        , MonadResource m)
                     => ProjectId -> SqlPersistT m [Int64]
fetchProjectSharesDB project_id = do
    pledges <-
        select $
        from $ \pledge -> do
        where_ $
            pledge ^. PledgeProject ==. val project_id
            &&. pledge ^. PledgeFundedShares >. val 0
        return pledge
    return $ map (pledgeFundedShares . entityVal) pledges

-- signature needs to remain generic, for SnowdriftProcessPayments
updateShareValue
    :: ( MonadBaseControl IO m
       , MonadLogger m
       , MonadResource m)
    => ProjectId
    -> SqlPersistT m ()
updateShareValue project_id = do
    pledges <- fetchProjectSharesDB project_id
    update $ \project -> do
        set project
            [ ProjectShareValue =. val (projectComputeShareValue pledges) ]
        where_ (project ^. ProjectId ==. val project_id)

-- | Project's monthly share value: 0.1¢ × number of patrons.
projectComputeShareValue :: [Int64] -> Milray
projectComputeShareValue patronPledgeLevel =
    Milray 10 $* (fromIntegral $ length $ filter (/= 0) patronPledgeLevel)

-- | Keep dropping shares, until there are no underfunded patrons.
-- (Recursion alert.)
dropAllUnderfunded :: DBConstraint m
                   => ProjectId -> Lazy.WriterT DropShares (SqlPersistT m) ()
dropAllUnderfunded projId = do
    -- Update share value before each run.
    lift $ updateShareValue projId
    unders <- lift $ decrementUnderfunded projId
    unless (null unders) $ do
        Lazy.tell unders
        dropAllUnderfunded projId

newtype DropShare = DropShare PledgeId
type DropShares = [DropShare]

-- | Drop one share from each pledge.
dropShares :: MonadIO m => [PledgeId] -> SqlPersistT m ()
dropShares [] = return ()
dropShares ps =
    update $ \p -> do
    set p [ PledgeFundedShares -=. val 1 ]
    where_ $ p ^. PledgeId `in_` valList ps

-- | Find pledges in a given project (if supplied), from a given set of
-- users, that have the greatest number of shares (greater than 0)
maxShares :: (MonadIO m, Functor m)
          => Maybe ProjectId
          -> [UserId]
          -> SqlPersistT m [PledgeId]
maxShares _     []   = return []
maxShares mproj uids = do
    -- select...max_ :: m [Value (Maybe a)]
    [Value mmaxCt] <-
        select $
        from $ \p -> do
        where_ $ (p ^. PledgeUser `in_` valList uids)
            &&. p ^. PledgeFundedShares >. val 0
        return $ max_ $ p ^. PledgeFundedShares

    case mmaxCt of
        Nothing -> return []
        Just maxCt -> do
            let projConstraint pledge = case mproj of
                    Just proj -> pledge ^. PledgeProject ==. val proj
                    _         -> val True

            fmap unwrapValues $
                select $
                from $ \p -> do
                where_ $ (p ^. PledgeUser `in_` valList uids)
                    &&. projConstraint p
                    &&. p ^. PledgeFundedShares ==. val maxCt
                return $ p ^. PledgeId

-- | Find underfunded patrons.
underfundedPatrons :: (MonadIO m, Functor m)
                   => SqlPersistT m [UserId]
underfundedPatrons = do
    -- :: DB [(UserId, Milray, Int64)]
    pledgeList <- fmap unwrapValues $
        select $
        from $ \(usr `InnerJoin` plg `InnerJoin` prj) -> do
        on_ $ prj ^. ProjectId  ==. plg ^. PledgeProject
        on_ $ plg ^. PledgeUser ==. usr ^. UserId
        return
            ( usr ^. UserId
            , prj ^. ProjectShareValue
            , plg ^. PledgeFundedShares
            )

    let uids = map (\(i,_,_) -> i) pledgeList

    -- :: DB (Map UserId Milray)
    balances <- fmap (M.fromList . unwrapValues) $
        select $
        from $ \(u `InnerJoin` a) -> do
        on_ $ u ^. UserAccount ==. a ^. AccountId
        where_ $ u ^. UserId `in_` valList uids
        return (u ^. UserId, a ^. AccountBalance)

    -- Sum outlays over the pledge list.
    let userOutlays :: M.Map UserId Milray
        userOutlays = getSum <$> foldMap outlaySum pledgeList

    -- Filter out non-negative (balance - outlay) and return
    return $ M.keys $ M.differenceWith maybeNegSubtract balances userOutlays

  where

    -- | Create something with a summable type.
    outlaySum :: (UserId, Milray, Int64) -> M.Map UserId (Sum Milray)
    outlaySum (u, shareValue, fundedShares) =
        M.singleton u (Sum $ fromIntegral fundedShares *$ shareValue)

    -- | Given "a - b", return just the absolute value (≡ b - a) if the
    -- difference is negative.
    maybeNegSubtract :: (Ord s, Num s) => s -> s -> Maybe s
    maybeNegSubtract a b
        | a < b     = Just $ b - a
        | otherwise = Nothing

-- | Drop one share from each highest-shared underfunded pledges to a
-- particular project, and update the project share value. Return which
-- ones got dropped.
decrementUnderfunded :: ProjectId -> DB DropShares
decrementUnderfunded projId = do
    droppers <- join $ maxShares (Just projId) <$> underfundedPatrons
    dropShares droppers
    return $ map DropShare droppers

-- | Fold some DropShares into EventDeactivatedPledges, one per affected
-- pledge.
--
-- To be implemented for SD-603.
foldDrops :: UTCTime -> DropShares -> [SnowdriftEvent]
foldDrops _ts = map snd . toList . foldr insertOrAdd M.empty
  where
    insertOrAdd = flip const

-- | After a patron modifies their pledge, some other patrons may be
-- underfunded. This method deactivates shares from those underfunded
-- pledges until all pledges are funded.
rebalanceProjectPledges :: ProjectId -> SYDB ()
rebalanceProjectPledges project_id = do
    allDrops <- lift . Lazy.execWriterT $ dropAllUnderfunded project_id
    now <- liftIO getCurrentTime
    tell $ foldDrops now allDrops

updateUserPledge :: Text -> Int64 -> HandlerT App IO ()
updateUserPledge project_handle shares = do
    Just pledge_render_id <-
        fmap (read . T.unpack) <$> lookupSession pledgeRenderKey

    status <- runSYDB $ do
        Entity user_id user <- lift (lift requireAuth)
        Just account <- lift $ get (userAccount user)
        Entity project_id project <-
            lift $ getBy404 (UniqueProjectHandle project_handle)
        mold_shares <- lift $ do
            mpledge <- getBy $ UniquePledge user_id project_id
            return $ case mpledge of
                Nothing                -> Nothing
                Just (Entity _ pledge) -> Just (pledgeFundedShares pledge)
        let mnew_shares  = if shares == 0 then Nothing else Just shares
            user_outlay  = projectShareValue project $* fromIntegral shares
            enough_money = accountBalance account >= user_outlay $* 3
            -- At the time of writing this comment, pledging
            -- multiple times breaks 'renderProject' and
            -- 'SnowdriftProcessPayments'.  In any case, there
            -- is no need to allow pledging the same amount
            -- multiple times ever.
            new_amount   = mold_shares /= mnew_shares

            pledge_dropped   = "You have dropped your pledge and are no longer "
                            <> "a patron of " <> projectName project <> "."
            pledge_updated   = "You have now pledged a base rate of "
                            <> (T.pack $ show $ millMilray shares)
                            <> " per patron. "
                            <> "Thank you for supporting "
                            <> projectName project <> "!"
            same_amount      = "you cannot pledge the same amount"
            not_enough_money = "you must have funds to support your pledge "
                            <> "for at least 3 months at current pledge value. "
                            <> "Please deposit additional funds to your account"
            status = case (enough_money, new_amount) of
                (True, True)   ->
                    if shares == 0
                        then Right pledge_dropped
                        else Right pledge_updated
                (True, False)  ->
                    Left $ "Sorry, " <> same_amount <> "."
                (False, True)  ->
                    Left $ "Sorry, " <> not_enough_money <> "."
                (False, False) ->
                    Left $ "Sorry, " <> same_amount <> " and "
                        <> not_enough_money <> "."
            success = isRight status

        when success $ do
            insertProjectPledgeDB
                user_id
                project_id
                shares
                pledge_render_id
            rebalanceProjectPledges project_id

        return status

    case status of
        Right msg -> alertSuccess msg
        Left  msg -> alertWarning msg

pledgeRenderKey :: Text
pledgeRenderKey = "pledge_render"

insertProjectPledgeDB :: UserId
                      -> ProjectId
                      -> Int64
                      -> PledgeFormRenderedId
                      -> SDB ()
insertProjectPledgeDB user_id project_id shares pledge_render_id = do
    now <- liftIO getCurrentTime
    let shares_pledged =
            SharesPledged now user_id project_id shares pledge_render_id
    shares_pledged_id <- lift (insert shares_pledged)
    lift (getBy (UniquePledge user_id project_id)) >>= \case
        Nothing -> do
            lift $ insert_ (Pledge now user_id project_id shares shares)
            tell [ENewPledge shares_pledged_id shares_pledged]
        Just (Entity pledge_id old_pledge) -> do
            if shares == 0
                then do
                    lift (deleteKey pledge_id)
                    tell [EDeletedPledge now
                                         user_id
                                         project_id
                                         (pledgeShares old_pledge)]
                else do
                    lift $
                        update $ \p -> do
                        set p [ PledgeShares       =. val shares
                              , PledgeFundedShares =. val shares
                              ]
                        where_ (p ^. PledgeId ==. val pledge_id)
                    tell [EUpdatedPledge (pledgeShares old_pledge)
                                         shares_pledged_id
                                         shares_pledged]


userBalance :: Fixme.User
            -> Int64
            -> Int64
            -> DB ( [Entity Transaction]
                  , Map (Key Fixme.Account) (Entity Fixme.User)
                  , Map (Key Fixme.Account) (Entity Fixme.Project))
userBalance user limit' offset' = do
    tx <- select $ from $ \transaction -> do
            where_ ( transaction ^. TransactionCredit ==. val (Just (userAccount user))
                    ||. transaction ^. TransactionDebit ==. val (Just (userAccount user)))
            orderBy [ desc (transaction ^. TransactionTs) ]
            limit limit'
            offset offset'
            return transaction

    let tx' = map entityVal tx
        accounts =
            catMaybes
                (setNub
                    (map transactionCredit tx' ++ map transactionDebit tx'))

    users <- selectList [ UserAccount <-. accounts ] []
    projects <- selectList [ ProjectAccount <-. accounts ] []

    let mkMapBy :: Ord b => (a -> b) -> [a] -> M.Map b a
        mkMapBy f = M.fromList . map (\e -> (f e, e))

    return
        ( tx
        , mkMapBy (userAccount . entityVal) users
        , mkMapBy (projectAccount . entityVal) projects
        )

setNub :: Ord a => [a] -> [a]
setNub = S.toList . S.fromList

incrementBalance :: MonadIO m
                 => Fixme.User
                 -> UTCTime
                 -> Milray
                 -> Fixme.SqlPersistT m (Either Text ())
incrementBalance user now amt = do
    c <- updateCount $ \account -> do
        set account [ AccountBalance +=. val amt ]
        where_ $ account ^. AccountId ==. val (userAccount user)
            &&. account ^. AccountBalance +. val amt <=. val balanceCap

    when (c == 1) $ do
        insert_
            (Transaction now
                         (Just $ userAccount user)
                         Nothing
                         Nothing
                         amt
                         "Test Load"
                         Nothing)
    return $ if (c == 1)
        then (Right ())
        else (Left emsg)
  where
    balanceCap :: Milray
    balanceCap = 1000000
    emsg = "Balance would exceed (testing only) cap of " <>
            T.pack (show balanceCap)

data PledgeStatus = AllFunded | ExistsUnderfunded

pledgeStatus :: UserId -> DB PledgeStatus
pledgeStatus uid = do
    underfunded <-
        select $
        from $ \u -> do
        where_ $ u ^. UserId ==. val uid
            &&. (exists $
                from $ \plg -> do
                where_ $ plg ^. PledgeShares >. plg ^. PledgeFundedShares
                    &&. plg ^. PledgeUser ==. val uid)
        return $ u ^. UserId
    return $ case underfunded of
        [] -> AllFunded
        _ -> ExistsUnderfunded

projectEvents :: MonadIO m
              => Fixme.ProjectId
              -> UTCTime
              -> Int64
              -> SqlPersistT m ( [(Fixme.EventNewPledgeId, Entity Fixme.SharesPledged)]
                               , [(Fixme.EventUpdatedPledgeId, Int64, Entity Fixme.SharesPledged)]
                               , [(Fixme.EventDeletedPledgeId, Fixme.EventDeletedPledge)]
                               , [Fixme.UserId]
                               , [Fixme.UserId])
projectEvents project_id before lim = do
    -- Fetch all new SharesPledged made on this Project before this time.
    new_pledge_events <- fmap unwrapValues $ select $ from $ \(enp `InnerJoin` sp) -> do
        on_ $ enp ^. EventNewPledgeSharesPledged ==. sp ^. SharesPledgedId
        where_ $ enp ^. EventNewPledgeTs <=. val before
            &&. sp ^. SharesPledgedProject ==. val project_id
        orderBy [ desc $ enp ^. EventNewPledgeTs
                , desc $ enp ^. EventNewPledgeId ]
        limit lim
        return (enp ^. EventNewPledgeId, sp)

    -- Fetch all updated Pledges made on this Project before this time,
    -- along with the old number of shares.
    updated_pledge_events <- fmap unwrapValues $ select $ from $ \(eup `InnerJoin` sp) -> do
        on_ $ eup ^. EventUpdatedPledgeSharesPledged ==. sp ^. SharesPledgedId
        where_ $ eup ^. EventUpdatedPledgeTs <=. val before
            &&. sp ^. SharesPledgedProject ==. val project_id
        orderBy [ desc $ eup ^. EventUpdatedPledgeTs
                , desc $ eup ^. EventUpdatedPledgeId ]
        limit lim
        return
            ( eup ^. EventUpdatedPledgeId
            , eup ^. EventUpdatedPledgeOldShares, sp)

    -- Fetch all deleted pledge events made on this Project before this
    -- time.
    deleted_pledge_events <- fmap (map $ onEntity (,)) $ select $ from $ \edp -> do
        where_ $ edp ^. EventDeletedPledgeTs <=. val before
            &&. edp ^. EventDeletedPledgeProject ==. val project_id

        orderBy [ desc $ edp ^. EventDeletedPledgeTs
                , desc $ edp ^. EventDeletedPledgeId ]
        limit lim
        return edp


    let shares_pledged =
            mappend (map (entityVal . snd) new_pledge_events)
                    (map (\(_, _, x) -> entityVal x) updated_pledge_events)
        pledging_users = map sharesPledgedUser shares_pledged
        unpledging_users = map (eventDeletedPledgeUser . snd)
                               deleted_pledge_events

    return ( new_pledge_events
           , updated_pledge_events
           , deleted_pledge_events
           , pledging_users
           , unpledging_users)


projectTransactions :: MonadIO m
                    => Text
                    -> SqlPersistT m ( Fixme.Project
                                     , Fixme.Account
                                     , Map Fixme.AccountId
                                           (Either (Entity Fixme.Project)
                                                   (Entity Fixme.User))
                                     , [( Maybe (Entity Payday)
                                        , [Entity Transaction])])
projectTransactions project_handle = do
    Entity _ project :: Entity Fixme.Project <-
        getBy404 $ UniqueProjectHandle project_handle

    account <- get404 $ projectAccount project

    transactions <- select $ from $ \t -> do
        where_ $
            t ^. TransactionCredit ==. val (Just $ projectAccount project)
                ||. t ^. TransactionDebit ==.
                        val (Just $ projectAccount project)

        orderBy [ desc $ t ^. TransactionTs ]
        return t

    let accounts =
            setNub
                (concatMap
                    (\(Entity _ t) ->
                        maybeToList (transactionCredit t) <>
                            maybeToList (transactionDebit t))
                    transactions)

    users_by_account <-
        fmap (M.fromList . map (userAccount . entityVal &&& Right))
             (select $ from $ \u -> do
                  where_ $ u ^. UserAccount `in_` valList accounts
                  return u)

    projects_by_account <-
        fmap (M.fromList . map (projectAccount . entityVal &&& Left))
             (select $ from $ \p -> do
                  where_ $ p ^. ProjectAccount `in_` valList accounts
                  return p)

    let account_map = projects_by_account `M.union` users_by_account

    payday_map <-
        fmap (M.fromList . map (entityKey &&& id))
             (select $ from $ \pd -> do
                  where_ $
                    pd ^. PaydayId `in_`
                        valList
                            (setNub (mapMaybe (transactionPayday . entityVal)
                                              transactions))
                  return pd)

    return (project, account, account_map, process payday_map transactions)
  where
    process payday_map =
        let process' [] [] = []
            process' (t':ts') [] =
                [( fmap (payday_map M.!) (transactionPayday (entityVal t'))
                 , reverse (t':ts'))]
            process' [] (t:ts) = process' [t] ts

            process' (t':ts') (t:ts)
                | samePayday t' t = process' (t:t':ts') ts
                | otherwise =
                    ( fmap (payday_map M.!) (transactionPayday $ entityVal t')
                        , reverse (t':ts'))
                    : process' [t] ts
         in process' []
    samePayday = (==) `on` (transactionPayday . entityVal)

data NegativeBalances = NegativeBalances ProjectId [UserId]
    deriving (Show, Typeable)

instance Exception NegativeBalances

payout :: (MonadIO m, Functor m)
       => UTCTime
       -> (Entity Fixme.Project, Entity Payday)
       -> SqlPersistT m Bool
payout now (Entity project_id project, Entity payday_id _) = do
    let project_name = projectName project

    pledges <- select $ from $ \pledge -> do
        where_ $ pledge ^. PledgeProject ==. val project_id
            &&. pledge ^. PledgeFundedShares >. val 0

        return pledge

    user_balances <- forM pledges $ \(Entity _ pledge) -> do
        Just user <- get $ pledgeUser pledge
        let amount =
                projectShareValue project
                $* fromIntegral (pledgeFundedShares pledge)
            user_account_id = userAccount user
            project_account_id = projectAccount project

        void $
            insert $
                Transaction now
                            (Just project_account_id)
                            (Just user_account_id)
                            (Just payday_id)
                            amount
                            "Project Payout"
                            Nothing

        user_account <-
            updateGet
                user_account_id
                [AccountBalance P.-=. amount]
        _            <-
            updateGet
                project_account_id
                [AccountBalance P.+=. amount]

        return (pledgeUser pledge, accountBalance user_account)

    let negative_balances = filter ((< 0) . snd) user_balances

    unless (null negative_balances)
           (throw $ NegativeBalances project_id $ map fst negative_balances)

    update $ \p -> do
        set p [ ProjectLastPayday =. val (Just payday_id) ]
        where_ $ p ^. ProjectId ==. val project_id

    liftIO $ putStrLn $ "paid to " <> T.unpack project_name

    return True

projectsToPay :: MonadIO m
              => UTCTime
              -> SqlPersistT m [(Entity Fixme.Project, Entity Payday)]
projectsToPay now =
    select $
    from $ \(project
        `LeftOuterJoin` last_payday
        `InnerJoin` payday) -> do
    on_ $ payday ^. PaydayDate
        >. coalesceDefault
            [ last_payday ?. PaydayDate ]
            (project ^. ProjectCreatedTs)
    on_ $ project ^. ProjectLastPayday ==. last_payday ?. PaydayId
    where_ $ payday ^. PaydayDate <=. val now
    orderBy [ asc $ payday ^. PaydayDate
            , desc $ project ^. ProjectShareValue ]
    return (project, payday)

rebalanceAllPledges :: (MonadBaseControl IO m, MonadResource m, MonadLogger m)
                    => Lazy.WriterT [PledgeId] (ReaderT SqlBackend m) ()
rebalanceAllPledges = do
    unders <- lift underfundedPatrons
    unless (null unders) $ do
        maxUnders <- lift $ maxShares Nothing unders
        lift $ dropShares maxUnders
        lift $ mapM_ updateShareValue =<< updatedProjects maxUnders
        Lazy.tell maxUnders
        rebalanceAllPledges

updatedProjects :: (MonadIO m, Functor m)
                => [PledgeId]
                -> SqlPersistT m [ProjectId]
updatedProjects pledges = fmap (map (pledgeProject . entityVal))
                               (selectList [PledgeId <-. pledges] [])