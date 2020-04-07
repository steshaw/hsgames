module GameServer.State where

import Control.Concurrent.STM
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans (MonadIO(..))
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Map as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import GHC.Natural
import System.Random (StdGen, split)

import GameServer.Game
import GameServer.Player
import GameServer.Utils

data Games = Games { seed :: StdGen
                   , games :: Map.Map Id Game
                   , players :: Map.Map Text Player
                   }

initialState :: StdGen -> Games
initialState initSeed = Games initSeed mempty mempty

type GameState = TVar Games

withState ::
  (MonadIO m) => GameState -> State Games a -> m a
withState st =
  liftIO . atomically . stateTVar st . runState

data Command = JoinGame { gameId :: Id, pName :: Text }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Event = GameCreated { gameId :: Id }
           | PlayerJoined { gameId :: Id, playerName :: Text }
           | PlayerAlreadyJoinedGame { gameId :: Id, playerName :: Text }
           | PlayerRegistered { registeredName :: Text }
           | DuplicatePlayer { duplicateName :: Text }
           | PlayerDoesNotExist { playerName :: Text }
           | UnknownGame { gameId :: Id }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype GameError = GameError { reason :: Event }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- * Queries

listGames :: State Games [Game]
listGames = Map.elems <$> gets games

lookupGame :: Id -> State Games (Maybe Game)
lookupGame gameId  = Map.lookup gameId  <$> gets games

listPlayers :: State Games [Player]
listPlayers = Map.elems <$> gets players

-- * Commands

applyCommand :: Command -> State Games (Either GameError Event)
applyCommand JoinGame{gameId,pName} = do
  gs <- get
  case Map.lookup gameId (games gs) of
    Nothing -> pure $ Left $ GameError $ UnknownGame gameId
    Just game -> if
      | pName `elem` gamePlayers game -> pure $ Left $ GameError $ PlayerAlreadyJoinedGame gameId pName
      | Map.lookup pName (players gs) == Nothing -> pure $ Left $ GameError $ PlayerDoesNotExist pName
      | otherwise -> do
          let game' = game { gamePlayers = pName : gamePlayers game }
          put (gs { games = Map.insert gameId game' (games gs) })
          pure $ Right $ PlayerJoined gameId pName


createGame :: Game -> State Games Event
createGame game = do
  gs <- get
  let gid = randomId (seed gs)
      (_,newSeed) = split (seed gs)
  put (gs { seed = newSeed
          , games = Map.insert gid game (games gs)
          })
  pure $ GameCreated gid

registerPlayer :: Player -> State Games Event
registerPlayer player@Player{playerName} = do
  gs <- get
  case Map.lookup playerName (players gs) of
    Just _ -> pure $ DuplicatePlayer playerName
    Nothing -> do
      put (gs { players = Map.insert playerName player (players gs) })
      pure $ PlayerRegistered playerName
