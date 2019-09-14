module Bautzen.REPL

import Bautzen.Game
import Bautzen.REPL.SExpParser
import Bautzen.SExp

import Data.Fin
import Prelude.Interactive

%default total

ToSExp Side where
  toSExp Axis = SSym "Axis"
  toSExp Allies = SSym "Allies"

ToSExp Nation where
  toSExp German = SSym "German"
  toSExp Russian = SSym "Russian"
  toSExp Polish = SSym "Polish"

ToSExp UnitType where
  toSExp Armored = SSym "Armored"
  toSExp HeavyArmored = SSym "HeavyArmored"
  toSExp MechInfantry = SSym "MechInfantry"
  toSExp Infantry = SSym "Infantry"
  toSExp HeavyEngineer = SSym "HeavyEngineer"
  toSExp Artillery = SSym "Artillery"
  toSExp AntiTank = SSym "AntiTank"
  toSExp HQ = SSym "HQ"
  toSExp SupplyColumn = SSym "SupplyColumn"

ToSExp UnitSize where
  toSExp Regiment = SSym "Regiment"
  toSExp Brigade = SSym "Brigade"
  toSExp Division = SSym "Division"
  toSExp Corps = SSym "Corps"

ToSExp StdFactors where
  toSExp (MkStdFactors attack defense) =
    SList [ toSExp attack
          , toSExp defense
          ]

ToSExp Arty where
  toSExp (MkArty support distance) =
    SList [ toSExp support
          , toSExp distance
          ]

ToSExp GameUnit where
  toSExp (MkGameUnit nation unitType name size move currentMP hit combat) =
    SList [ SSym ":unit"
          , toSExp nation
          , toSExp unitType
          , toSExp name
          , toSExp size
          , toSExp move
          , toSExp currentMP
          , toSExp hit
          , case unitType of
                                    Armored       => toSExp combat
                                    HeavyArmored  => toSExp combat
                                    MechInfantry  => toSExp combat
                                    Infantry      => toSExp combat
                                    HeavyEngineer => toSExp combat
                                    Artillery     => toSExp combat
                                    AntiTank      => toSExp combat
                                    HQ            => toSExp combat
                                    SupplyColumn  => toSExp combat
          ]

ToSExp Pos where
  toSExp (Hex col row) =
    SList [ toSExp col
          , toSExp row
          ]

ToSExp Cost where
  toSExp = toSExp . toNat

ToSExp GameError where
  toSExp (NoSuchUnit unitName) = SList [ SSym ":error", SSym "NoSuchUnit", SStr unitName ]
  toSExp (NotYourTurn side) = SList [ SSym ":error", SSym "NoSuchUnit", toSExp side ]
  toSExp (EnemyInHex unit hex) = SList [ SSym ":error", SSym "EnemyInHex", toSExp unit, toSExp hex ]
  toSExp (MoveFromZocToZoc unit to) = SList [ SSym ":error", SSym "MoveFromZocToZoc", toSExp unit, toSExp to ]
  toSExp (ForbiddenTerrain from to) = SList [ SSym ":error", SSym "ForbiddenTerrain", toSExp from, toSExp to ]
  toSExp (InvalidMove from to) = SList [ SSym ":error", SSym "InvalidMove", toSExp from, toSExp to ]
  toSExp (NotEnoughMPs unit from to mp) = SList [ SSym ":error", SSym "NotEnoughMPs", toSExp unit, toSExp from, toSExp to, toSExp mp ]

ToSExp Event where
  toSExp (Moved unit from to cost) =
      SList [ SSym ":moved"
            , SSym ":unit", toSExp unit
            , SSym ":from", toSExp from
            , SSym ":to", toSExp to
            , SSym ":cost", toSExp cost
            ]

makeMoveCommand : (unitName : String) -> (col : Int) -> (row : Int) -> Either String (Command Move)
makeMoveCommand unitName col row with (fromIntegerNat (cast col), fromIntegerNat (cast row))
  | (c , r) with (isLTE c 22, isLTE r 12)
    | (Yes _, Yes _) = Right $ MoveTo unitName (Hex c r)
    | _              = Left $ "position should be between (0,0) and (22, 12): " ++ show col ++ ", " ++ show row


makeCommand : (game : Game) -> SExp -> Either String (Command  (curSegment game))
makeCommand game (SList [ SSym ":move", SStr unitName, SList [ SInt col, SInt row] ] ) with (curSegment game)
  | Move = makeMoveCommand unitName col row
  | other = Left $ "Invalid command for segment " ++ show other
makeCommand _ sexp = Left $ "Unknown command " ++ show sexp

partial
parseCommand : (game : Game) -> String -> Either String (Command (curSegment game))
parseCommand game input = do
  sexp <- parseSExp input
  makeCommand game sexp

partial
commandHandler : Game -> String -> (String, Game)
commandHandler game command =
  case parseCommand game command of
    Left err => (err, game)
    Right cmd => case act game cmd of
                      Left err => (show (toSExp err) ++ "\n", game)
                      Right event => (show (toSExp event) ++ "\n", apply event game)

partial
eoiHandler : Game -> String
eoiHandler = show

initialPositions : List (GameUnit, Pos)
initialPositions = [ (Bautzen.GameUnit.r13_5dp, Hex 3 4)
                   , (Bautzen.GameUnit.g21_20pz, Hex 3 5)
                   ]

initialState : GameState
initialState = MkGameState 0 Axis Move initialPositions

initialGame : Game
initialGame = MkGame [] initialState

export
partial
repl : IO ()
repl = processStdin initialGame commandHandler eoiHandler
