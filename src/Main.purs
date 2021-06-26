module Main where

import Prelude
import Prim.RowList as RL -- (RowList, Cons, Nil, class RowToList)
--import Prim.Row as Row
import Type.Row as Row
import Type.Row
import Type.RowList
import Data.Symbol (class IsSymbol, reflectSymbol)
import Record as R
import Type.Proxy

import Effect (Effect)
import Effect.Console (log)

data True
data False

data ValidationResult = Error String | Ok

instance Show ValidationResult where
  show (Error s) = s
  show _ = ""

type Rules a = Array (a -> ValidationResult)

class IsRecord (a :: Type) bool | a -> bool

instance IsRecord (Record row) True
else instance IsRecord a False

class PermutedSubRL :: forall rl1 rl2. rl1 -> rl2 -> Constraint
class PermutedSubRL rl1 rl2

-- {} < everything
instance PermutedSubRL Nil rl

-- if a < b and are records, then { x :: a } < { x :: b } + r
else instance ( IsSymbol l
              , RowToList ty1 rl1
              , RowToList ty2 rl2
              , PermutedSubRL rl1 rl2 )
             => PermutedSubRL (Cons l (Record ty1) Nil)
                              (Cons l (Record ty2) trash)

-- { x :: Rules a } < { x :: a } + r for non-record a, b
else instance ( IsSymbol l
              , IsRecord a False )
             => PermutedSubRL (Cons l (Rules a) Nil) (Cons l a rest)

-- if { x :: a } < r, then { x :: a } < { y :: b } + r
else instance ( IsSymbol l1
              , PermutedSubRL (Cons l1 a Nil) rl
              , IsSymbol l2 )
             => PermutedSubRL (Cons l1 a Nil) (Cons l2 b rl)

-- if r1 < r2 and { x :: a } < r2, then { x :: a } + r1 < r2
else instance ( IsSymbol l
              , PermutedSubRL (Cons l a Nil) rl2
              , PermutedSubRL rl1 rl2 )
             => PermutedSubRL (Cons l a rl1) rl2


greaterThan :: Int -> Int -> ValidationResult
greaterThan x y = if x > y then Ok else Error "too small"

lessThan :: Int -> Int -> ValidationResult
lessThan x y = if x < y then Ok else Error "too big"

notEmpty :: String -> ValidationResult
notEmpty s = if s == "" then Error "empty" else Ok


each validator xs = map validator xs


type Student = { studentId :: Int, name :: String, age :: Int }
type School  = { name :: String
                , schoolId :: Int
--                , students :: Array Student
                }



type Errs = Array ValidationResult

class RLErrorsMap a b | a -> b

instance RLErrorsMap Nil Nil

else instance ( IsSymbol l
              , RowToList row1 rl1
              , RowToList row2 rl2
              , RLErrorsMap rl1 rl2
              , RLErrorsMap rest1 rest2 )
             => RLErrorsMap (Cons l (Record row1) rest1) (Cons l (Record row2) rest2)

else instance ( IsSymbol l
              , RLErrorsMap rl1 rl2 )
             => RLErrorsMap (Cons l a rl1) (Cons l Errs rl2)

-- else instance ( IsSymbol l
--               , RLErrorsMap (Cons l a Nil) rl
--               , RLErrorsMap rest rl )
--              => RLErrorsMap (Cons l a rest) (Cons l String rest)

gogo :: forall rl1 rl2 row1 row2. RLErrorsMap rl1 rl2
                               => RowToList row1 rl1
                               => RowToList row2 rl2
                               => Record row1
                               -> Record row2
                               -> Int
gogo _ _ = 5

hi :: Int
hi = gogo {a: 2, x: {w: 4, y: 4, z: 6}} {x: {z: [Ok], w: [Ok], y: [Ok]}, a: [Ok]}



class GValidate (validatorRL :: RowList Type)
                (validatorRow :: #Type)
                (inputRL :: RowList Type)
                (inputRow :: #Type)
                (outputRL :: RowList Type)
                (outputRow :: #Type)
                | validatorRL -> outputRL
                , validatorRL -> validatorRow
                , validatorRL -> outputRow
                , outputRL -> outputRow
                , inputRL -> inputRow
  where
  gValidate :: Record validatorRow
            -> Proxy validatorRL
            -> Record inputRow
            -> Proxy inputRL
            -> Proxy outputRL
            -> Record outputRow

instance GValidate Nil () inputRL inputRow Nil () where
  gValidate _ _ _ _ _ = {}


else instance
  ( RowToList inputRow inputRL

  , IsSymbol l
  , RowToList validatorRow validatorRL
  , RowToList validatorRow (Cons l (Record subValidatorRow) validatorRLRest)
  , RowToList subValidatorRow subValidatorRL
  , RowToList validatorRowRest validatorRLRest
  , Lacks l validatorRowRest
  , Cons l (Record subValidatorRow ) validatorRowRest validatorRow

  , Cons l (Record subInputRow) inputRowRest inputRow
  , RowToList subInputRow subInputRL

  , RowToList outputRow outputRL
  , RowToList outputRow (Cons l (Record subOutputRow) outputRLRest)
  , RowToList outputRowRest outputRLRest
  , RowToList subOutputRow subOutputRL
  , Lacks l outputRowRest
  , Cons l (Record subOutputRow) outputRowRest outputRow

  , GValidate subValidatorRL
              subValidatorRow
              subInputRL
              subInputRow
              subOutputRL
              subOutputRow

  , GValidate validatorRLRest
              validatorRowRest
              inputRL
              inputRow
              outputRLRest
              outputRowRest
  )
 => GValidate (Cons l (Record subValidatorRow) validatorRLRest)
              validatorRow
              inputRL
              inputRow
              outputRL
              outputRow where
    gValidate validator _ input _ _ = R.insert proxy subErrs restErrs
      where
        proxy :: Proxy l
        proxy = Proxy

        fieldName = reflectSymbol proxy
        subInput = R.get proxy input
        subValidator = R.get proxy validator
        subErrs = gValidate subValidator
                            (Proxy :: Proxy subValidatorRL)
                            subInput
                            (Proxy :: Proxy subInputRL)
                            (Proxy :: Proxy subOutputRL)
        validatorRest = R.delete proxy validator
        restErrs = gValidate validatorRest
                             (Proxy :: Proxy validatorRLRest)
                             input
                             (Proxy :: Proxy inputRL)
                             (Proxy :: Proxy outputRLRest)



else instance
  ( RowToList inputRow inputRL
  , Cons l ty inputRowRest inputRow

  , IsSymbol l
  , RowToList validatorRow validatorRL
  , RowToList validatorRow (Cons l (Rules ty) validatorRLRest)
  , RowToList validatorRowRest validatorRLRest
  , Lacks l validatorRowRest
  , Cons l (Rules ty) validatorRowRest validatorRow

  , RowToList outputRow outputRL
  , RowToList outputRow (Cons l (Array ValidationResult) outputRLRest)
  , RowToList outputRowRest outputRLRest
  , Lacks l outputRowRest
  , Cons l (Array ValidationResult) outputRowRest outputRow

  , GValidate validatorRLRest
              validatorRowRest
              inputRL
              inputRow
              outputRLRest
              outputRowRest
  )
 => GValidate validatorRL validatorRow inputRL inputRow outputRL outputRow where
    gValidate validator _ input _ _ = R.insert proxy fieldErrors restErrors
      where
        proxy :: Proxy l
        proxy = Proxy

        fieldName = reflectSymbol proxy
        fieldValue = R.get proxy input
        fieldValidators = R.get proxy validator
        fieldErrors = map (_ $ fieldValue) fieldValidators
        validatorRest = R.delete proxy validator
        restErrors = gValidate validatorRest
                               (Proxy :: Proxy validatorRLRest )
                               input
                               (Proxy :: Proxy inputRL)
                               (Proxy :: Proxy outputRLRest)


validate :: forall validatorRow validatorRL inputRow inputRL outputRow outputRL.
            RowToList validatorRow validatorRL
         => RowToList inputRow inputRL
         => RowToList outputRow outputRL
         => PermutedSubRL validatorRL inputRL
         => RLErrorsMap validatorRL outputRL
         => GValidate validatorRL validatorRow inputRL inputRow outputRL outputRow
         => Record validatorRow
         -> Record inputRow
         -> Record outputRow
validate validator input = gValidate validator
                                     (Proxy :: Proxy validatorRL)
                                     input
                                     (Proxy :: Proxy inputRL)
                                     (Proxy :: Proxy outputRL)

subinput = {b: 17}
input = {a: subinput }


subvalidator = { b: [greaterThan 17]}

validator = { a: subvalidator }

errors = { y: [ Ok ] }


-- blah :: { y :: Array ValidationResult }
blah = validate validator input
