module Main where

import Prelude
import Type.Row as Row
import Type.Row
import Type.RowList
import Data.Symbol (class IsSymbol, reflectSymbol)
import Record as R
import Type.Proxy
import Data.Foldable
import Data.FoldableWithIndex (foldrWithIndex)
import Data.List as List
import Data.Array as Ary

import Effect (Effect)

data ValidationResult a = FieldError String
                        | ChildrenErrors (Array { index :: Int, childErrors :: a })
                        | Ok

removeOks = Ary.filter (not <<< isOk)
  where isOk Ok = true
        isOk _ = false

instance Show a => Show (ValidationResult a) where
  show (FieldError s) = "Field error: " <> s
  show (ChildrenErrors ary) = "Children errors: " <> show ary
  show _ = "ok"

derive instance Eq a => Eq (ValidationResult a)

type Rules a b = Array (a -> ValidationResult b)

class ThreeWay :: forall validatorRL inputRL outputRL.
                  validatorRL -> inputRL -> outputRL -> Constraint
class ThreeWay validatorRL inputRL outputRL | validatorRL inputRL -> outputRL

instance ThreeWay Nil Nil Nil

instance ( IsSymbol l ) => ThreeWay Nil (Cons l a restRL) Nil

else instance ( IsSymbol l
         , ThreeWay validatorRL inputRL outputRL
         , RowToList validatorRow validatorRL
         , RowToList inputRow inputRL
         , RowToList outputRow outputRL
         )
        => ThreeWay (Cons l (Record validatorRow) Nil)
                    (Cons l (Record inputRow) anything)
                    (Cons l (Record outputRow) Nil)

else instance ( IsSymbol l )
             => ThreeWay (Cons l (Rules inputTy outputTy) Nil)
                         (Cons l (Array inputTy) anything)
                         (Cons l outputTy Nil)

else instance ( IsSymbol l
              , ThreeWay validatorRL inputRestRL outputRL
              )
             => ThreeWay validatorRL
                         (Cons l b inputRestRL)
                         outputRL


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
  , ThreeWay validatorRL inputRL outputRL
  , RowToList validatorRow validatorRL
  , RowToList validatorRow (Cons l (Record subValidatorRow) validatorRLRest)
  , RowToList subValidatorRow subValidatorRL
  , RowToList validatorRowRest validatorRLRest
  , Lacks l validatorRowRest
  , Cons l (Record subValidatorRow) validatorRowRest validatorRow

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
  , RowToList validatorRow (Cons l (Rules ty tyOut) validatorRLRest) -- should tyOut by tyOut?
  , RowToList validatorRowRest validatorRLRest
  , Lacks l validatorRowRest
  , Cons l (Rules ty tyOut) validatorRowRest validatorRow

  , RowToList outputRow outputRL
  , RowToList outputRow (Cons l (Array (ValidationResult tyOut)) outputRLRest)
  , RowToList outputRowRest outputRLRest
  , Lacks l outputRowRest
  , Cons l (Array (ValidationResult tyOut)) outputRowRest outputRow

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
        fieldErrors = map (_ $ fieldValue) fieldValidators # removeOks
        validatorRest = R.delete proxy validator
        restErrors = gValidate validatorRest
                               (Proxy :: Proxy validatorRLRest)
                               input
                               (Proxy :: Proxy inputRL)
                               (Proxy :: Proxy outputRLRest)


validate :: forall validatorRow validatorRL inputRow inputRL outputRow outputRL.
            RowToList validatorRow validatorRL
         => RowToList inputRow inputRL
         => RowToList outputRow outputRL
         => GValidate validatorRL validatorRow inputRL inputRow outputRL outputRow
         => Record validatorRow
         -> Record inputRow
         -> Record outputRow
validate validator input = gValidate validator
                                     (Proxy :: Proxy validatorRL)
                                     input
                                     (Proxy :: Proxy inputRL)
                                     (Proxy :: Proxy outputRL)


atLeast :: forall a. Int -> Int -> ValidationResult Void
atLeast x y = if x <= y then Ok else FieldError "too small"

lengthAtLeast x y = if x <= Ary.length y then Ok else FieldError "too few elements"

atMost :: forall a. Int -> Int -> ValidationResult a
atMost x y = if x < y then Ok else FieldError "too big"

notEmpty :: forall a. String -> ValidationResult Void
notEmpty s = if s == "" then FieldError "empty" else Ok


each :: forall validatorRow validatorRL inputRow inputRL outputRow outputRL
      . GValidate validatorRL validatorRow inputRL inputRow outputRL outputRow
     => RowToList validatorRow validatorRL
     => RowToList inputRow inputRL
     => RowToList outputRow outputRL
     => AllErrorsEmpty outputRL outputRow
     => Eq (Record outputRow)
     => Record validatorRow
     -> Array (Record inputRow)
     -> ValidationResult (Record outputRow)
each validator xs = childrenErrors # List.toUnfoldable
                                   # (\x -> if Ary.null x then Ok else ChildrenErrors x)
  where childrenErrors = foldrWithIndex go List.Nil xs
        go i (x :: Record inputRow) acc =
                     let errs = validate validator x
                         isValid = allErrorsEmpty (Proxy :: Proxy outputRL) errs
                     in if isValid
                        then acc
                        else List.Cons { index: i, childErrors: errs } acc

isValid :: forall row rl. RowToList row rl
                       => AllErrorsEmpty rl row
                       => Record row
                       -> Boolean
isValid errs = allErrorsEmpty (Proxy :: Proxy rl) errs


class AllErrorsEmpty rl row | rl -> row where
  allErrorsEmpty :: Proxy rl -> Record row -> Boolean

instance AllErrorsEmpty Nil () where
  allErrorsEmpty _ _ = true

else instance ( IsSymbol l
              , RowToList row rl
              , RowToList row (Cons l (Array trash) restRL)
              , RowToList restRow restRL
              , Cons l (Array trash) restRow row
              , Lacks l restRow
              , AllErrorsEmpty restRL restRow
              )
             => AllErrorsEmpty (Cons l (Array trash) restRL) row where
  allErrorsEmpty _ rec = if Ary.null field
                           then allErrorsEmpty (Proxy :: Proxy restRL) (R.delete proxy rec)
                           else false
    where proxy :: Proxy l
          proxy = Proxy

          field = R.get proxy rec

else instance AllErrorsEmpty a b where
  allErrorsEmpty _ _ = false
