{- |
 - Module      :  $Header$
 - Description :  Function to evaluate an rpn expression
 - Copyright   :  (c) Yoriyasu Yano
 - License     :  MIT
 -
 - Maintainer  :  yorinasub17@gmail.com
 - Stability   :  experimental
 - Portability :  portable
 - -}

module RPN ( rpn ) where

-- |This will evaluate the given RPN expression and updates the passed in stack. This will return the final stack at the end.
rpn :: [String]     -- ^ Represents the list of terms that need to be evaluated
    -> [Float]      -- ^ Represents the rpn stack, containing the current evaluation results
    -> [Float]      -- ^ Returns the state of the stack at the end
rpn [] stack = stack
rpn ("+":xs) (x:y:zs) = rpn xs ((x+y):zs)
rpn ("-":xs) (x:y:zs) = rpn xs ((x-y):zs)
rpn ("*":xs) (x:y:zs) = rpn xs ((x*y):zs)
rpn ("/":xs) (x:y:zs) = rpn xs ((x/y):zs)
rpn (x:xs) stack = rpn xs ((read x :: Float):stack)

