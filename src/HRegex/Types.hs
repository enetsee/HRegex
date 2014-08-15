{-# LANGUAGE OverloadedStrings #-}
module HRegex.Types where

    import qualified Data.Text as T
    import Data.Char

    data RegexExpr = 
          Atom RegexAtom
        | Seq [RegexExpr]
        | Choice [RegexExpr]
        | Assertion LookAround RegexExpr


    data RegexAtom =
          Char Char
        | CharSet [RegexSet] Negated
        | CaptureGrp Int T.Text RegexExpr
        | NonCaptureGrp RegexExpr
        | BackRef Int
        | Repeat RegexAtom Quantifier Greedy
        | Anchor Anchor

    data RegexSet =
          Range Char Char
        | SetChar Char


    type  Quantifier =  (Int, Maybe Int)

    data Anchor = BOL | EOL | Boundary | NotBoundary

    data LookAround = LookAhead | LookBehind | NegLookAhead | NegLookBehind

    type Greedy = Bool

    type Negated = Bool



    
    negateRegex :: RegexExpr -> RegexExpr
    negateRegex (Atom a) = Atom $ negateAtom a
    negateRegex (Seq xs) = Seq $ map negateRegex xs
    negateRegex (Choice xs) = Choice $ map negateRegex xs
    negateRegex (Assertion l cond) = flip Assertion cond $ negateLookAround l



    negateAtom (Char c) = CharSet [SetChar c] True

    negateAtom (CharSet x@((SetChar c):[]) negated) 
        | negated == True =  Char c
        | otherwise = CharSet x True

    negateAtom (CharSet as b) =  CharSet as $ not b

    negateAtom (CaptureGrp n nm expr) = CaptureGrp n nm $ negateRegex expr

    negateAtom (NonCaptureGrp expr) = NonCaptureGrp $ negateRegex expr

    negateAtom (Repeat atom qnt greedy) = Repeat atom' qnt greedy
        where
            atom' = negateAtom atom

    negateAtom a = a


    negateLookAround :: LookAround -> LookAround
    negateLookAround LookAhead = NegLookAhead
    negateLookAround NegLookAhead = LookAhead
    negateLookAround LookBehind = NegLookBehind
    negateLookAround NegLookBehind = LookBehind


    tabAtom :: RegexAtom
    tabAtom = Char '\t'

    tab :: RegexExpr
    tab = Atom tabAtom

    formFeedAtom :: RegexAtom
    formFeedAtom = Char '\FF'

    ff :: RegexExpr
    ff = Atom formFeedAtom

    crAtom :: RegexAtom
    crAtom = Char '\CR'

    cr :: RegexExpr
    cr = Atom crAtom


    lineFeedAtom :: RegexAtom
    lineFeedAtom = Char '\LF'

    lf :: RegexExpr
    lf = Atom lineFeedAtom


    verticalTabAtom :: RegexAtom
    verticalTabAtom = Char '\VT'

    vt :: RegexExpr
    vt = Atom verticalTabAtom
    
    nulAtom :: RegexAtom
    nulAtom = Char '\NUL'

    nul :: RegexExpr
    nul = Atom crAtom




    whitespaceChars :: [Char]
    whitespaceChars = [c | c <- [minBound..maxBound], generalCategory c == Space]

    whitespaceAtom :: RegexAtom
    whitespaceAtom = flip CharSet False $ map SetChar whitespaceChars

    notWhitespaceAtom :: RegexAtom
    notWhitespaceAtom = negateAtom whitespaceAtom

    whitespace :: RegexExpr
    whitespace = Atom whitespaceAtom

    notWhitespace :: RegexExpr
    notWhitespace = Atom notWhitespaceAtom
    

    newlineChars :: [Char]
    newlineChars =  [ c | c <- [minBound .. maxBound] , generalCategory c == LineSeparator]  -- ['\r','\n','\u2028', '\u2029']

    newlineAtom :: RegexAtom
    newlineAtom = flip CharSet False $ map SetChar newlineChars

    newline :: RegexExpr
    newline = Atom newlineAtom

    dotAtom :: RegexAtom
    dotAtom = negateAtom newlineAtom

    dot :: RegexExpr
    dot = Atom dotAtom

    digitAtom :: RegexAtom
    digitAtom = CharSet [Range '0' '9'] False

    digit :: RegexExpr
    digit = Atom digitAtom

    notDigitAtom :: RegexAtom
    notDigitAtom = negateAtom digitAtom

    notDigit :: RegexExpr
    notDigit = Atom notDigitAtom

    wordAtom :: RegexAtom
    wordAtom = CharSet [Range 'A' 'Z', Range 'a' 'z', Range '0' '9', SetChar '_'] False

    word :: RegexExpr
    word = Atom wordAtom

    notWordAtom :: RegexAtom
    notWordAtom = negateAtom wordAtom

    notWord :: RegexExpr
    notWord = Atom notWordAtom