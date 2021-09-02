module Inigo.Template

import Data.List1
import Data.SnocList
import Data.String
import Debug.Trace

public export
record Pos where
    constructor MkPos
    line : Int
    col : Int

Show Pos where
    show pos = "\{show pos.line}:\{show pos.col}"

initPos : Pos
initPos = MkPos 1 1

nextChar : Pos -> Pos
nextChar = record { col $= (+ 1) }

nextLine : Pos -> Pos
nextLine = record { line $= (+ 1), col = 1 }

advance : Char -> Pos -> Pos
advance '\n' = nextLine
advance _ = nextChar

data TempVar
    = Package
    | Namespace
    | File String

data TempPart : Type where
    Var : TempVar -> TempPart
    Text : String -> TempPart

data TempState : Type where
    InText : SnocList Char -> TempState
    InVar : SnocList Char -> TempState
    Escape : TempState

initState : TempState
initState = InText [<]

unknownVar : Either String a
unknownVar = Left "Unknown variable"

parseVar : List Char -> Maybe TempVar
parseVar ['P', 'a', 'c', 'k', 'a', 'g', 'e'] = Just Package
parseVar ['N', 'a', 'm', 'e', 's', 'p', 'a', 'c', 'e'] = Just Namespace
parseVar ('f' :: 'i' :: 'l' :: 'e' :: ' ' :: file) = Just $ File $ fastPack file
parseVar _ = Nothing

makeText : SnocList TempPart -> SnocList Char -> SnocList TempPart
makeText acc [<] = acc
makeText acc xs = acc :< Text (fastPack $ xs <>> [])

cleanStart : List TempPart -> List TempPart
cleanStart (Text t :: xs) = if "\n" `isPrefixOf` t
    then Text (assert_total $ strSubstr 1 (cast (length t) - 1) t) :: xs
    else Text t :: xs
cleanStart xs = xs

cleanEnd : SnocList TempPart -> SnocList TempPart
cleanEnd (xs :< Text t) = if "\n" `isSuffixOf` t
    then xs :< Text (assert_total $ strSubstr 0 (cast (length t) - 1) t)
    else xs :< Text t
cleanEnd xs = xs

parse :
    TempState -> -- state
    List Char -> -- input
    SnocList TempPart -> -- accumulator
    Pos -> -- position
    Either (Pos, String) (SnocList TempPart)
parse (InText xs) [] acc pos = Right $ makeText acc xs
parse (InText xs) ('\\' :: ys) acc pos = parse Escape ys (makeText acc xs) (nextChar pos)
parse (InText xs) (x :: ys) acc pos = parse (InText $ xs :< x) ys acc (advance x pos)

parse (InVar xs) [] acc pos = Left (pos, "Missing closing brace")
parse (InVar xs) ('}' :: ys) acc pos =
    let xs' = xs <>> []
     in case parseVar xs' of
        Nothing => Left (pos, "Unknown variable: \{fastPack xs'}")
        Just var@(File _) => parse initState ys (cleanEnd acc :< Var var) (nextChar pos)
        Just var => parse initState ys (acc :< Var var) (nextChar pos)
parse (InVar xs) (x :: ys) acc pos = parse (InVar $ xs :< x) ys acc (advance x pos)

parse Escape [] acc pos = Left (pos, "Trailing escape character")
parse Escape ('\\' :: xs) acc pos = parse initState xs (acc :< Text "\\") (nextChar pos)
parse Escape ('{' :: xs) acc pos = parse (InVar [<]) xs acc (nextChar pos)
parse Escape (x :: xs) acc pos = Left (pos, "Unknown escape character: \{show x}")

seperateFiles :
    SnocList TempPart -> -- input
    List TempPart -> -- accumulator for the file
    List (String, List TempPart) -> -- accumulator for the entire template
    Either String (List (String, List TempPart))
seperateFiles [<] _ acc = Right acc
seperateFiles (sx :< Var (File fp)) file acc = seperateFiles sx [] ((fp, cleanStart file) :: acc)
seperateFiles (sx :< x) xs ys = seperateFiles sx (x :: xs) ys

parameters (ns, pkg : String)

    renderVar : TempVar -> Either String String
    renderVar Package = Right pkg
    renderVar Namespace = Right ns
    renderVar (File x) = Left "Internal Error: Unexpected \\{file ...}"

    renderTemplate :
        List TempPart -> -- template
        SnocList String -> -- accumulator
        Either String (SnocList String)
    renderTemplate [] acc = Right acc
    renderTemplate (Var x :: xs) acc = case renderVar x of
        Left err => Left err
        Right v => renderTemplate xs (acc :< v)
    renderTemplate (Text x :: xs) acc = renderTemplate xs (acc :< x)

    render :
        List (String, List TempPart) ->
        List (String, String) ->
        Either String (List (String, String))
    render [] acc = Right acc
    render ((fp, temp) :: ts) acc = case renderTemplate temp [<] of
        Left err => Left err
        Right res => render ts $ (fp, fastConcat $ res <>> []) :: acc

    export
    runTemplate :
        String -> -- filepath
        String -> -- input
        Either String (List (String, String))
    runTemplate fp inp0 = do
        let Right inp1 = parse initState (fastUnpack inp0)  [<] initPos
            | Left (pos, err) => Left $ "Parse error in \{fp}, at \{show pos}: \{err}"
        let Right inp2 = seperateFiles inp1 [] []
            | Left err => Left $ "Parse error in \{fp}: \{err}"
        render inp2 []
