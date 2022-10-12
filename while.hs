import Data.Bool ( Bool(False, True) )


-- While langauge
type Zahl = Int
type Var = String
data Aexp = Z Zahl | V Var | Add Aexp Aexp | Mul Aexp Aexp | Sub Aexp Aexp deriving (Show, Eq)
data Bexp = BTrue | BFalse | Eq Aexp Aexp | Leq Aexp Aexp | Not Bexp | And Bexp Bexp deriving (Show, Eq)
data Statement = Assign Var Aexp | Skip | Colon Statement Statement | If Bexp Statement Statement | While Bexp Statement deriving (Show, Eq)

-- States
type State = Var -> Int -- General Definition
sigma :: State -- Defines the current state
sigma x = 0

-- Semantic function for numbers
fancyN :: Zahl -> Int
fancyN n = n

-- Semantic function for Aexp
fancyA :: Aexp -> (State -> Int)
fancyA (Z n) _ = fancyN n
fancyA (V x) sigma = sigma x
fancyA (Add a1 a2) sigma = fancyA a1 sigma + fancyA a2 sigma
fancyA (Mul a1 a2) sigma = fancyA a1 sigma * fancyA a2 sigma
fancyA (Sub a1 a2) sigma = fancyA a1 sigma - fancyA a2 sigma

-- Semantic function for Bexp
fancyB :: Bexp -> (State -> Bool)
fancyB BTrue _ = True
fancyB BFalse _ = False
fancyB (Eq a1 a2) sigma = fancyA a1 sigma == fancyA a2 sigma
fancyB (Leq a1 a2) sigma = fancyA a1 sigma <= fancyA a2 sigma
fancyB (Not b) sigma = not (fancyB b sigma)
fancyB (And b1 b2) sigma = fancyB b1 sigma && fancyB b2 sigma

-- Substitution function for Aexp
substA :: Aexp -> Var -> Aexp -> Aexp
substA (Z n) pattern replacement = Z n
substA (V x) pattern replacement = if x == pattern then replacement else V x
substA (Add a1 a2) pattern replacement = Add (substA a1 pattern replacement) (substA a2 pattern replacement)
substA (Mul a1 a2) pattern replacement = Mul (substA a1 pattern replacement) (substA a2 pattern replacement)
substA (Sub a1 a2) pattern replacement = Sub (substA a1 pattern replacement) (substA a2 pattern replacement)

-- Substitution function for Bexp
substB :: Bexp -> Var -> Aexp -> Bexp
substB BTrue _ _ = BTrue
substB BFalse _ _ = BFalse
substB (Eq a1 a2) pattern replacement = Eq (substA a1 pattern replacement) (substA a2 pattern replacement)
substB (Leq a1 a2) pattern replacement = Leq (substA a1 pattern replacement) (substA a2 pattern replacement)
substB (Not b) pattern replacement = Not (substB b pattern replacement)
substB (And b1 b2) pattern replacement = And (substB b1 pattern replacement) (substB b2 pattern replacement)

-- Substitution function for states
substState :: State -> Var -> Int -> State
substState sigma variable replacement input
    | input == variable = replacement
    | otherwise = sigma input

-- Natural semantic for While
naturalSemantic :: Statement -> State -> State
naturalSemantic (Assign x a) sigma = substState sigma x (fancyA a sigma)
naturalSemantic Skip sigma = sigma
naturalSemantic (Colon s1 s2) sigma = naturalSemantic s2 (naturalSemantic s1 sigma)
naturalSemantic (If b s1 s2) sigma = if fancyB b sigma then naturalSemantic s1 sigma else naturalSemantic s2 sigma
naturalSemantic (While b s) sigma = if fancyB b sigma then naturalSemantic (While b s) (naturalSemantic s sigma) else sigma

main :: IO()
main = do
    -- Example 1
    print "Example 1 (Terminates if initial x is greater or equal to 1)"
    print ("x: " ++ show (sigma "x"))
    print ("y: " ++ show (sigma "y"))
    let example1 = naturalSemantic (While (Not (Eq (V "x") (Z 1))) (Colon (Assign "y" (Mul (V "y") (V "x"))) (Assign "x" (Sub (V "x") (Z 1))))) sigma
    print ("x: " ++ show (example1 "x"))
    print ("y: " ++ show (example1 "y"))

    -- Example 2
    print "Example 2 (Terminates for all initial x)"
    print ("x: " ++ show (sigma "x"))
    print ("y: " ++ show (sigma "y"))
    let example2 = naturalSemantic (While (Leq (Z 1) (V "x")) (Colon (Assign "y" (Mul (V "y") (V "x"))) (Assign "x" (Sub (V "x") (Z 1))))) (substState sigma "x" (fancyA (Z 1) sigma))
    print ("x: " ++ show (example2 "x"))
    print ("y: " ++ show (example2 "y"))

    -- Example 3 (Endless loop)
    print "Example 3 (Endless loop)"
    print ("x: " ++ show (sigma "x"))
    print ("y: " ++ show (sigma "y"))
    let example3 = naturalSemantic (While BTrue Skip) sigma
    print (example3 "x")
    print (example3 "y")
