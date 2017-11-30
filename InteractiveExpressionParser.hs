import Data.Char
import Data.List
import Parsing
import System.IO


--Problem 1--
type Env = [(String, Int)]

data Binop = Add | Sub | Mul | Div | Mod deriving (Eq, Show)
data Expr = Bin Binop Expr Expr | Val Int | Var String deriving (Eq, Show)

getValue :: (a , b) -> b
getValue  (x,y) = y

getItem :: (a, b) -> a
getItem (x,y) = x

retrieveEnv :: Env -> String -> Maybe Int
retrieveEnv [] y   = Nothing
retrieveEnv (x:xs) y = if getItem x == y then Just (getValue x)
                       else retrieveEnv xs y 

myAdd :: Maybe Int -> Maybe Int -> Maybe Int
myAdd Nothing _ = Nothing 
myAdd _ Nothing = Nothing
myAdd (Just x) (Just y) = Just (x + y)

myDivide :: Maybe Int -> Maybe Int -> Maybe Int
myDivide Nothing _ = Nothing
myDivide _ Nothing = Nothing
myDivide (Just x) (Just y) = Just (div x y)

myMultiple :: Maybe Int -> Maybe Int -> Maybe Int
myMultiple Nothing _ =Nothing
myMultiple _ Nothing =Nothing
myMultiple (Just x ) (Just y) =Just (x * y)

mySubstract :: Maybe Int -> Maybe Int -> Maybe Int
mySubstract Nothing _ =Nothing
mySubstract _ Nothing = Nothing
mySubstract (Just x) (Just y) = Just (x - y)

myMod :: Maybe Int -> Maybe Int -> Maybe Int
myMod Nothing _ =Nothing
myMod _ Nothing =Nothing
myMod (Just x) (Just y)= Just (mod x y) 

eval :: Env -> Expr -> Maybe Int
eval xs (Val x)       = Just x
eval xs (Var x)       = if retrieveEnv xs x /= Nothing then retrieveEnv xs x
                        else Nothing
eval xs (Bin Add e1 e2) = myAdd (eval xs e1) (eval xs e2)
eval xs (Bin Sub e1 e2) = mySubstract (eval xs e1) (eval xs e2)
eval xs (Bin Mul e1 e2) = myMultiple (eval xs e1) (eval xs e2)
eval xs (Bin Div e1 e2) = if eval xs e2 == Just 0 then Nothing
                          else myDivide (eval xs e1) (eval xs e2)
eval xs (Bin Mod e1 e2) = if eval xs e2 == Just 0  then Nothing   
                          else myMod (eval xs e1) (eval xs e2)

--Problem 2--

--I just assume that the identifier's format is right, so I won't do some check--            
pFactor :: Parser Expr
pFactor = (do char '('
              e <- pExpr
              char ')'
              return e ) +++ (do x <- integer
                                 return (Val x)) +++ (do x <-  ident
                                                         return (Var x))

--the left-hand association, so the pOpfactor needs to return a value--
pOpfactor :: Expr -> Parser Expr
pOpfactor ex = (do
                   char '*'
                   y <- pFactor
                   (do
                      z <- pOpfactor (Bin Mul ex y)
                      return z) +++ return (Bin Mul ex y)) +++ (do 
                                                                   char '/'
                                                                   y <- pFactor
                                                                   (do z<- pOpfactor (Bin Div ex y)
                                                                       return z) +++ return (Bin Div ex y)) +++ (do  
                                                                                                                   char '%'
                                                                                                                   y <- pFactor
                                                                                                                   (do z <- pOpfactor(Bin Mod ex y)
                                                                                                                       return z) +++ return (Bin Mod ex y))


pTerm :: Parser Expr
pTerm = do x <- pFactor
           (do y <- pOpfactor x
               return y) +++ return x

pOpTerm :: Expr -> Parser Expr
pOpTerm ex = (do 
                 char '+'
                 y <- pTerm
                 (do z <- pOpTerm (Bin Add ex y)
                     return z ) +++ return (Bin Add ex y)) +++ (do 
                                                                   char '-'
                                                                   y <- pTerm
                                                                   (do z <- pOpTerm (Bin Sub ex y)
                                                                       return z) +++ return (Bin Sub ex y))

pExpr :: Parser Expr 
pExpr = do x <- pTerm
           (do y <- pOpTerm x
               return y ) +++ return x

getLength :: [a] -> Int
getLength [] = 0
getLength (x:xs) = 1 + getLength xs 

--Problem 3--
runParser :: Parser a -> String -> Maybe a
runParser p [] = Nothing
runParser p ns = if ((getLength xs) == 0) then Nothing
                 else if getValue (head xs) /= [] then Nothing
                 else Just (getItem (head xs)) 
                 where 
                  xs = parse p (removeSpace ns)

--Problem 4--
data Instr = IVal Int | IBin Binop | IVar String deriving (Eq, Show)
type Stack = [Int]
type Prog  = [Instr]

head' ::[Maybe a]->Maybe a
head' (x:xs) =x
head' [] = Nothing


removeHead :: [a] -> [a]
removeHead [] = []  
removeHead (x:xs) = xs 

--for example [IVal 1, IVal 2, IBin Add] getOperator returns [Add], getOperand returns [1,2]--
getOperator :: Prog -> [Instr]
getOperator xs = [ x | x <-xs , getType x ==3] 

getType :: Instr -> Int
getType (IVar x) = 1
getType (IVal x) = 2
getType (IBin x) = 3

istrToMaybeInt:: Instr -> Maybe Int
istrToMaybeInt (IVal x) = Just x
_ = Nothing

parseVar :: Instr -> String
parseVar (IVar x) = x 

getOperand :: Prog -> Env -> [Maybe Int]
getOperand [] es = [] 
getOperand (x:xs) es 
           | getType x == 1 = [retrieveEnv es (parseVar x)] ++ getOperand xs es
           | getType x == 2 = [istrToMaybeInt x] ++ getOperand xs es 
           | getType x == 3 = getOperand xs es

runProg :: Prog -> Env -> Maybe Int 
runProg xs es = runProgHelp cs os
                  where
                    cs = getOperand xs es
                    os = getOperator xs 

runProgHelp:: [Maybe Int] -> Prog -> Maybe Int
runProgHelp [] is = Nothing
runProgHelp xs [] = if getLength xs /= 1 then Nothing
                    else head xs
runProgHelp xs is = if a == Nothing then Nothing
                    else runProgHelp ([a] ++ (removeHead (removeHead xs))) (removeHead is)
                    where
                      a = apply (head is) (head' xs) (head' (removeHead xs))

apply :: Instr ->Maybe Int ->Maybe Int -> Maybe Int 
apply _ Nothing _ = Nothing
apply _ _ Nothing = Nothing
apply (IBin Add) x y = myAdd y x 
apply (IBin Sub) x y = mySubstract y x 
apply (IBin Mul) x y = myMultiple y x
apply (IBin Div) x y = myDivide y x
apply (IBin Mod) x y = myMod y x

--Problem 5--
compile :: Expr -> Prog
compile (Bin Sub a b)= compile b ++ compile a ++ [IBin Sub]
compile (Bin Add a b)= compile b ++ compile a ++ [IBin Add]
compile (Bin Mul a b)= compile b ++ compile a ++ [IBin Mul] 
compile (Bin Div a b)= compile b ++ compile a ++ [IBin Div]
compile (Val a) = [IVal a]
compile (Var a) = [IVar a] 


--Problem 6--
getType' ::Maybe Expr -> Int
getType' (Just (Val a)) = 1
getType' (Just (Var a)) = 2
getType' _     = 3 

calculate :: Expr -> Int
calculate (Var a) = 0 
calculate (Val a) = a
calculate (Bin Add e1 e2) = calculate e1 + calculate e2
calculate (Bin Sub e1 e2) = calculate e1 - calculate e2
calculate (Bin Mul e1 e2) = calculate e1 * calculate e2
calculate (Bin Div e1 e2) = div (calculate e1)  (calculate e2)
calculate (Bin Mod e1 e2) = mod (calculate e1) (calculate e2)

optimize :: Expr -> Maybe Expr
optimize (Val x) = Just (Val x)
optimize (Var x) = Just (Var x)
optimize (Bin Sub e1 (Val 0)) = optimize e1
optimize (Bin Add e1 (Val 0)) = optimize e1
optimize (Bin Add (Val 0) e2) = optimize e2 
optimize (Bin Mul (Val 0) e2) = Just (Val 0)
optimize (Bin Mul e1 (Val 0)) = Just (Val 0)
optimize (Bin Sub e1 e2) = if (getType' (optimize e1) == 1) && (getType' (optimize e2)== 1) then Just (Val (calculate e1 - calculate e2))
                         else if (getType' (optimize e1) == 1) &&( getType' (optimize e2) == 2 ) then Just (Bin Sub (Val (calculate e1)) e2)
                         else if (getType' (optimize e1) == 2) && (getType' ( optimize e2)== 1 ) && (calculate e2 /= 0) then Just (Bin Sub e1 (Val (calculate e2)))
                         else if (getType' (optimize e1) == 2) && (getType' ( optimize e2)== 1 ) && (calculate e2 == 0) then (optimize e1)
                         else Nothing
optimize (Bin Add e1 e2) = if (getType' (optimize e1) == 1) && (getType' (optimize e2)== 1)  then Just (Val (calculate e1 + calculate e2))
                         else if (getType' (optimize e1) == 1) &&( getType' (optimize e2) == 2 ) && (calculate e1 /= 0) then Just (Bin Add (Val (calculate e1)) e2)
                         else if (getType' (optimize e1) == 1) &&( getType' (optimize e2) == 2 ) && (calculate e1 == 0) then (optimize e2)
                         else if (getType'  (optimize e1) == 2) && (getType' ( optimize e2)== 1 ) && (calculate e2 /= 0) then Just (Bin Add e1 (Val (calculate e2)))
                         else if (getType' (optimize e1) == 2) && (getType' ( optimize e2)== 1 ) && (calculate e2 == 0) then (optimize e1)
                         else Nothing
optimize (Bin Mul e1 e2) = if (getType' (optimize e1) == 1) && (getType' (optimize e2)== 1) then Just (Val (calculate e1 + calculate e2))
                         else if (getType' (optimize e1) == 1) &&( getType' (optimize e2) == 2 ) && (calculate e1 /= 0) then Just (Bin Mul (Val (calculate e1)) e2)
                         else if (getType' (optimize e1) == 1) &&( getType' (optimize e2) == 2 ) && (calculate e1 == 0) then Just (Val 0)
                         else if (getType'  (optimize e1) == 2) && (getType' ( optimize e2)== 1 ) && (calculate e2 /= 0) then Just (Bin Mul e1 (Val (calculate e2)))
                         else if (getType' (optimize e1) == 2) && (getType' ( optimize e2)== 1 ) && (calculate e2 == 0) then Just (Val 0)
                         else Nothing
optimize (Bin Div e1 e2) = if (getType' ( optimize e2)) == 1 && (calculate e2 ==0) then Nothing
                         else if (getType' (optimize e1) == 1) && (getType' (optimize e2)== 1) then Just (Val (calculate e1 + calculate e2))
                         else if (getType' (optimize e1) == 1) &&( getType' (optimize e2) == 2 ) then Just (Bin Div (Val (calculate e1)) e2)
                         else if (getType'  (optimize e1) == 2) && (getType' ( optimize e2)== 1 )then Just (Bin Div e1 (Val (calculate e2)))
                         else Nothing

--Problem 7--
main :: IO ()
main = do 
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout NoBuffering
  repl []

repl :: Env -> IO ()
repl env = do putStr "\n>"
              line <- getLine
              dispatch env line

quit :: IO ()
quit = return ()

loop :: String -> Env -> IO ()
loop str env = do putStrLn str
                  repl env

dispatch :: Env -> String -> IO()
dispatch env line 
             | removeSpace line == "quit" = quit 
             | removeSpace line == "env"  = do showEnv (sort env)
                                               loop [] env
             | (parse pLetExpr (removeSpace line)) /= [] = let result = eval env ex
                                                               ex = getValue item
                                                               char = getItem item
                                                               item = head (getItem temp)
                                                               temp = head (parse pLetExpr (removeSpace line))
                                                            in case result of 
                                                               Nothing -> whenWrong line env
                                                               Just a -> do putStr ([char] ++ " = " ++ (show a))
                                                                            loop [] (addToEnv env ([char], a))
             | (parse pVar (removeSpace line)) /= []&& (parse pVar (removeSpace line)) /= [("","")] = let value =retrieveEnv env [var]
                                                                                                          var = (head (getItem (head (parse pVar (removeSpace line)))))
                                                                                                      in case value of 
                                                                                                      Nothing -> whenWrong line env
                                                                                                      Just a -> do putStr ([var] ++ " = " ++ (show a))
                                                                                                                   loop [] env
             | (parse pDel (removeSpace line)) /= []&& (parse pDel (removeSpace line)) /= [("","")] = let vardel =head (getItem (head (parse pDel (removeSpace line))))
                                                                                                      in if (hasElem env vardel) == True 
                                                                                                         then 
                                                                                                         do 
                                                                                                           putStr ("Deleted" ++ " "++ [vardel])
                                                                                                           loop [] (removeElement env vardel)
                                                                                                         else whenWrong line env                               
              | (parse pExpr (removeSpace line)) /= []= let result = eval env (getItem (head (parse pExpr (removeSpace line))))
                                                        in case result of 
                                                        Nothing -> whenWrong line env
                                                        Just a -> do 
                                                                     putStr ("ans" ++ "=" ++ (show a))
                                                                     loop [] env                       

dispatch env _ = whenWrong [] env

whenWrong :: String -> Env -> IO()
whenWrong line env= do putStr "Error"
                       loop [] env

showEnv :: Env -> IO()
showEnv []    = do putStr ""
showEnv (e:env) = do putStrLn ((getItem e) ++ " = " ++ (show (getValue e)))
                     showEnv env

addToEnv :: Env -> (String,Int)-> Env
addToEnv es (s,i)= if retrieveEnv es s /= Nothing then addToEnvTool es (s,i)
                   else es ++ [(s,i)]

addToEnvTool :: Env -> (String,Int)-> Env
addToEnvTool (e:es) (s,i) = if (getItem e) == s then [(s,i)] ++ es
                        else addToEnvTool es (s,i)                      

pLetExpr :: Parser [(Char,Expr)]
pLetExpr = do l <- item 
              e <- item 
              t <- item
              c <- item
              eq <- item
              expr <- pExpr
              if ([l] ++ [e] ++ [t]) == "let" then return [(c,expr)]
              else return []

pVar :: Parser [Char]
pVar = do v <- item
          a <- item
          r <- item
          var <- item
          if ([v] ++ [a] ++ [r]) == "var" then return [var]
          else return []

pDel :: Parser [Char]
pDel = do  d <- item 
           e <- item 
           l <- item
           var <- item 
           if ([d]++ [e] ++ [l] )== "del" then return [var]
           else return []

hasElem :: Env -> Char -> Bool
hasElem (x:xs) y = ((getItem x) == [y])|| hasElem xs y

removeElement :: Env -> Char -> Env 
removeElement (e:es) a= if getItem e == [a] then es
                        else e:(removeElement es a)

sortEnv :: Env -> Env
sortEnv es = sortEnvTool es vs
             where vs= sort (extractEnvVar es)

sortEnvTool:: Env -> [String] -> Env
sortEnvTool es xs = [ (x,retrieveEnv' es x) | x <- xs]

retrieveEnv' :: Env -> String -> Int
retrieveEnv' (x:xs) y = if getItem x == y then (getValue x)
                        else retrieveEnv' xs y

extractEnvVar::Env -> [String]
extractEnvVar [] = [] 
extractEnvVar es = [ getItem e| e <- es ]



removeSpace :: String -> String
removeSpace [] = [] 
removeSpace (x:xs) = if x == ' ' then removeSpace xs
                   else x:removeSpace xs 
