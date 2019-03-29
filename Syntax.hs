module Syntax where

import Data.List

type Ind = Int 

type Nombre = String


data Term = V Ind | F Nombre [Term] deriving(Show, Eq)

--pr es predicado
data Form = TrueF
		| FalseF
		| Pr Nombre [Term]
		| Eq Term Term
		| Neg Form
		| Conj Form Form
		| Disy Form Form
		| Imp Form Form
		| Equi Form Form
		| All Ind Form
		| Ex Ind Form deriving(Show,Eq)


varT :: Term -> [Ind]
varT term = case term of
	V x -> [x]
	F _ l -> nub(concat [varT t | t <- l])

--free var
fv :: Form -> [Int]
fv phi = case phi of
	TrueF -> []
	FalseF -> []
	Pr _ l -> nub(concat [varT t | t <- l])
	Eq t1 t2 -> union (varT t1) (varT t2)
	Neg p -> fv (p)
	Conj p q -> union (fv p) (fv q)
	Disy p q -> union (fv p) (fv q)
	Imp p q -> union (fv p) (fv q)
	Equi p q -> union (fv p) (fv q)
	All x p -> (fv p) \\ [x]
	Ex x p -> (fv p) \\ [x]


--bound variables
bv :: Form -> [Ind]
bv phi = case phi of 
	TrueF -> []
	FalseF -> []
	Pr _ _ -> []
	Eq _ _ -> []
	Neg p -> bv p
	Conj p q -> union (bv p) (bv q)
	Disy p q -> union (bv p) (bv q)	
	Imp p q -> union (bv p) (bv q)
	Equi p q -> union (bv p) (bv q)
	All x p -> union [x] (bv p)
	Ex x p -> union [x] (bv p)
		

--Cerradura Universal

aC1 :: Form -> Form
aC1 phi = aC1_aux phi (fv phi)


aC1_aux :: Form -> [Ind] -> Form
aC1_aux phi l = case l of 
	[] -> phi
	x:xs -> All x (aC1_aux phi xs)


--Cerradura Existencial

aC2 :: Form -> Form
aC2 phi = aC2_aux phi (fv phi)


aC2_aux :: Form -> [Ind] -> Form
aC2_aux phi l = case l of 
	[] -> phi
	x:xs -> Ex x (aC2_aux phi xs)



type Subst = [(Ind, Term)]

verifSus :: Subst -> Bool
verifSus s = (repetidos [ v| (v, t) <- s])

apsubT :: Term -> Subst -> Term
apsubT t sus = case t of
  V x -> case sus of
    [] -> V x
    (v , t2):xs -> if x == v
                     then t2
                     else apsubT (V x) xs
  F f lt -> F f [apsubT t sus | t <- lt]

apsubF :: Form -> Subst -> Form
apsubF phi sus = case phi of
  TrueF -> TrueF
  FalseF -> FalseF
  Pr p lt -> Pr p [apsubT t sus | t <- lt]
  Eq t1 t2 -> Eq (apsubT t1 sus) (apsubT t2 sus)
  Neg p -> Neg (Neg (apsubF p sus))
  Conj p q -> Conj (apsubF p sus) (apsubF q sus)
  Disy p q -> Disy (apsubF p sus) (apsubF q sus)
  Imp p q -> Imp (apsubF p sus) (apsubF q sus)
  Equi p q -> Equi (apsubF p sus) (apsubF q sus)
  All x p -> if elem x lv
             then error "Sustitucion invalida"
             else All x (apsubF p sus)
    where lv = union xs ts
          (xs, tt) = unzip sus
          ts = concat (map varT tt)





repetidos :: (Eq a) => [a] -> Bool
repetidos l = case l of
	[] -> False
	x:xs -> if (elem x xs)
			then True
			else repetidos xs





