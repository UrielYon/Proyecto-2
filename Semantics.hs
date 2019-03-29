module Semantics where

import Syntax

--Tipo que representa la interpretacion de un simbolo de funcion
type IntF a = Nombre -> [a] -> a

--Tipo que representa la interpretacion de un simbolo de predicado
type IntR a = Nombre -> [a] -> Bool

--Tipo que representa un estado de las variables
type Estado a = Ind -> a


--Tipo que representa una estructura
type Estructura a = ([a] , IntF a , IntR a)



actEst :: Estado a -> Ind -> a -> Estado a
actEst e x n = ne
  where ne y = if x == y then n else e y


--Funcion que devuelve la interpretacion de un termino dado un estado
--de las variables
iTerm :: Estado a -> IntF a -> Term -> a
iTerm e iF t = case t of
  V x -> e x
  F f lt -> iF f [iTerm e iF t | t <- lt]

--Funcion que dad auna estructura y un estado de las variables,
-- determina la validez de una formula.
iForm :: Eq a => Estructura a -> Estado a -> Form -> Bool
iForm str e phi  = case phi of 
	 FalseF -> False
	 TrueF -> True 
	 Pr p lt -> iR p(map(iTerm e iF)lt) where (_,iF,iR) = str
	 Eq t1 t2 -> (iTerm e iF t1) == (iTerm e iF t2) where (_,iF,_) = str
	 Neg p -> not (iForm str e p)
	 Conj p q -> (iForm str e p) && (iForm str e p)
	 Disy p q -> (iForm str e p) || (iForm str e p)
	 Imp p q -> iForm str e (Disy (Neg p) q)
	 Equi p q -> iForm str e (Conj (Imp p q) (Imp q p ))
	 All x p -> and [(iForm str (actEst e x m) p) | m <- u] where (u,_,_) = str
	 Ex x p -> or [(iForm str (actEst e x m) p) | m <- u] where (u,_,_) = str
