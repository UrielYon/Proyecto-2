module Ejemplo where

import Syntax
import Semantics


--Funciones




--Funcion que nos indica cuando un animal es enemigo del otro.
enem :: String -> String -> Bool
enem "Perro" b = case b of 
	"Gato" -> True
	_-> False
	
enem "Perico" b = case b of 
	"Gato" -> True
	_-> False
enem "Gato" b = case b of
	"Perro" -> True
	_-> False


--Predicados
odia x y = enem x y == True
amigos x y = enem x y == False
gato x = (x == "Gato") == True 


-- interpretacion de los simbolos de predicado
iP :: IntR String
iP p l = case p of
	"Od" -> odia (l!!0) (l!!1)
	"Am" -> amigos (l!!0) (l!!1)
	"Kt" -> gato (l!!0)

est = id :: Int -> Int

main = do
 --Universo de animales.
  let univ = ["Perro", "Gato", "Perico"]

--Todos los gatos tienen un perro que los odia.
  let p1 = All 1 (Conj (Eq (V 1) (F "Gato" [])) (Ex 2 (Eq (F "Od" [V 1,V 2]) (F "Perro" []))))
 
  
  print $ iForm (univ, iF, iP) est p1
