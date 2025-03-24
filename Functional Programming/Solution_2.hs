{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use second" #-}
module Proceso (Procesador, AT(Nil,Tern), RoseTree(Rose), Trie(TrieNodo), foldAT, foldRose, foldTrie, procVacio, procId, procCola, procHijosRose, procHijosAT, procRaizTrie, procSubTries, unoxuno, sufijos, inorder, preorder, postorder, preorderRose, hojasRose, ramasRose, caminos, palabras, ifProc,(++!), (.!)) where

import Test.HUnit
import Control.Arrow (Arrow(first, second))
import Data.Maybe (isJust)


--Definiciones de tipos

type Procesador a b = a -> [b]


-- Árboles ternarios
data AT a = Nil | Tern a (AT a) (AT a) (AT a) deriving Eq
--E.g., at = Tern 1 (Tern 2 Nil Nil Nil) (Tern 3 Nil Nil Nil) (Tern 4 Nil Nil Nil)
--Es es árbol ternario con 1 en la raíz, y con sus tres hijos 2, 3 y 4.

-- RoseTrees
data RoseTree a = Rose a [RoseTree a] deriving Eq
--E.g., rt = Rose 1 [Rose 2 [], Rose 3 [], Rose 4 [], Rose 5 []] 
--es el RoseTree con 1 en la raíz y 4 hijos (2, 3, 4 y 5)

-- Tries
data Trie a = TrieNodo (Maybe a) [(Char, Trie a)] deriving Eq
-- E.g., t = TrieNodo (Just True) [('a', TrieNodo (Just True) []), ('b', TrieNodo Nothing [('a', TrieNodo (Just True) [('d', TrieNodo Nothing [])])]), ('c', TrieNodo (Just True) [])]
-- es el Trie Bool de que tiene True en la raíz, tres hijos (a, b, y c), y, a su vez, b tiene como hijo a d.


-- Definiciones de Show

instance Show a => Show (RoseTree a) where
    show = showRoseTree 0
      where
        showRoseTree :: Show a => Int -> RoseTree a -> String
        showRoseTree indent (Rose value children) =
            replicate indent ' ' ++ show value ++ "\n" ++
            concatMap (showRoseTree (indent + 2)) children

instance Show a => Show (AT a) where
    show = showAT 0
      where
        showAT :: Show a => Int -> AT a -> String
        showAT _ Nil = replicate 2 ' ' ++ "Nil"
        showAT indent (Tern value left middle right) =
            replicate indent ' ' ++ show value ++ "\n" ++
            showSubtree (indent + 2) left ++
            showSubtree (indent + 2) middle ++
            showSubtree (indent + 2) right

        showSubtree :: Show a => Int -> AT a -> String
        showSubtree indent subtree =
            case subtree of
                Nil -> replicate indent ' ' ++ "Nil\n"
                _   -> showAT indent subtree

instance Show a => Show (Trie a) where
    show = showTrie ""
      where
        showTrie :: Show a => String -> Trie a -> String
        showTrie indent (TrieNodo maybeValue children) =
            let valueLine = case maybeValue of
                                Nothing -> indent ++ "<vacío>\n"
                                Just v  -> indent ++ "Valor: " ++ show v ++ "\n"
                childrenLines = concatMap (\(c, t) -> showTrie (indent ++ "  " ++ [c] ++ ": ") t) children
            in valueLine ++ childrenLines


--Ejercicio 1
procVacio :: Procesador a b
procVacio _ = []

procId :: Procesador a a
procId a = [a]

procCola :: Procesador [a] a
procCola (x:xs) = xs

procHijosRose :: Procesador (RoseTree a) (RoseTree a)
procHijosRose (Rose _ hijos) = hijos

procHijosAT :: Procesador (AT a) (AT a)
procHijosAT Nil = []
procHijosAT (Tern _ h1 h2 h3) = [h1,h2,h3]

procRaizTrie :: Procesador (Trie a) (Maybe a)
procRaizTrie (TrieNodo raiz _) = [raiz]

procSubTries :: Procesador (Trie a) (Char, Trie a)
procSubTries (TrieNodo _ hijos) = hijos


--Ejercicio 2

foldAT :: b -> (a -> b -> b -> b -> b) -> AT a -> b
foldAT cBase cRec arbol = case arbol of
    Nil -> cBase
    (Tern nodo izq medio der) -> cRec nodo (rec izq) (rec medio) (rec der)
    where rec = foldAT cBase cRec

foldRose :: (a -> [b] -> b) -> RoseTree a -> b
foldRose cRec (Rose raiz hijos) = cRec raiz (map (foldRose cRec) hijos)

foldTrie :: (Maybe a -> [(Char, b)] -> b) -> Trie a -> b
foldTrie cRec (TrieNodo raiz hijos) = cRec raiz (map (\(c,t) -> (c, foldTrie cRec t)) hijos)


--Ejercicio 3
unoxuno :: Procesador [a] [a]
unoxuno = map (: [])

sufijos :: Procesador [a] [a]
sufijos = foldr (\x rec -> (x : head rec) : rec ) [[]]


--Ejercicio 4
preorder :: AT a -> [a]
preorder = foldAT [] (\x ri rm rd -> x : (ri ++ rm ++ rd))

inorder :: AT a -> [a]
inorder = foldAT [] (\x ri rm rd -> ri ++ rm ++ [x] ++ rd)

postorder :: AT a -> [a]
postorder = foldAT [] (\x ri rm rd -> ri ++ rm ++ rd ++ [x])


--Ejercicio 5

preorderRose :: Procesador (RoseTree a) a
preorderRose = foldRose (\x rec -> if null rec then [x] else x : concat rec)

hojasRose :: Procesador (RoseTree a) a
hojasRose = foldRose (\x rec -> if null rec then [x] else concat rec)

ramasRose :: Procesador (RoseTree a) [a]
ramasRose = foldRose (\x rec -> if null rec then [[x]] else map (x :) (concat rec))


--Ejercicio 6

caminos :: Trie a -> [String]
caminos = foldTrie (\value listarec -> if null listarec
                                          then [""]
                                          else "":concatMap (\(c, strings) -> map (c:) strings) listarec)


--Ejercicio 7

palabras :: Trie a -> [String]
palabras t = if isJust (fst (palabrasConRaiz t)) then "" : snd (palabrasConRaiz t) else snd (palabrasConRaiz t)

palabrasConRaiz :: Trie a -> (Maybe a, [String])
palabrasConRaiz = foldTrie (\value listarec -> (value, concatMap (\(c, (v, strings)) -> if isJust v
                                                                                        then [c] : map (c:) strings
                                                                                        else map (c:) strings)
                                                                  listarec ))

--Ejercicio 8
-- 8.a)
ifProc :: (a->Bool) -> Procesador a b -> Procesador a b -> Procesador a b
ifProc f p1 p2 a = if f a then p1 a else p2 a

-- 8.b)
(++!) :: Procesador a b -> Procesador a b -> Procesador a b
(++!) p1 p2 a = p1 a ++ p2 a

-- 8.c)
(.!) :: Procesador b c -> Procesador a b -> Procesador a c
(.!) p1 p2 a = concatMap p1 (p2 a)


--Ejercicio 9
-- Se recomienda poner la demostración en un documento aparte, por claridad y prolijidad, y, preferentemente, en algún formato de Markup o Latex, de forma de que su lectura no sea complicada.


{-Tests-}

main :: IO Counts
main = do runTestTT allTests

tern1 = Tern 16
      (Tern 1 (Tern 9 Nil Nil Nil) (Tern 7 Nil Nil Nil) (Tern 2 Nil Nil Nil))
      (Tern 14 (Tern 0 Nil Nil Nil) (Tern 3 Nil Nil Nil) (Tern 6 Nil Nil Nil))
      (Tern 10 (Tern 8 Nil Nil Nil) (Tern 5 Nil Nil Nil) (Tern 4 Nil Nil Nil))
tern1Hijos = [
      Tern 1 (Tern 9 Nil Nil Nil) (Tern 7 Nil Nil Nil) (Tern 2 Nil Nil Nil),
      Tern 14 (Tern 0 Nil Nil Nil) (Tern 3 Nil Nil Nil) (Tern 6 Nil Nil Nil),
      Tern 10 (Tern 8 Nil Nil Nil) (Tern 5 Nil Nil Nil) (Tern 4 Nil Nil Nil)]

rose1 = Rose 20 [
      Rose 19 [
        Rose 18 [],
        Rose 17 [Rose 0 []]
      ],
      Rose 15 [
        Rose 10 []
      ],
      Rose 7 []
    ]

rose1Hijos = [
      Rose 19 [
        Rose 18 [],
        Rose 17 [Rose 0 []]
      ],
      Rose 15 [
        Rose 10 []
      ],
      Rose 7 []
    ]

trie1 = TrieNodo Nothing
    [ ('a', TrieNodo (Just True) []),
      ('b', TrieNodo Nothing
        [('a', TrieNodo (Just True)
          [('d', TrieNodo Nothing [])])
        ]),
      ('c', TrieNodo (Just True) [])
    ]

trie1subtries =
    [ ('a', TrieNodo (Just True) []),
      ('b', TrieNodo Nothing
        [('a', TrieNodo (Just True)
          [('d', TrieNodo Nothing [])])
        ]),
      ('c', TrieNodo (Just True) [])
    ]

trie2 = TrieNodo (Just 2)
    [ ('a', TrieNodo (Just 3) []),
      ('b', TrieNodo Nothing
        [('a', TrieNodo (Just 4)
          [('d', TrieNodo Nothing [])])
        ]),
      ('c', TrieNodo (Just 1) [])
    ]

allTests = test [ -- Reemplazar los tests de prueba por tests propios
  "ejercicio1" ~: testsEj1,
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6,
  "ejercicio7" ~: testsEj7,
  "ejercicio8a" ~: testsEj8a,
  "ejercicio8b" ~: testsEj8b,
  "ejercicio8c" ~: testsEj8c
  ]
testsEj1 = test [ -- Casos de test para el ejercicio 1
  procVacio [1,2,3]
    ~=? ([] :: [[Int]]),
  procId [1,2,3]
    ~=? [[1,2,3]],
  procCola [1,2,3]
    ~=? [2,3],
  procHijosRose rose1
    ~=? rose1Hijos,
  procHijosAT tern1
    ~=? tern1Hijos,
  procRaizTrie trie1
    ~=? [Nothing],
  procRaizTrie trie2
    ~=? [Just 2],
  procSubTries trie1
    ~=? trie1subtries
  ]

testsEj2 = test [ -- Casos de test para el ejercicio 2
  foldAT 0 (\_ i m d -> 1 + i + m + d) tern1 -- foldAT debe recorrer 13 nodos del at
    ~=? 13,
  foldRose (\_ rec -> if null rec then 1 else 1 + sum rec) rose1 -- foldRose debe recorrer 8 nodos del rose tree
    ~=? 8,
  foldTrie (\value listarec -> if null listarec
                                          then 1
                                          else 1 + sum (map snd listarec)) trie1 -- foldTrie debe recorrer 6 nodos del trie
    ~=? 6
  ]

testsEj3 = test [ -- Casos de test para el ejercicio 3
  unoxuno [3,1,4,1,5,9]
    ~=? [[3],[1],[4],[1],[5],[9]],
  sufijos "Plp"
    ~=? ["Plp", "lp", "p", ""]
  ]

testsEj4 = test [ -- Casos de test para el ejercicio 4
  preorder tern1
    ~=? [16,1,9,7,2,14,0,3,6,10,8,5,4],
  postorder tern1
    ~=? [9,7,2,1,0,3,6,14,8,5,4,10,16],
  inorder tern1
    ~=? [9,7,1,2,0,3,14,6,16,8,5,10,4]
  ]

testsEj5 = test [ -- Casos de test para el ejercicio 5
  preorderRose rose1
    ~=? [20,19,18,17,0,15,10,7],
  hojasRose rose1
    ~=? [18,0,10,7],
  ramasRose rose1
    ~=? [[20,19,18],[20,19,17,0],[20,15,10],[20,7]]
  ]

testsEj6 = test [ -- Casos de test para el ejercicio 6
  caminos trie1
    ~=? ["", "a", "b", "ba", "bad", "c"]
  ]

testsEj7 = test [ -- Casos de test para el ejercicio 7
  palabras trie1
    ~=? ["a", "ba", "c"],
  palabras trie2
    ~=? ["","a", "ba", "c"]
  ]

testsEj8a = test [ -- Casos de test para el ejercicio 7
  ifProc null procVacio procCola [1,2,3]
    ~=? [2,3],
  ifProc null procVacio procCola []
    ~=? ([] :: [[Int]])
  ]
testsEj8b = test [ -- Casos de test para el ejercicio 7
  (postorder ++! preorder) tern1
    ~=? [9,7,2,1,0,3,6,14,8,5,4,10,16,16,1,9,7,2,14,0,3,6,10,8,5,4]
  ]
testsEj8c = test [ -- Casos de test para el ejercicio 7
  ((\z->[0..z]) .! map (+1)) [1,3]
    ~=? [0,1,2,0,1,2,3,4]
  ]
