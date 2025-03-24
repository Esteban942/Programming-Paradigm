import Test.HUnit

{-- Tipos --}

import Data.Either
import Data.List

data Dirección = Norte | Sur | Este | Oeste
  deriving (Eq, Show)
type Posición = (Float, Float)

data Personaje = Personaje Posición String  -- posición inicial, nombre
  | Mueve Personaje Dirección               -- personaje que se mueve, dirección en la que se mueve
  | Muere Personaje                         -- personaje que muere
  deriving (Eq, Show)
data Objeto = Objeto Posición String        -- posición inicial, nombre
  | Tomado Objeto Personaje                 -- objeto que es tomado, personaje que lo tomó
  | EsDestruido Objeto                      -- objeto que es destruido
  deriving (Eq, Show)
type Universo = [Either Personaje Objeto]

{-- Observadores y funciones básicas de los tipos --}

siguiente_posición :: Posición -> Dirección -> Posición
siguiente_posición p Norte = (fst p, snd p + 1)
siguiente_posición p Sur = (fst p, snd p - 1)
siguiente_posición p Este = (fst p + 1, snd p)
siguiente_posición p Oeste = (fst p - 1, snd p)

posición :: Either Personaje Objeto -> Posición
posición (Left p) = posición_personaje p
posición (Right o) = posición_objeto o

posición_objeto :: Objeto -> Posición
posición_objeto = foldObjeto const (const posición_personaje) id

nombre :: Either Personaje Objeto -> String
nombre (Left p) = nombre_personaje p
nombre (Right o) = nombre_objeto o

nombre_personaje :: Personaje -> String
nombre_personaje = foldPersonaje (const id) const id

está_vivo :: Personaje -> Bool
está_vivo = foldPersonaje (const (const True)) (const (const True)) (const False)

fue_destruido :: Objeto -> Bool
fue_destruido = foldObjeto (const (const False)) const (const True)

universo_con :: [Personaje] -> [Objeto] -> [Either Personaje Objeto]
universo_con ps os = map Left ps ++ map Right os

es_un_objeto :: Either Personaje Objeto -> Bool
es_un_objeto (Left o) = False
es_un_objeto (Right p) = True

es_un_personaje :: Either Personaje Objeto -> Bool
es_un_personaje (Left o) = True
es_un_personaje (Right p) = False

-- Asume que es un personaje
personaje_de :: Either Personaje Objeto -> Personaje
personaje_de (Left p) = p

-- Asume que es un objeto
objeto_de :: Either Personaje Objeto -> Objeto
objeto_de (Right o) = o

en_posesión_de :: String -> Objeto -> Bool
en_posesión_de n = foldObjeto (const (const False)) (\ r p -> nombre_personaje p == n) (const False)

objeto_libre :: Objeto -> Bool
objeto_libre = foldObjeto (const (const True)) (const (const False)) (const False)

norma2 :: (Float, Float) -> (Float, Float) -> Float
norma2 p1 p2 = sqrt ((fst p1 - fst p2) ^ 2 + (snd p1 - snd p2) ^ 2)

cantidad_de_objetos :: Universo -> Int
cantidad_de_objetos = length . objetos_en

cantidad_de_personajes :: Universo -> Int
cantidad_de_personajes = length . personajes_en

distancia :: (Either Personaje Objeto) -> (Either Personaje Objeto) -> Float
distancia e1 e2 = norma2 (posición e1) (posición e2)

objetos_libres_en :: Universo -> [Objeto]
objetos_libres_en u = filter objeto_libre (objetos_en u)

está_el_personaje :: String -> Universo -> Bool
está_el_personaje n = foldr (\x r -> es_un_personaje x && nombre x == n && (está_vivo $ personaje_de x) || r) False

está_el_objeto :: String -> Universo -> Bool
está_el_objeto n = foldr (\x r -> es_un_objeto x && nombre x == n && not (fue_destruido $ objeto_de x) || r) False

-- Asume que el personaje está
personaje_de_nombre :: String -> Universo -> Personaje
personaje_de_nombre n u = foldr1 (\x1 x2 -> if nombre_personaje x1 == n then x1 else x2) (personajes_en u)

-- Asume que el objeto está
objeto_de_nombre :: String -> Universo -> Objeto
objeto_de_nombre n u = foldr1 (\x1 x2 -> if nombre_objeto x1 == n then x1 else x2) (objetos_en u)

es_una_gema :: Objeto -> Bool
es_una_gema o = isPrefixOf "Gema de" (nombre_objeto o)

{-Ejercicio 1-}

foldPersonaje :: (Posición -> String -> a) -> (a -> Dirección -> a) -> (a -> a) -> Personaje -> a
foldPersonaje fBase fMueve fMuere (Personaje pos nombre) = fBase pos nombre
foldPersonaje fBase fMueve fMuere (Mueve personaje direccion) = fMueve (foldPersonaje fBase fMueve fMuere personaje) direccion
foldPersonaje fBase fMueve fMuere (Muere personaje) = fMuere (foldPersonaje fBase fMueve fMuere personaje)

foldObjeto :: (Posición -> String -> a) -> (a -> Personaje -> a) -> (a -> a) -> Objeto -> a
foldObjeto fBase fTomado fDestruido (Objeto pos nombre) = fBase pos nombre
foldObjeto fBase fTomado fDestruido (Tomado objeto personaje) = fTomado (foldObjeto fBase fTomado fDestruido objeto) personaje
foldObjeto fBase fTomado fDestruido (EsDestruido objeto) = fDestruido (foldObjeto fBase fTomado fDestruido objeto)

{-Ejercicio 2-}

--Asumimos que un personaje muerto no se puede mover
posición_personaje :: Personaje -> Posición
posición_personaje = foldPersonaje const siguiente_posición id

nombre_objeto :: Objeto -> String
nombre_objeto = foldObjeto (const id) const id

{-Ejercicio 3-}

--Asumimos que un objeto destruido pertenece al universo.
objetos_en :: Universo -> [Objeto]
objetos_en = foldr (\x rec -> if es_un_objeto x then objeto_de x : rec else rec) []

--Asumimos que un personaje muerto pertenece al universo.
personajes_en :: Universo -> [Personaje]
personajes_en = foldr (\x rec -> if es_un_personaje x then personaje_de x : rec else rec) []

{-
Demostración de la siguiente propiedad:
∀ u :: Universo . ∀ o :: Objeto . elem o (objetos_en u) ⇒ elem (Right o) u
Sabemos que Universo es de tipo [Either Personaje Objeto], entones es equivalente a pedir:
∀ ys :: [Either Personaje Objeto] . ∀ o :: Objeto . elem o (objetos_en ys) ⇒ elem (Right o) ys
Vamos a probarlo por inducción estructural sobre lista.
∀ ys :: [Either Personaje Objeto] vale P(ys), donde 
P(ys) = ∀ o :: Objeto. elem o (objetos_en ys) ⇒ elem (Right o) ys

- Caso base P([]), tenemos: 
    elem o (objetos_en []) ⇒ elem (Right o) []
  Por definicion de objetos_en:
    elem o (foldr (\x rec -> if es_un_objeto x then objeto_de x : rec else rec) [] [])
  Por definición de foldr:
    elem o [] ⇒ elem (Right o) []
  elem o [], por definición de elem, devuelve False. Nos queda:
    False ⇒ elem (Right o) []
  Lo cual es verdadero.

- Caso inductivo. ∀ ys :: [Either Personaje Objeto]. ∀ y :: Either Personaje Objeto. (P(ys) ⇒ P(y:ys))
  HI: P(ys)
  Qvq: P(y:ys)
  Tenemos
    elem o (objetos_en (y:ys)) ⇒ elem (Right o) (y:ys)
  En el caso donde lo de la izquierda es Falso, la implicación vale trivialmente.
  Entonces, veamos el caso donde lo de la izquierda es Verdadero.
  Por definicion de objetos_en:
    elem o (foldr (\x rec -> if es_un_objeto x then objeto_de x : rec else rec) [] (y:ys)) ⇒ elem (Right o) (y:ys)
  Por definición de foldr (llamaremos F al primer parametro de foldr para simplificar):
    elem o (F y foldr F [] ys) ⇒ elem (Right o) (y:ys)
  Por F, tenemos dos casos. Si 'y' es un objeto, o si no.
  - Caso 'y' es objeto:
      elem o ((objeto_de y):(foldr F [] ys)) ⇒ elem (Right o) (y:ys)
    Por definición de elem:
      o == (objeto_de y) || elem o (foldr F [] ys) ⇒ elem (Right o) (y:ys)
    - Caso o == (objeto_de y):
        Del lado derecho, como estamos en el caso de que 'y' es objeto, lo escribimos como y = (Right o),
        pues por Either, o == (objeto_de y) sii (Right o) == y :
          True || elem o (foldr F [] ys) ⇒ elem (Right o) ((Right o):ys)
        Por || :
          True ⇒ elem (Right o) ((Right o):ys)
        Por definición de elem:
          True ⇒ (Right o) == (Right o) || elem (Right o) ys
        Por == :
          True ⇒ True || elem (Right o) ys
        Por lo tanto este caso es verdadero.
    - Caso o != (objeto_de y):
      Tenemos:
        False || elem o (foldr F [] ys) ⇒ elem (Right o) (y:ys)
      Por ||, resta ver que:
        elem o (foldr F [] ys) ⇒ elem (Right o) (y:ys)
      Recordemos que (foldr F [] ys) = objetos_en ys
        elem o (objetos_en ys) ⇒ elem (Right o) (y:ys)
      Por definicion de elem:
        elem o (objetos_en ys) ⇒ (Right o) == y || elem (Right o) ys
      Como estamos en el caso  o != (objeto_de y) que vale sii (Right o) != y :
        elem o (objetos_en ys) ⇒ False || elem (Right o) ys
      Por || :
        elem o (objetos_en ys) ⇒ elem (Right o) ys
      Y vale por nuestra hipótesis inductiva.
  - Caso 'y' no es objeto:
    Tenemos:
      elem o ((foldr F [] ys)) ⇒ elem (Right o) (y:ys)
    Que es lo que acabamos de probar en el paso anterior.

Como vale el caso base y el caso inductivo, concluimos que vale la propiedad.
-}

{-Ejercicio 4-}

-- Asumimos que el personaje por el que se pregunta está en el universo.
-- Asumimos que si el personaje está muerto, entonces no posee ningún objeto.
-- Asumimos que no se pueden tomar objetos que ya estén destruidos.
objetos_en_posesión_de :: String -> Universo -> [Objeto]
objetos_en_posesión_de nombre universo = foldr (\x rec -> if en_posesión_de nombre x then x : rec else rec) [] (objetos_en universo)

{-Ejercicio 5-}

-- Asumimos que hay al menos un objeto libre.
-- Asumimos que un objeto destruido no es un objeto libre.
objeto_libre_mas_cercano :: Personaje -> Universo -> Objeto
objeto_libre_mas_cercano nombre universo = foldr1 (\x y ->  if distancia (Right x) (Left nombre) < distancia (Right y) (Left nombre) 
                                                            then x 
                                                            else y) (objetos_libres_en universo)

{-Ejercicio 6-}

-- Asumimos que no hay gemas repetidas y que podrían haber más de 6.
tiene_thanos_todas_las_gemas :: Universo -> Bool
tiene_thanos_todas_las_gemas u = está_el_personaje "Thanos" u && cant_gemas_de_thanos (objetos_en_posesión_de "Thanos" u) >= 6

cant_gemas_de_thanos :: [Objeto] -> Int
cant_gemas_de_thanos = foldr (\x rec -> if es_una_gema x then rec + 1 else rec) 0

{-Ejercicio 7-}

-- Asumimos que si Thanos no está en el universo, entonces podemos ganarle.
-- Los personajes para ganar (Thor o Wanda y Vision) deben estar vivos.
-- Los objetos para ganar no deben estar destruidos.
podemos_ganarle_a_thanos :: Universo -> Bool
podemos_ganarle_a_thanos u = not (está_el_personaje "Thanos" u) || (not (tiene_thanos_todas_las_gemas u) && (caso_thor u || caso_wanda_vision u))

caso_thor :: Universo -> Bool
caso_thor u = está_el_personaje "Thor" u && está_el_objeto "Stormbreaker" u

caso_wanda_vision :: Universo -> Bool
caso_wanda_vision u = está_el_personaje "Wanda" u && está_el_personaje "Visión" u && está_el_objeto "Gema de la Mente" u &&
                      en_posesión_de "Visión" (objeto_de_nombre "Gema de la Mente" u)

{-Tests-}

main :: IO Counts
main = do runTestTT allTests

allTests = test [
  "ejercicio1" ~: testsEj1,
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6,
  "ejercicio7" ~: testsEj7
  ]

-- Personajes de prueba:
phil = Personaje (0,0) "Phil"
thor = Personaje (0,0) "Thor"
thanos = Personaje (0,0) "Thanos"
wanda = Personaje (0,0) "Wanda"
vision = Personaje (0,0) "Visión"
ironMan = Mueve (Mueve (Mueve (Personaje (0,0) "IronMan") Oeste) Oeste) Norte

-- Objetos de prueba:
stormbreaker = Objeto (1,1) "Stormbreaker"
gemaMenteSinTomar = Objeto (2,2) "Gema de la Mente"
gemaMenteVision = Tomado (Objeto (3,3) "Gema de la Mente") vision
gemaMente = Tomado (Objeto (2,2) "Gema de la Mente") thanos
gemaEspacio = Tomado (Objeto (3,3) "Gema del Espacio") thanos
gemaTiempo = Tomado (Objeto (1,1) "Gema del Tiempo") thanos
gemaAlma = Tomado (Objeto (0,0) "Gema del Alma") thanos
gemaRealidad = Tomado (Objeto (0,0) "Gema de la Realidad") thanos
gemaPoder = Tomado (Objeto (5,5) "Gema del Poder") thanos
objDestruido = EsDestruido (Tomado (Objeto (0,0) "Gema de la Mente") vision)
mjölnir = Objeto (2,2) "Mjölnir"

-- Universos de prueba:
universo_sin_thanos = universo_con [phil] [mjölnir]
universo_gana_thanos = universo_con [thanos, thor] [gemaAlma, gemaEspacio, gemaMente, gemaPoder, gemaRealidad, gemaTiempo]
universo_casi_gana_thanos = universo_con [thanos, thor] [gemaAlma, gemaEspacio, gemaMente, gemaPoder, gemaRealidad]
universo_podemos_ganar_thor = universo_con [thanos, thor] [gemaAlma, gemaEspacio, gemaTiempo, gemaPoder, gemaRealidad, gemaMenteSinTomar, stormbreaker]
universo_podiamos_ganar_pero_murio_thor = universo_con [thanos, Muere thor] [gemaAlma, gemaEspacio, gemaTiempo, gemaPoder, gemaRealidad, gemaMenteSinTomar, stormbreaker]
universo_podemos_ganar_wanda_vision = universo_con [thanos, wanda, vision] [gemaAlma, gemaEspacio, gemaTiempo, gemaPoder, gemaRealidad, gemaMenteVision]
universo_ya_perdimos = universo_con [thanos, wanda, vision, thor] [gemaAlma, gemaEspacio, gemaTiempo, gemaPoder, gemaRealidad, gemaMente, stormbreaker]
universo_solo_thanos = universo_con [thanos, thor] [gemaAlma, gemaEspacio, gemaTiempo, gemaPoder, gemaRealidad, gemaMenteSinTomar]
universo_test_distancia = universo_con [thanos, wanda] [stormbreaker, gemaMenteSinTomar, gemaAlma, objDestruido]

testsEj1 = test [ -- Casos de test para el ejercicio 1
  foldPersonaje (\p s -> 0) (\r d -> r+1) (\r -> r+1) phil             -- Caso de test 1 - expresión a testear
    ~=? 0                                                               -- Caso de test 1 - resultado esperado
  ,
  foldPersonaje (\p s -> 0) (\r d -> r+1) (\r -> r+1) (Muere phil)     -- Caso de test 2 - expresión a testear
    ~=? 1,                                                               -- Caso de test 2 - resultado esperado
  foldObjeto (\p s -> 0) (\r d -> r+1) (\r -> r+1) objDestruido
    ~=? 2,
  foldPersonaje (\p s -> 0) (\r d -> r+1) (\r -> r+1) ironMan
    ~=? 3,
  foldPersonaje (\p s -> 0) (\r d -> r+1) (\r -> r+1) (Muere ironMan)
    ~=? 4,
  foldObjeto (\p s -> 0) (\r d -> r+1) (\r -> r+1) gemaAlma
    ~=? 1
  ]

testsEj2 = test [ -- Casos de test para el ejercicio 2
  posición_personaje phil       -- Caso de test 1 - expresión a testear
    ~=? (0,0),                   -- Caso de test 1 - resultado esperado
  posición_personaje ironMan
    ~=? (-2,1),
  posición_personaje (Muere ironMan)
    ~=? (-2,1),
  nombre_objeto objDestruido
    ~=? "Gema de la Mente",
  nombre_objeto gemaAlma
    ~=? "Gema del Alma",
  nombre_objeto stormbreaker
    ~=? "Stormbreaker"
  ]

testsEj3 = test [ -- Casos de test para el ejercicio 3
  objetos_en []       -- Caso de test 1 - expresión a testear
    ~=? [],            -- Caso de test 1 - resultado esperado
  objetos_en universo_gana_thanos
    ~=? [gemaAlma, gemaEspacio, gemaMente, gemaPoder, gemaRealidad, gemaTiempo],
  personajes_en []
    ~=? [],
  personajes_en universo_gana_thanos
    ~=? [thanos, thor]
  ]

testsEj4 = test [ -- Casos de test para el ejercicio 4
  objetos_en_posesión_de "Phil" []       -- Caso de test 1 - expresión a testear
    ~=? [],                             -- Caso de test 1 - resultado esperado
  objetos_en_posesión_de "Thanos" universo_podemos_ganar_thor
    ~=? [gemaAlma, gemaEspacio, gemaTiempo, gemaPoder, gemaRealidad],
  objetos_en_posesión_de "Visión" [Right objDestruido, Left vision]
    ~=? []
  ]

testsEj5 = test [ -- Casos de test para el ejercicio 5
  objeto_libre_mas_cercano phil [Right mjölnir]       -- Caso de test 1 - expresión a testear
    ~=? mjölnir,                                       -- Caso de test 1 - resultado esperado
  objeto_libre_mas_cercano wanda universo_test_distancia
    ~=? stormbreaker
  ]

testsEj6 = test [ -- Casos de test para el ejercicio 6
  tiene_thanos_todas_las_gemas universo_sin_thanos       -- Caso de test 1 - expresión a testear
    ~=? False,                                            -- Caso de test 1 - resultado esperado
  tiene_thanos_todas_las_gemas universo_gana_thanos
    ~=? True,
  tiene_thanos_todas_las_gemas universo_casi_gana_thanos
    ~=? False
  ]

testsEj7 = test [ -- Casos de test para el ejercicio 7
  podemos_ganarle_a_thanos universo_sin_thanos         -- Caso de test 1 - expresión a testear
    ~=? True,                                          -- Caso de test 1 - resultado esperado
  podemos_ganarle_a_thanos universo_podemos_ganar_thor
    ~=? True,
  podemos_ganarle_a_thanos universo_podiamos_ganar_pero_murio_thor
    ~=? False, 
  podemos_ganarle_a_thanos universo_podemos_ganar_wanda_vision
    ~=? True,
  podemos_ganarle_a_thanos universo_ya_perdimos
    ~=? False,
  podemos_ganarle_a_thanos universo_solo_thanos
    ~=? False
  ]