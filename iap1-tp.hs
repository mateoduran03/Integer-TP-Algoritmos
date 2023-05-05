-- Completar con los datos del grupo
--
-- Nombre de Grupo: xx
-- Integrante 1: Nombre Apellido, email, LU
-- Integrante 2: Nombre Apellido, email, LU
-- Integrante 3: Nombre Apellido, email, LU
-- Integrante 4: Nombre Apellido, email, LU

type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])

-- Funciones basicas

usuarios :: RedSocial -> [Usuario]
usuarios (us, _, _) = us

relaciones :: RedSocial -> [Relacion]
relaciones (_, rs, _) = rs

publicaciones :: RedSocial -> [Publicacion]
publicaciones (_, _, ps) = ps

idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id 

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre 

usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

likesDePublicacion :: Publicacion -> [Usuario]
likesDePublicacion (_, _, us) = us

-- Ejercicios

-- Predicados auxiliares
pertenece :: Eq t => t -> [t] -> Bool
pertenece n [] = False
pertenece n (x:xs) |n == x = True 
                   |otherwise = pertenece n xs

mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos [] [] = True
mismosElementos []  _ = False
mismosElementos _ []  = False
mismosElementos (x:xs) ys = pertenece x ys && mismosElementos xs (quitartodos x ys)

quitartodos :: (Eq t) => t -> [t] -> [t]
quitartodos x xs | not (pertenece x xs) = xs
                 | pertenece x xs && not (hayRepetidos xs) = quitar x xs
                 | otherwise = quitartodos x (quitar x xs)

quitar :: (Eq t) => t -> [t] -> [t]
quitar x xs | not (pertenece x xs) = xs
            | pertenece x xs && x == head xs = tail xs
            | otherwise = [head xs] ++ quitar x (tail xs)

hayRepetidos :: (Eq t) => [t] -> Bool
hayRepetidos ls     | longitud ls <= 1 = False
hayRepetidos (x:xs) | pertenece x xs = True
                    | otherwise= hayRepetidos (head xs:tail xs)

longitud :: [t] -> Integer
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

sinRepetidos :: Eq t => [t] -> [t] -> Bool
sinRepetidos (x:xs) y | longitud (x:xs) == 0 = True
                      | pertenece x y == True = False
                      | otherwise = sinRepetidos xs y

noHayIdsRepetidos :: [Usuario] -> Bool
noHayIdsRepetidos (x:xs) | longitud (x:xs) <= 1 = True
                         | pertenece (idDeUsuario x) (tomarIds xs) = False  
                         | otherwise = noHayIdsRepetidos xs 
                                            
--Toma la lista de todos los ids de una lista de usuarios
tomarIds :: [Usuario] -> [Integer]
tomarIds [] = []
tomarIds (x:xs) = idDeUsuario x : nohayrepeAux xs                        

relacionesAsimetricas :: [Relacion] -> Bool
relacionesAsimetricas (x:xs) | longitud (x:xs) == 1 = True
                             | pertenece (relacionesAsimetricasaux x) xs == True = False
                             | otherwise =  relacionesAsimetricas xs 
             
relacionesAsimetricasaux:: Relacion -> Relacion
relacionesAsimetricasaux (x , y) = (y , x) 

usuarioValido :: Usuario -> Bool
usuarioValido n | idDeUsuario n > 0 && espalabra n  = True
                | otherwise = False 

espalabra :: Usuario -> Bool
espalabra n | longitud (nombreDeUsuario n) > 0 && ((head (nombreDeUsuario n) /= ' ')) = True
            | otherwise = False

--Consultar caso base profesores.
usuariosValidos :: [Usuario] -> Bool 
usuariosValidos [] = True 
usuariosValidos (x:xs) | usuarioValido x && noHayIdsRepetidos (x:xs) = usuariosValidos xs
                       | otherwise = False 

noHayRelacionesRepetidas :: [Relacion] -> Bool
noHayRelacionesRepetidas x = noHayRelacionesRepetidas2 x x

noHayRelacionesRepetidas2 :: [Relacion] -> [Relacion] -> Bool   
noHayRelacionesRepetidas2 (x:xs) y | longitud (x:xs) == 1 = True
                                   | noHayRelacionesRepetidasaux x y == False = False
                                   | otherwise = noHayRelacionesRepetidas2 xs y

noHayRelacionesRepetidasaux :: Relacion -> [Relacion] -> Bool
noHayRelacionesRepetidasaux x (y:ys) | pertenece x ys == True = False
                                     | otherwise = True
                                     
sonDeLaRed :: RedSocial -> [Usuario] -> Bool
sonDeLaRed n [] = True
sonDeLaRed n (x:xs) | pertenece x (usuarios n) = sonDeLaRed n xs
                    | otherwise = False 

-- EJERCICIOS 




-- describir qué hace la función: .....
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe = undefined

-- describir qué hace la función: .....
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos = undefined

-- describir qué hace la función: .....
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos = undefined

-- describir qué hace la función: .....
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos = undefined

-- describir qué hace la función: .....
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe = undefined

-- describir qué hace la función: .....
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA = undefined

-- describir qué hace la función: .....
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones = undefined

-- describir qué hace la función: .....
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel = undefined

-- describir qué hace la función: .....
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos = undefined
