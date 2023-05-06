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
tomarIds (x:xs) = idDeUsuario x : tomarIds xs                        

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

relacionadosDirecto :: Usuario -> Usuario -> RedSocial -> Bool
relacionadosDirecto u1 u2 r| pertenece (u1,u2) (relaciones r) = True
                           |pertenece (u2,u1) (relaciones r) = True
                           | otherwise = False
                  
--cadenaDeAmigos LE FALTAN AJUSTES no funciona del todo aun 
cadenaDeAmigos :: [Usuario] -> RedSocial -> Bool
cadenaDeAmigos [] r = False
cadenaDeAmigos (x:xs) r| relacionadosDirecto x (head xs) r = True
                       | otherwise = cadenaDeAmigos xs r   
                       
-- EJERCICIOS 
-- describir que hace la funcion: la funcion me devuelve una secuencia de los nombres de los usuarios en la red social
nombreDeUsuarios :: RedSocial -> [[Char]] 
nombreDeUsuarios x = proyectarNombres (usuarios x)

proyectarNombres :: [Usuario] -> [[Char]] -- dada una secuencia de usuarios me devuelve una secuencia de sus nombres
proyectarNombres [] = []
proyectarNombres (x:xs) = nombreDeUsuario x : proyectarNombres xs

-- describir qué hace la función: dado un usuario devuelve una lista de usuarios relacionados con el
-- idea: dada red, devolver relaciones de red y en cada relacion que contenga al usuario, entraer al usuario2, luego a la lista final le quito repetidos
amigosDe :: RedSocial -> Usuario -> [Usuario] -- (falta testear)
amigosDe x y = eliminarrepetidos (amigosDe2 (relaciones x) y)

amigosDe2 :: [Relacion] -> Usuario -> [Usuario]
amigosDe2 (x:xs) y | longitud (x:xs) == 1 = [amigosDeaux x y]
                   | pertenece y (relAusuarios x) == True = [amigosDeaux x y] ++ amigosDe2 xs y
                   | otherwise = amigosDe2 xs y

relAusuarios :: Relacion -> [Usuario]
relAusuarios (a, b) = [a, b]

amigosDeaux :: Relacion -> Usuario -> Usuario
amigosDeaux (a, b) y | a == y = b
                     | b == y = a
                     
eliminarrepetidos :: (Eq t) => [t] -> [t]
eliminarrepetidos [] = []
eliminarrepetidos (x:xs) | pertenece x xs == True = x : quitartodos x xs
                         | otherwise = x : eliminarrepetidos xs 

-- describir qué hace la función: dado un usuario perteneciente a la red social, devuelve la cantidad de amigos que tiene
cantidadDeAmigos :: RedSocial -> Usuario -> Integer
cantidadDeAmigos x y = longitud (amigosDe x y)


-- describir qué hace la función: dada una redsocial, devuelve el usuario con mas amigos
-- idea: comparo la cantidad de amigos de cada usuario hasta quedarme con el mayor
usuarioConMasAmigos :: RedSocial -> Usuario -- (falta testear)
usuarioConMasAmigos r = usuarioConMasAmigosaux r (usuarios r) (primerusuario r)

primerusuario :: RedSocial -> Usuario
primerusuario x = head (usuarios x)

usuarioConMasAmigosaux :: RedSocial -> [Usuario] -> Usuario -> Usuario
usuarioConMasAmigosaux r (x:xs) y | longitud xs == 0 = y
                                  | (cantidadDeAmigos r x) > (cantidadDeAmigos r y) = usuarioConMasAmigosaux r xs x
                                  | otherwise = usuarioConMasAmigosaux r xs y
                                  

-- describir qué hace la función: si existe un usuario con mas de 1000000 de amigos devuelve true
estaRobertoCarlos :: RedSocial -> Bool -- (falta testear)
estaRobertoCarlos x = estaRobertoCarlosaux x (usuarios x)

estaRobertoCarlosaux :: RedSocial -> [Usuario] -> Bool
estaRobertoCarlosaux r (x:xs) | longitud xs == 0 = False
                              | cantidadDeAmigos r x > 1000000 = True
                              | otherwise = estaRobertoCarlosaux r xs

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
