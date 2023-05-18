module Solucion where

-- Completar con los datos del grupo
--
-- Nombre de Grupo: xx
-- Integrante 1: Nombre Apellido, email, LU
-- Integrante 2: Valentin Aguilar, valentinaguila80@gmail.com , LU
-- Integrante 3: Jorge Cordido, jorgecordido1@gmail.com , 639/23
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
                  
cadenaDeAmigos :: [Usuario] -> RedSocial -> Bool
cadenaDeAmigos (x:xs) r| longitud (xs) == 0 = False
                       | relacionadosDirecto x (head xs) r = True
                       | otherwise = cadenaDeAmigos xs r      
                       
-- EJERCICIOS 
-- describir que hace la funcion: Dada una red social, utiliza su lista de usuarios, para devolver sus nombres en una lista recursivamente
nombreDeUsuarios :: RedSocial -> [[Char]] 
nombreDeUsuarios x = eliminarrepetidos (proyectarNombres (usuarios x))

proyectarNombres :: [Usuario] -> [[Char]] -- dada una secuencia de usuarios me devuelve una secuencia de sus nombres
proyectarNombres [] = []
proyectarNombres (x:xs) = nombreDeUsuario x : proyectarNombres xs

-- describir qué hace la función: dado un usuario devuelve una lista de usuarios relacionados con el
amigosDe :: RedSocial -> Usuario -> [Usuario] 
amigosDe r x = eliminarrepetidos (amigosDe2 (relaciones r) x)

amigosDe2 :: [Relacion] -> Usuario -> [Usuario]
amigosDe2 [] y = []
amigosDe2 (x:xs) y | pertenece y (relAusuarios x) == True = [amigosDeaux x y] ++ amigosDe2 xs y
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

-- describir qué hace la función: dada una red social y un usuario, mide la longitud de la lista de amigos del usuario
cantidadDeAmigos :: RedSocial -> Usuario -> Integer
cantidadDeAmigos x y = longitud (amigosDe x y)

-- describir qué hace la función: Dada una red social, compara recursivamente el primer elemento de la lista de usuarios con el siguiente, se queda con el mayor y lo compara con el siguiente, hasta que la lista termina
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos r = usuarioConMasAmigosaux r (usuarios r) (primerusuario r)

primerusuario :: RedSocial -> Usuario
primerusuario x = head (usuarios x)

usuarioConMasAmigosaux :: RedSocial -> [Usuario] -> Usuario -> Usuario
usuarioConMasAmigosaux r (x:xs) y | longitud xs == 0 = y
                                  | (cantidadDeAmigos r (head xs)) < (cantidadDeAmigos r y) = usuarioConMasAmigosaux r xs y
                                  | otherwise = usuarioConMasAmigosaux r xs (head xs)

-- describir qué hace la función: Dada una red social, toma su lista de usuarios y prueba recursivamente en cada uno de ellos, si existe un usuario con mas de 10 amigos devuelve true
estaRobertoCarlos :: RedSocial -> Bool 
estaRobertoCarlos x = estaRobertoCarlosaux x (usuarios x)

estaRobertoCarlosaux :: RedSocial -> [Usuario] -> Bool
estaRobertoCarlosaux r [] = False
estaRobertoCarlosaux r (x:xs) | cantidadDeAmigos r x > 10 = True
                              | otherwise = estaRobertoCarlosaux r xs

-- describir qué hace la función: dado un usuario devuelve una lista de sus publicaciones
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe r x = publicacionesDeaux (publicaciones r) x 

publicacionesDeaux :: [Publicacion] -> Usuario -> [Publicacion] 
publicacionesDeaux (x:xs) y | (longitud xs == 0) && (y == (usuarioDePublicacion x)) = [x]
                            | (longitud xs == 0) && (y /= (usuarioDePublicacion x)) = []
                            | y == (usuarioDePublicacion x) = [x] ++ publicacionesDeaux xs y
                            | otherwise = publicacionesDeaux xs y

-- describir qué hace la función: Dado una red social y un usuario, toma la lista de publicaciones en la red y crea recursivamente una lista de las publicaciones que le gustan al usuario
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA r u = publicacionesQueLeGustanAaux (publicaciones r) u

publicacionesQueLeGustanAaux :: [Publicacion] -> Usuario -> [Publicacion]
publicacionesQueLeGustanAaux [] _ = []
publicacionesQueLeGustanAaux (x:xs) u | pertenece u (likesDePublicacion x) = x : publicacionesQueLeGustanAaux xs u 
                                      | otherwise = publicacionesQueLeGustanAaux xs u   
                                      
-- describir qué hace la función: Dados dos usuarios, la funcion se fija si a los dos usuarios les gustan las mismas publicaciones.
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones r u u2 | mismosElementos (publicacionesQueLeGustanA r u) (publicacionesQueLeGustanA r u2) = True
                                       | otherwise = False  
    

-- describir qué hace la función: Dado un usuario, la función evalua si el usuario tiene un seguidor fiel, es decir un usario que puso like en todas las publicaciones
-- ver el caso en el que el usuario es su propio seguidor fiel
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel r u = tieneUnSeguidorFielaux (publicacionesDe r u) (likesDePublicacion (head (publicacionesDe r u)))

tieneUnSeguidorFielaux :: [Publicacion] -> [Usuario] -> Bool
tieneUnSeguidorFielaux [] _ = False
tieneUnSeguidorFielaux _ [] = False 
tieneUnSeguidorFielaux (x:xs) (u:us) | longitud xs == 0  && pertenece u (likesDePublicacion x) = True
                                     | pertenece u (likesDePublicacion x) = tieneUnSeguidorFielaux xs [u] 
                                     | otherwise = tieneUnSeguidorFielaux (x:xs) us    


-- describir qué hace la función: .....
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos r u1 u2 |(u1 == u2 && cantidadDeAmigos r u1 >= 1) = True 
                                |otherwise = existeSecuenciaDeAmigosSub r u1 u2 (amigosDe r u1) [u1]

existeSecuenciaDeAmigosSub :: RedSocial -> Usuario -> Usuario -> [Usuario]-> [Usuario] -> Bool
existeSecuenciaDeAmigosSub _ _ _ [] z = False 
existeSecuenciaDeAmigosSub r u1 u2 (x:xs) z| u1==u2 = False
                                           | pertenece u2 (x:xs) = True  
                                           | otherwise = existeSecuenciaDeAmigosSub r u1 u2 (amigosDePero r x (z++[x])) (z++[x]) || existeSecuenciaDeAmigosSub r u1 u2 xs (z++[x]) 

amigosDePero :: RedSocial -> Usuario -> [Usuario] -> [Usuario] 
amigosDePero r x z = eliminarrepetidos (amigosDePero2 (relaciones r) x z)

amigosDePero2 :: [Relacion] -> Usuario -> [Usuario] -> [Usuario]
amigosDePero2 [] y z = []
amigosDePero2 (x:xs) y z | pertenece y (relAusuarios x)  == True = (quitarTodosl [amigosDeaux x y] z) ++ amigosDePero2 xs y z
                         | otherwise = amigosDePero2 xs y z
                         
quitarTodosl :: [Usuario] -> [Usuario] -> [Usuario]
quitarTodosl [] _ = []
quitarTodosl (x:xs) y | pertenece x y = quitarTodosl xs y 
                      | otherwise = [x]++quitarTodosl xs y                        

