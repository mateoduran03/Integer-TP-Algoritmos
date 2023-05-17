module Tests where

import Test.HUnit
import Solucion

run = runtestTT testSuiteEj1
run2 = runtestTT testSuiteEj2
run3 = runtestTT testSuiteEj3
run4 = runtestTT testSuiteEj4
run5 = runtestTT testSuiteEj5
run6 = runtestTT testSuiteEj6
run7 = runtestTT testSuiteEj7
run8 = runtestTT testSuiteEj8
run9 = runtestTT testSuiteEj9
run10 = runtestTT testSuiteEj10

testSuiteEj1 = test [
    "Caso 1: Usuarios vacios" ~: (nombreDeUsuarios ([],[],[])) ~?= []
    "Caso 2: Un solo usuario" ~: (nombreDeUsuarios ([(1, "Jorge")],[],[])) ~?= ["Jorge"]
    "Caso 3: Dos usuarios con nombres distintos" ~: (nombreDeUsuarios ([(1, "Jorge"), (2, "Juan")],[],[])) ~?= ["Jorge", "Juan"]
    "Caso 4: Dos usuarios con nombres iguales" ~: (nombreDeUsuarios ([(1, "Jorge"), (2, "Jorge")],[],[])) ~?= ["Jorge"]
    ]

testSuiteEj2 = test [
    "Caso 1: Un solo usuario" ~: (amigosDe ([(1, "Jorge")],[],[]) (1, "Jorge")) ~?= []
    "Caso 2: dos usuarios relacionados" ~: (amigosDe ([(1, "Jorge"), (2, "Juan")],[((1, "Jorge"), (2, "Juan"))],[]) (1, "Jorge"))   ~?= [(2, "Juan")]
    "Caso 3: tres usuarios, dos relacionados" ~: (amigosDe ([(1, "Jorge"), (2, "Juan"), (3, "Ariel")],[((1, "Jorge"), (2, "Juan")), ((1, "Jorge"), (3, "Ariel"))],[]) (1, "Jorge")) ~?= [(2, "Juan"), (3, "Ariel")]
    "Caso 4: sin relaciones" ~: (amigosDe ([(1, "Jorge"), (2, "Juan")],[],[])) ~?= []
    "Caso 5: el usuario no se relaciona" ~: (amigosDe ([(1, "Jorge"), (2, "Juan"), (3, "Ariel")],[((1, "Jorge"), (2, "Juan"))],[]) (3, "Ariel")) ~?= []
    ]

testSuiteEj3 = test [
    "Caso 1: un solo usuario" ~: (cantidadDeAmigos ([(1, "Jorge")],[],[]) (1, "Jorge")) ~?= 0
    "Caso 2: dos usuarios relacionados" ~: (cantidadDeAmigos ([(1, "Jorge"), (2, "Juan")],[((1, "Jorge"), (2, "Juan"))],[]) (1, "Jorge")) ~?= 1
    "Caso 3: tres usuarios, dos relacionados" ~: (cantidadDeAmigos ([(1, "Jorge"), (2, "Juan"), (3, "Ariel")],[((1, "Jorge"), (2, "Juan")), ((1, "Jorge"), (3, "Ariel"))],[]) (1, "Jorge")) ~?= 2 
    "Caso 4: sin relaciones" ~: (cantidadDeAmigos ([(1, "Jorge"), (2, "Juan")],[],[])) ~?= 0
    "Caso 5: el usuario no se relaciona" ~: (cantidadDeAmigos ([(1, "Jorge"), (2, "Juan"), (3, "Ariel")],[((1, "Jorge"), (2, "Juan"))],[]) (3, "Ariel")) ~?= 0
    ]

testSuiteEj4 = test [
    "Caso 1: un usuario en la red" ~: (usuarioConMasAmigos ([(1, "Jorge")],[],[])) ~?= (1, "Jorge")
    "Caso 2: dos usuarios con misma cantidad de amigos" ~: (usuarioConMasAmigos ([(1, "Jorge"), (2, "Juan"), (3, "Ariel"), (4, "Lautaro")],[((1, "Jorge"), (2, "Juan")), ((4, "Lautaro"), (3, "Ariel"))],[])) ~?= (1, "Jorge")
    "Caso 3: un usuario con mas cantidad de amigos" ~: (usuarioConMasAmigos ([(1, "Jorge"), (2, "Juan"), (3, "Ariel"), (4, "Lautaro"), (5, "Augusto")],[((1, "Jorge"), (2, "Juan")), ((4, "Lautaro"), (3, "Ariel")), ((1, "Jorge"), (5, "Augusto"))],[])) ~?= (1, "Jorge")
    ]
testSuiteEj5 = test [
    "Caso 1: Red vacía" ~: (estaRobertoCarlos ([],[],[])) ~?= False
    "Caso 2: No hay relaciones" ~: (estaRobertoCarlos ([(1, "Jorge")],[],[])) ~?= False
    "Caso 3: No hay usuario con mas de 10 amigos" ~: (estaRobertoCarlos ([(1, "Jorge"), (2, "Juan"), (3, "Ariel"), (4, "Lautaro"), (5, "Augusto"), (6, "Tomas"), (7, "Valentin"), (8, "Mateo"), (9, "Camila"), (10, "Vicky"), (11, "Angie"), (12, "Santiago")],[((1, "Jorge"), (2, "Juan")), ((1, "Jorge"), (3, "Ariel")), ((1, "Jorge"), (5, "Augusto")), ((1, "Jorge"), (6, "Tomas")), ((1, "Jorge"), (7, "Valentin")), ((1, "Jorge"), (8, "Mateo")), ((1, "Jorge"), (9, "Camila")), ((1, "Jorge"), (10, "Vicky")), ((1, "Jorge"), (11, "Angie")), ((1, "Jorge"), (12, "Santiago"))],[])) ~=?= False
    "Caso 4: Hay usuario con mas de 10 amigos" ~: (estaRobertoCarlos ([(1, "Jorge"), (2, "Juan"), (3, "Ariel"), (4, "Lautaro"), (5, "Augusto"), (6, "Tomas"), (7, "Valentin"), (8, "Mateo"), (9, "Camila"), (10, "Vicky"), (11, "Angie"), (12, "Santiago")],[((1, "Jorge"), (2, "Juan")), ((1, "Jorge"), (3, "Ariel")), ((1, "Jorge"), (5, "Augusto")), ((1, "Jorge"), (6, "Tomas")), ((1, "Jorge"), (7, "Valentin")), ((1, "Jorge"), (8, "Mateo")), ((1, "Jorge"), (9, "Camila")), ((1, "Jorge"), (10, "Vicky")), ((1, "Jorge"), (11, "Angie")), ((1, "Jorge"), (12, "Santiago")), ((1, "Jorge"), (4, "Lauti"))],[])) ~=?= True
    ]
    
testSuiteEj6 = test [
    "Caso 1: Usuario sin publicaciones" ~: (publicacionesDe ([(1, "Jorge")],[],[]) (1, "Jorge")) ~?= []
    "Caso 2: Una publicacion" ~: (publicacionesDe ([(1, "Jorge")],[],[((1, "Jorge"), "Hola",[])]) (1, "Jorge")) ~?= ["Hola"]
    "Caso 3: Dos publicaciones iguales" ~: (publicacionesDe ([(1, "Jorge")],[],[((1, "Jorge"), "Hola",[]), ((1, "Jorge"), "Hola",[])]) (1, "Jorge")) ~?= ["Hola"]
    "Caso 4: Varias publicaciones" ~: (publicacionesDe ([(1, "Jorge"), (2, "Juan")],[],[((1, "Jorge"), "Hola",[]), ((2, "Juan"), "Buen día",[]), ((2, "Juan"), "Nos vemos",[])]) (2, "Juan")) ~?= ["Buen día", "Nos vemos"]
    -- 
    ]

testSuiteEj7 = test [
    "Caso 1: No hay publciaciones" ~: (publicacionesQueLeGustanA ([(1, "Jorge"), (2, "Juan")],[],[]) (1, "Jorge")) ~?= []
    "Caso 2: Al usuario no le gustan las publicaciones" ~: (publicacionesQueLeGustanA ([(1, "Jorge"), (2, "Juan"), (3, "Ariel")],[],[((2, "Juan"), "Buen día",[(3, "Ariel")]), ((2, "Juan"), "Nos vemos",[(3, "Ariel")])]) (1, "Jorge")) ~?= []
    "Caso 3: Al usuario le gustan las publicaciones" ~: (publicacionesQueLeGustanA ([(1, "Jorge"), (2, "Juan"), (3, "Ariel")],[],[((2, "Juan"), "Buen día",[(3, "Ariel")]), ((2, "Juan"), "Nos vemos",[(3, "Ariel")])]) (3, "Ariel")) ~?= ["Buen dia", "Nos vemos"]
    ]

testSuiteEj8 = test [
    "Caso 1: No hay publciaciones" ~: (lesGustanLasMismasPublicaciones ([(1, "Jorge"), (2, "Juan")],[],[]) (1, "Jorge") (2, "Juan")) ~?= False
    "Caso 2: No les gustan las mismas publicaciones" ~: (lesGustanLasMismasPublicaciones ([(1, "Jorge"), (2, "Juan"), (3, "Ariel")],[],[((2, "Juan"), "Buen día",[(3, "Ariel")]), ((2, "Juan"), "Nos vemos",[(1, "Jorge")])]) (1, "Jorge") (3, "Ariel")) ~?= False
    "Caso 3: Les gustan las mismas publicaciones" ~: (lesGustanLasMismasPublicaciones ([(1, "Jorge"), (2, "Juan"), (3, "Ariel")],[],[((2, "Juan"), "Buen día",[(1, "Jorge"), (3, "Ariel")]), ((2, "Juan"), "Nos vemos",[(1, "Jorge"), (3, "Ariel")])]) (1, "Jorge") (3, "Ariel")) ~?= True
    ]

testSuiteEj9 = test [
    "Caso 1: Un usuario" ~: (tieneUnSeguidorFiel ([(1, "Jorge")],[],[((1, "Jorge"), "Hola",[])]) (1, "Jorge")) ~?= False
    "Caso 2: Tiene seguidor fiel" ~: (tieneUnSeguidorFiel ([(1, "Jorge"), (2, "Juan"), (3, "Ariel")],[],[((2, "Juan"), "Buen día",[(1, "Jorge"), (3, "Ariel")]), ((2, "Juan"), "Nos vemos",[(1, "Jorge"), (3, "Ariel")])]) (2, "Juan")) ~?= True
    "Caso 3: Publicacion sin likes" ~: (tieneUnSeguidorFiel ([(1, "Jorge"), (2, "Juan")],[],[((1, "Jorge"), "Hola",[])]) (1, "Jorge")) ~?= False
    "Caso 4: No tiene seguidor fiel" ~: (tieneUnSeguidorFiel ([(1, "Jorge"), (2, "Juan"), (3, "Ariel")],[],[((2, "Juan"), "Buen día",[(1, "Jorge")]), ((2, "Juan"), "Nos vemos",[(3, "Ariel")])]) (2, "Juan")) ~?= False
    ]

testSuiteEj10 = test [

    ]
