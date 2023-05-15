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
    "Caso 1: Un solo usuario" ~: (amigosDe ([(1, "Jorge")],[],[])) ~?= []
    "Caso 2: dos usuarios relacionados" ~: (amigosDe ([(1, "Jorge"), (2, "Juan")],[((1, "Jorge"), (2, "Juan"))],[]) (1, "Jorge"))   ~?= [(2, "Juan")]
    "Caso 3: tres usuarios, dos relacionados" ~: (amigosDe ([(1, "Jorge"), (2, "Juan"), (3, "Ariel")],[((1, "Jorge"), (2, "Juan")), ((1, "Jorge"), (3, "Ariel"))],[]) (1, "Jorge")) ~?= [(2, "Juan"), (3, "Ariel")]
    "Caso 4: sin relaciones" ~: (amigosDe ([(1, "Jorge"), (2, "Juan")],[],[])) ~?= []
    "Caso 5: el usuario no se relaciona" ~: (amigosDe ([(1, "Jorge"), (2, "Juan"), (3, "Ariel")],[((1, "Jorge"), (2, "Juan"))],[]) (3, "Ariel")) ~?= []
    --  
    ]

testSuiteEj3 = test [
    -- un solo usuario - devuelve 0
    -- 2 usuarios, una relacion - devuelve 1
    -- 3 usuarios, dos relaciones - devuelve 2
    -- sin relaciones - devuelve 0
    -- hay relaciones, pero el usuario no pertenece - devuelve 0
    --
    ]

testSuiteEj4 = test [
    -- 1 solo usuario, devuelve a si mismo
    -- 2 usuarios con maxima cantidad de amigos, devuelve uno de los dos
    -- 3 usuarios con la misma cantidad de amigos, devuelve1 de los tre
    -- 3 usuarios, uno con mas amigos que el otro, devuelve el que tiene mas
    -- 2 usuarios, uno con mas amigos que el otro, devuelve el que tiene mas
    -- 
    ]
testSuiteEj5 = test [
    -- lista vacìa - false
    -- 1 usuario - false
    -- 11 usuarios, un usuario con 10 amigos - false
    -- 12 usarios, un usuario con 11 amigos  - true
    ]
    
testSuiteEj6 = test [
    -- usuario sin publicaciones - lista vacia
    -- usuario con una publicacion - esa publicacion
    -- usuario con dos publicaciones iguales - esa publicacion una sola vez
    -- varios usuarios con varias publicaciones - publicaciones del usuario elegido
    -- 
    ]

testSuiteEj7 = test [
    -- sin publicaciones - lista vacia
    -- un solo usuario - lista vacia
    -- usuario, no le gustan las publicaciones - lista vacia
    -- usuarios, le  gusata una publicacion - esa publicacion
    -- 
    ]

testSuiteEj8 = test [
    -- sin publicaciones - false
    -- no les gustan las mismas publicaciones - false
    -- les gusta las mismas publicaciones - true
    -- hay 2 que les gustan las mismas publicaciones - true
    ]

testSuiteEj9 = test [
    -- no tiene likes - false
    -- tiene seguidor fiel - true
    -- un solo usuario - false
    -- no le gustan todas la publicaciones - false

    ]

testSuiteEj10 = test [

    ]
