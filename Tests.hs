module Tests where

import Test.HUnit
import Solucion

run = runtestTT testSuiteEj1
run2 = runtestTT testSuiteEj2
run3 = runtestTT testSuiteEj3
run4 = runtestTT testSuiteEj4

testSuiteEj1 = test [
    -- cuando la lista de usaurios es vacia - vacio
    -- un usuario - solo ese nombre
    -- usuarios con dos nombres distintos - los 2 nombres
    -- usuarios con dos nombres iguales - el nombre 1 sola vez
    ]

testSuiteEj2 = test [
    -- un solo usuario - devuelve lista vacia
    -- 2 usuarios, una relacion - uno amigo del otro
    -- 3 usuarios, dos relaciones - devuelve 2 usuarios amigo del primero
    -- sin relaciones - devuelve lista vacia
    -- hay relaciones, pero el usuario no pertenece - lista vacia
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