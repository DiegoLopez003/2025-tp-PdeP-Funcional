import Text.Show.Functions()
import Data.List()

-------------------------------
-------- Modelado -------------
-------------------------------

type Auto = (String, String, Desgaste, Float, Float, [String]) 
          --(marca, modelo, desgaste, vel. max., tiempo de carrera, apodos)
type Desgaste = (Float, Float) -- (ruedas, chasis)

-------------------------------
-------- Ejercicio 1 ----------
-------------------------------

ferrari :: Auto
ferrari = ("Ferrari", "F50", (0, 0), 65, 0, ["La nave", "El fierro", "Ferrucho"])

lamborghini :: Auto
lamborghini = ("Lamborghini", "Diablo", (4, 7), 73, 0 , ["Lambo", "La bestia"])

fiat :: Auto
fiat = ("Fiat", "600", (27, 33), 44, 0 , ["La Bocha", "La bolita", "Fitito"])

peugeot :: Auto
peugeot = ("Peugeot", "504", (0,0), 40, 0, ["El rey del desierto"])

--(marca, modelo, desgaste, vel. max., tiempo de carrera, apodos)
-- Desgaste : (ruedas, chasis)

-------------------------------
-------- Ejercicio 2 ----------
-------------------------------

--a)
esPar :: Float -> Bool
esPar = even . round

estaEnBuenEstado :: Auto -> Bool
estaEnBuenEstado ("Peugeot", _, _, _, _, _) = False
estaEnBuenEstado (_, _, (ruedas, chasis), _, tiempo, _) = 
    (esPar tiempo && chasis < 20) || ((not . esPar $ tiempo) && chasis < 40 && ruedas < 60)

--b)
primerApodo :: [String] -> Bool
primerApodo apodos = take 3 (head apodos) == "La "

mayorA80 :: Float -> Bool
mayorA80 = (> 80)

noDaMas :: Auto -> Bool
noDaMas (_, _, _, _, _, []) = False
noDaMas (_, _, (ruedas, chasis), _, _, apodos) = 
    (primerApodo apodos && mayorA80 chasis) || ((not . primerApodo $ apodos) && mayorA80 ruedas)

--c)
esParApodo :: [String] -> Bool
esParApodo = even . length

esChiche :: Auto -> Bool 
esChiche (_, _, (_, chasis), _, _, apodos) = 
    (esParApodo apodos && (chasis < 20)) || ((not . esParApodo) apodos && (chasis < 50))

--d)
niMasDeUnApodo :: [String] -> Bool
niMasDeUnApodo = (<= 1) . length

esJoya :: Auto -> Bool
esJoya (_, _, (0, 0), _, _, apodos) = niMasDeUnApodo apodos --ejem: el tamaño de apodo de ferrari es 3 y peugeot es 1
esJoya _ = False

--e)
nivelChetez :: Auto -> Int
nivelChetez (_, modelo, _, _, _, apodos) = 20 * length apodos * length modelo

--f)
tamanioCabezaListaApodos :: [String] -> Int
tamanioCabezaListaApodos = length . head

capacidadSuper :: Auto -> Int
capacidadSuper (_, _, _, _, _, apodos) = tamanioCabezaListaApodos apodos

--g)

-- Caso de prueba valido para que devuelva lo que tenga que devolver estaEnBuenEstado :
--("Fiat", "600", (27, 50), 44, 3 , ["La Bocha", "La bolita", "Fitito"])
-- o tambien podemos usar --

riesgo :: Auto -> Float
riesgo (marca, modelo, (ruedas, chasis), velMax, tiempo, apodos) 
    | estaEnBuenEstado (marca, modelo, (ruedas, chasis), velMax, tiempo, apodos) = riesgoBase
    | otherwise =  2 * riesgoBase
    where 
        riesgoBase = velMax * ruedas / 10


-------------------------------
-------- Ejercicio 3 ----------
-------------------------------

--(marca, modelo, desgaste, vel. max., tiempo de carrera, apodos)
-- Desgaste : (ruedas, chasis)

--a)

repararAuto :: Auto -> Auto
repararAuto (marca, modelo, (_, chasis), velMax, tiempo, apodos) = (marca, modelo, (0, chasis * 0.15), velMax, tiempo, apodos)

--b)

aplicarPenalidad :: Float -> Auto -> Auto
aplicarPenalidad cantidadSegundos (marca, modelo, (ruedas, chasis), velMax, tiempo, apodos) = (marca, modelo, (ruedas, chasis), velMax, tiempo + cantidadSegundos, apodos)

--c)

ponerNitro :: Auto -> Auto
ponerNitro (marca, modelo, (ruedas, chasis), velMax, tiempo, apodos) = (marca, modelo, (ruedas, chasis), velMax * 1.2, tiempo, apodos)

--d)

bautizarAuto :: String -> Auto -> Auto
bautizarAuto nuevoApodo (marca, modelo, desgaste, velMax, tiempo, apodos) = (marca, modelo, desgaste, velMax, tiempo, apodos ++ [nuevoApodo])

--e)
autoAlDesarmadero :: String -> String -> Auto -> Auto
autoAlDesarmadero marcaNueva modeloNuevo (_,_,desgaste,velMax,tiempo,_) = (marcaNueva, modeloNuevo, desgaste, velMax, tiempo, ["Nunca taxi"])

-------------------------------
-------- Ejercicio 4 ----------
-------------------------------


type Pista = (String, String, Float, [Tramo])
type Tramo = Auto -> Auto 


pista :: Pista
pista  = ("Autodromo", "Argentina", 1000, [curvaPeligrosa, tramoRectoClassic, zigZagLoco, ruloClasico])


type Curva = (Float, Float) -- (angulo, longitud)

--a)
--atravesarCurva :: (Float, Float) -> Auto -> Auto
atravesarCurva :: Curva -> Tramo
atravesarCurva (angulo, longitud) (marca, modelo, (_, chasis), velMax, _, apodos) = 
    (marca, modelo, ((3 * longitud) / angulo, chasis), velMax,  longitud / (velMax / 2), apodos)

curvaPeligrosa :: Tramo
curvaPeligrosa = atravesarCurva (60, 300)

curvaTranca :: Tramo
curvaTranca = atravesarCurva (110, 550)

--b)

atravesarTramoRecto :: Float -> Tramo
atravesarTramoRecto longitud (marca, modelo, (ruedas, chasis), velMax, tiempo, apodos) = 
    (marca, modelo, (ruedas, chasis + ( longitud / 100)), velMax, tiempo + (longitud / velMax), apodos)

tramoRectoClassic :: Tramo
tramoRectoClassic = atravesarTramoRecto 715

tramito :: Tramo
tramito = atravesarTramoRecto 260

--c)

atravesarZigZag :: Float -> Auto -> Auto
atravesarZigZag cantCambiosDireccion (marca, modelo, (ruedas,chasis), velMax, tiempo, apodos) = 
    (marca, modelo, (ruedas + (velMax * (cantCambiosDireccion / 10)), chasis + 5), velMax, tiempo + 3 * cantCambiosDireccion, apodos)

-- Ahora, en vez de tener zigZagLoco como Float, que sea directamente Tramo:
zigZagLoco :: Tramo
zigZagLoco = atravesarZigZag 5

casiCurva :: Tramo
casiCurva = atravesarZigZag 1


--d)

atravesarTramoRulo :: Float -> Tramo
atravesarTramoRulo diametro (marca, modelo, (ruedas,chasis), velMax, tiempo, apodos)=
    (marca, modelo, (ruedas + diametro * 1.5, chasis), velMax,tiempo + 5 * (diametro / velMax), apodos)

ruloClasico :: Tramo
ruloClasico =  atravesarTramoRulo 13
deseoDeMuerte :: Tramo
deseoDeMuerte =  atravesarTramoRulo 26


-------------------------------
-------- Ejercicio 5 ----------
-------------------------------

--a)


tramo1 :: [Tramo]
tramo1 = [curvaPeligrosa, tramoRectoClassic, zigZagLoco, ruloClasico]

pisteandoLikeAChamp :: [Tramo] -> Auto -> Auto
pisteandoLikeAChamp [] auto = auto -- Si no hay más tramos, el auto queda igual.
pisteandoLikeAChamp (cabeza: restoCola) auto
  | estaEnBuenEstado auto && tiempoTotal auto < 100 = pisteandoLikeAChamp restoCola (cabeza auto)
  | otherwise = auto

-- Función auxiliar para extraer el tiempo del Auto
tiempoTotal :: Auto -> Float
tiempoTotal (_, _, _, _, tiempo, _) = tiempo

{-
--((even (round tiempo) && chasis < 20) || (odd (round tiempo) && chasis < 40 && ruedas < 60))
pisteandoLikeAChamp tramo1 ("Ferrari", "F50", (30, 15), 65, 0, ["Ferrucho"])
Resultado : > ("Ferrari","F50",(15.0,22.15),65.0,20.23077,["Ferrucho"])

--aplico atravesarCurva curvaPeligrosa a ferrari
-- Resultado = (ruedas, chasis) = (15, 15) velMax =65  tiempo = 9.230769231

--aplico atravesarTramoRecto tramoRectoClassic a ferrari
-- Resultado = (ruedas, chasis) = (15, 22.15) velMax = 65  tiempo = 20.23076923 Se detiene aca porque el chasis es 22.15 y el tiempo es par por el redondeo del round.

--aplico atravesarZigZag zigZagLoco a ferrari
-- Resultado = (ruedas, chasis) = (47.5, 27.15) velMax =65  tiempo = 35.23076923

-}



--b)

autosEquipo :: [Auto]
autosEquipo = [ferrari, lamborghini, fiat, peugeot] --Solo cumple peugeot

nivelDeJoyez :: [Auto] -> Int
nivelDeJoyez = sum . map joyez -- Sumar el resultado de aplicar joyez a cada auto

joyez :: Auto -> Int
joyez auto
  | esJoya auto && tiempoTotal auto < 50  = 1
  | esJoya auto && tiempoTotal auto >= 50 = 2
  | otherwise = 0

--c)

recorrerTramos :: [Tramo] -> Auto -> Auto
recorrerTramos tramos auto = foldl (\autoActual tramo -> tramo autoActual) auto tramos

simularCarrera :: Int -> [Tramo] -> Auto -> Auto
simularCarrera numeroVueltas tramos auto
    | numeroVueltas == 0 = auto 
    | (not . estaEnBuenEstado) auto = auto
    | (>= 300) . tiempoTotal $ auto = auto
    | otherwise = simularCarrera (numeroVueltas - 1) tramos (recorrerTramos tramos auto)



--d)
-- testear con esto : paraEntendidos [ferrari, lamborghini, peugeot, fiat] --> Me da false porque fiat y peugeot no estan en buen estado.
paraEntendidos :: [Auto] -> Bool
paraEntendidos autos = all cumple autos

cumple :: Auto -> Bool
cumple auto = estaEnBuenEstado auto &&  ((< 200) . tiempoTotal) auto

-------------------------------
-------- Ejercicio 6 ----------
-------------------------------
 
type Equipo = (String, [Auto], Float) -- (nombre, autos, presupuesto)


-------------Autos de prueba modificados para que den mas ------------------

ferrari' :: Auto
ferrari' = ("Ferrari", "F50", (20, 90), 20, 2, ["La nave", "El fierro", "Ferrucho"])

lamborghini' :: Auto --usado para el ejer 5
lamborghini' = ("Lamborghini", "Diablo", (4, 7), 73, 2, ["Lambo", "La bestia"])

fiat' :: Auto
fiat'= ("Fiat", "600", (27, 33), 15, 0 , ["La Bocha", "La bolita", "Fitito"])

peugeot' :: Auto
peugeot' = ("Peugeot", "504", (20, 10), 25, 0, ["El rey del desierto"])

------------------------------------------------------------------------------

equipo1 :: Equipo
equipo1 = ("Los de siempre", [ferrari', peugeot', fiat'], 50000) --cambiar para el 5a a 150k
{-

ferrari' = 20 * 1000 = 20000
peugeot' = 25 * 1000 = 25000
fiat' = 15 * 1000 = 15000
total = 20000 + 25000 + 15000 = 60000

le agrego lamborghini' = 73 * 1000 = 73000

total = 60000 + 73000 = 133000 -> Queda dentro del presupuesto y lo agrego al equipo.

-}

--a)

costoInscripcion :: Auto -> Float
costoInscripcion (_, _, _, velMax, _, _) = velMax * 1000

costoTotalAutos :: [Auto] -> Float
costoTotalAutos = sum . map costoInscripcion

agregarAutos :: [Auto] -> Equipo -> Equipo
agregarAutos [] equipo = equipo
agregarAutos (auto:resto) (nombre, autos, presupuestoTotal)
  | costoInscripcion auto <= (presupuestoTotal - costoTotalAutos autos) =
      agregarAutos resto (nombre, auto : autos, presupuestoTotal)
  | otherwise =
      agregarAutos resto (nombre, autos, presupuestoTotal)

    
--b)

{-
ferrari' : 38250
peugeot' : 4250
fiat' : 14025

total = 38250 + 4250 + 14025 = 56525

-}


equipo2 :: Equipo
equipo2 = ("Los de siempre", [ferrari', peugeot', fiat'], 50000)

repararEquipo :: Equipo -> Equipo
repararEquipo (nombre, autos, presupuesto) = repararAutos nombre autos presupuesto []

repararAutos :: String -> [Auto] -> Float -> [Auto] -> Equipo
repararAutos nombre [] presupuesto acumulados = (nombre, reverse acumulados, presupuesto)
repararAutos nombre (auto:resto) presupuesto acumulados
  | costoReparacion auto <= presupuesto =
      repararAutos nombre resto (presupuesto - costoReparacion auto) (repararAuto auto : acumulados)
  | otherwise =
      repararAutos nombre resto presupuesto (auto : acumulados)

costoReparacion :: Auto -> Float
costoReparacion (_, _, (_, chasis), _, _, _) = 500 * (chasis * 0.85) 



--c)

equipo3 :: Equipo
equipo3 = ("Los de siempre", [lamborghini', peugeot', fiat'], 150000)

optimizarAutos :: Equipo -> Equipo
optimizarAutos (nombre, autos, presupuesto) = (nombre, optimizarAutosAux autos, presupuesto)

optimizarAutosAux :: [Auto] -> [Auto]
optimizarAutosAux [] = [] 
optimizarAutosAux (auto:resto)
    | estaEnBuenEstado auto = ponerNitro auto : optimizarAutosAux resto 
    | otherwise = auto : resto 

-- d)

{-
venderAuto ferrari' = 40000
venderAuto fiat' = 30000
venderAuto peugeot' = 50000

total = 40000 + 50000 + 30000 = 120000
-}

venderAutoEnMalEstado :: Equipo -> Equipo
venderAutoEnMalEstado (nombre, autos, presupuesto) =
  (nombre, autosRestantes, presupuesto + gananciaTotal)
  where
    (autosAVender, autosRestantes) = partirHastaNoDarMas autos
    gananciaTotal = sum (map ganancia autosAVender)


partirHastaNoDarMas :: [Auto] -> ([Auto], [Auto]) 
partirHastaNoDarMas [] = ([], [])
partirHastaNoDarMas (auto:resto)
  | noDaMas auto = (auto : vendidos, restantes) 
  | otherwise = ([], auto:resto) 
  where
    (vendidos, restantes) = partirHastaNoDarMas resto

ganancia :: Auto -> Float
ganancia (_, _, _, velMax, _, _) = velMax * 2000