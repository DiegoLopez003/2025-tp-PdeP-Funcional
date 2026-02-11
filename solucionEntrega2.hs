import Text.Show.Functions()
import Data.List()

-------------------------------
-------- Modelado -------------
-------------------------------

data Auto = Auto{
    marca :: String,
    modelo :: String,
    desgasteRuedas :: Float,
    desgasteChasis :: Float,
    velMax :: Float,
    tiempoCarrera :: Float,
    apodos :: [String]
}deriving(Show, Eq)

-------------------------------
-------- Ejercicio 1 ----------
-------------------------------

ferrari :: Auto
ferrari = Auto{
    marca = "Ferrari",
    modelo = "F50",
    desgasteRuedas = 79,
    desgasteChasis = 0,
    velMax = 65,
    tiempoCarrera = 0,
    apodos = ["La nave", "El fierro", "Ferrucho"]
}

lamborghini :: Auto
lamborghini = Auto{
    marca = "Lamborghini",
    modelo = "Diablo",
    desgasteRuedas = 4,
    desgasteChasis = 7,
    velMax = 73,
    tiempoCarrera = 0,
    apodos = ["Lambo", "La bestia"]
}

fiat :: Auto
fiat = Auto{
    marca = "Fiat",
    modelo = "600",
    desgasteRuedas = 27,
    desgasteChasis = 33,
    velMax = 44,
    tiempoCarrera = 0,
    apodos = ["La Bocha", "La bolita", "Fitito"]
}

peugeot :: Auto
peugeot = Auto{
    marca = "Peugeot",
    modelo = "504",
    desgasteRuedas = 79,
    desgasteChasis = 0,
    velMax = 40,
    tiempoCarrera = 0,
    apodos = ["El rey del desierto"]
}


-------------------------------
-------- Ejercicio 2 ----------
-------------------------------

--a)
esPar :: Float -> Bool
esPar = even . round

estaEnBuenEstado :: Auto -> Bool
estaEnBuenEstado auto = 
    marca auto /= "Peugeot" && 
    ((esPar (tiempoCarrera auto) && (desgasteChasis auto) < 20) ||((not . esPar $ tiempoCarrera auto) && desgasteChasis auto < 40 && desgasteRuedas auto < 60))

--b)
primerApodo :: [String] -> Bool
primerApodo apodos = take 3 (head apodos) == "La "

mayorA80 :: Float -> Bool
mayorA80 = (> 80)

noDaMas :: Auto -> Bool
noDaMas auto = 
    (primerApodo (apodos auto) && mayorA80 (desgasteChasis auto)) || 
    ((not . primerApodo $ (apodos auto)) && mayorA80 (desgasteRuedas auto))

--c)
esParApodo :: [String] -> Bool
esParApodo = even . length

esChiche :: Auto -> Bool 
esChiche auto = 
    (esParApodo (apodos auto) && (desgasteChasis auto < 20)) || ((not . esParApodo) (apodos auto) && ((desgasteChasis auto) < 50))

--d)
niMasDeUnApodo :: [String] -> Bool
niMasDeUnApodo = (<= 1) . length

esJoya :: Auto -> Bool
esJoya auto = niMasDeUnApodo (apodos auto)--ejem: el tamaño de apodo de ferrari es 3 y peugeot es 1
--esJoya _ = False

--e)
nivelChetez :: Auto -> Int
nivelChetez auto = 20 * length (apodos auto) * length (modelo auto)

--f)
tamanioCabezaListaApodos :: [String] -> Int
tamanioCabezaListaApodos = length . head

capacidadSuper :: Auto -> Int
capacidadSuper auto = tamanioCabezaListaApodos (apodos auto)

--g)
riesgoBase :: Auto -> Float
riesgoBase auto = velMax auto * (desgasteRuedas auto) / 10

riesgo :: Auto -> Float
riesgo auto 
    | estaEnBuenEstado auto = riesgoBase auto
    | otherwise =  2 * riesgoBase auto



-------------------------------
-------- Ejercicio 3 ----------
-------------------------------

--a)

repararAuto :: Auto -> Auto
repararAuto auto = auto{
        desgasteRuedas = 0,
        desgasteChasis = (desgasteChasis auto) * 0.15
    }

--b)

aplicarPenalidad :: Float -> Auto -> Auto
aplicarPenalidad cantidadSegundos auto = auto{
        tiempoCarrera = (tiempoCarrera auto) + cantidadSegundos
    }

--c)

ponerNitro :: Auto -> Auto
ponerNitro auto = auto{
        velMax = (velMax auto) * 1.2
    }

--d)

bautizarAuto :: String -> Auto -> Auto
bautizarAuto nuevoApodo auto = auto{
        apodos = (apodos auto) ++ [nuevoApodo]
    }

--e)
autoAlDesarmadero :: String -> String -> Auto -> Auto
autoAlDesarmadero marcaNueva modeloNuevo auto = auto{
        marca = marcaNueva,
        modelo = modeloNuevo,
        apodos = ["Nunca taxi"]
    }

-------------------------------
-------- Ejercicio 4 ----------
-------------------------------


--type Pista = (String, String, Float, [Tramo])

data Pista = Pista{
    nombrePista :: String,
    pais :: String,
    precio :: Float,
    tramos :: [Tramo]
} deriving(Show)

type Tramo = Auto -> Auto 


-- pista :: Pista
-- pista  = ("Autodromo", "Argentina", 1000, [curvaPeligrosa, tramoRectoClassic, zigZagLoco, ruloClasico])

pista :: Pista
pista = Pista{
    nombrePista = "Autodromo",
    pais = "Argentina",
    precio = 1000,
    tramos = [curvaPeligrosa, tramoRectoClassic, zigZagLoco, ruloClasico]
}

type Curva = (Float, Float) -- (angulo, longitud)

--a)
--atravesarCurva :: (Float, Float) -> Auto -> Auto
atravesarCurva :: Curva -> Tramo
atravesarCurva (angulo, longitud) auto =auto{
        desgasteRuedas = desgasteRuedas auto + (3 * longitud) / angulo,
        tiempoCarrera = tiempoCarrera auto + (longitud / (velMax auto / 2))
    }

curvaPeligrosa :: Tramo
curvaPeligrosa = atravesarCurva (60, 300)

curvaTranca :: Tramo
curvaTranca = atravesarCurva (110, 550)

--b)

atravesarTramoRecto :: Float -> Tramo
atravesarTramoRecto longitud auto = auto{
        desgasteChasis = desgasteChasis auto + (longitud / 100),
        tiempoCarrera = tiempoCarrera auto + (longitud / velMax auto)
    }

tramoRectoClassic :: Tramo
tramoRectoClassic = atravesarTramoRecto 715

tramito :: Tramo
tramito = atravesarTramoRecto 260

--c)

atravesarZigZag :: Float -> Tramo
atravesarZigZag cantCambiosDireccion auto = auto{
        desgasteRuedas = (desgasteRuedas auto) + (velMax auto * (cantCambiosDireccion / 10)),
        desgasteChasis = (desgasteChasis auto) + 5,
        tiempoCarrera = (tiempoCarrera auto) + 3 * cantCambiosDireccion
    }

-- Ahora, en vez de tener zigZagLoco como Float, que sea directamente Tramo:
zigZagLoco :: Tramo
zigZagLoco = atravesarZigZag 5

casiCurva :: Tramo
casiCurva = atravesarZigZag 1


--d)

atravesarTramoRulo :: Float -> Tramo
atravesarTramoRulo diametro auto = auto{
        desgasteRuedas = (desgasteRuedas auto) + diametro * 1.5,
        tiempoCarrera = (tiempoCarrera auto) + 5 * (diametro / velMax auto)
    }

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
tiempoTotal auto = tiempoCarrera auto

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
recorrerTramos tramos auto = foldl aplicar auto tramos
  where
    aplicar :: Auto -> Tramo -> Auto
    aplicar autoActual tramo = tramo autoActual

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
 
--type Equipo = (String, [Auto], Float) -- (nombre, autos, presupuesto)

data Equipo = Equipo{
    nombre :: String,
    autos :: [Auto],
    presupuesto :: Float
}deriving(Show)

-------------Autos de prueba modificados para que den mas ------------------


ferrari' :: Auto
ferrari' = Auto{
    marca = "Ferrari",
    modelo = "F50",
    desgasteRuedas = 20,
    desgasteChasis = 10,
    velMax = 20,
    tiempoCarrera = 2,
    apodos = ["La nave", "El fierro", "Ferrucho"]
}

lamborghini' :: Auto --usado para el ejer 5
lamborghini' = Auto{
    marca = "Lamborghini",
    modelo = "Diablo",
    desgasteRuedas = 4,
    desgasteChasis = 20,
    velMax = 73,
    tiempoCarrera = 2,
    apodos = ["Lambo", "La bestia"]
}

fiat' :: Auto
fiat' = Auto{
    marca = "Fiat",
    modelo = "600",
    desgasteRuedas = 27,
    desgasteChasis = 50,
    velMax = 15,
    tiempoCarrera = 0,
    apodos = ["La Bocha", "La bolita", "Fitito"]
}

peugeot' :: Auto
peugeot' = Auto{
    marca = "Peugeot",
    modelo = "504",
    desgasteRuedas = 20,
    desgasteChasis = 10,
    velMax = 25,
    tiempoCarrera = 0,
    apodos = ["El rey del desierto"]
}


------------------------------------------------------------------------------

equipo1 :: Equipo
equipo1 = Equipo{
    nombre = "Los de siempre",
    autos = [ferrari', peugeot', fiat'],
    presupuesto = 20000
}

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
costoInscripcion auto = velMax auto * 1000

costoTotalAutos :: [Auto] -> Float
costoTotalAutos = sum . map costoInscripcion

agregarAutos :: [Auto] -> Equipo -> Equipo
agregarAutos [] equipo = equipo
agregarAutos (auto:resto) equipo --(nombre, autos, presupuestoTotal)
  | costoInscripcion auto <= (presupuesto equipo - costoTotalAutos (autos equipo)) =
      agregarAutos resto equipo{autos = auto : (autos equipo)}
  | otherwise =
      agregarAutos resto equipo



-- d)

{-
venderAuto ferrari' = 40000
venderAuto fiat' = 30000
venderAuto peugeot' = 50000

total = 40000 + 50000 + 30000 = 120000
-}

venderAutoEnMalEstado :: Equipo -> Equipo
venderAutoEnMalEstado equipo =
    equipo{
        nombre = nombre equipo,
        autos = autosRestantes,
        presupuesto = presupuesto equipo + gananciaTotal
    }
  where
    (autosAVender, autosRestantes) = partirHastaNoDarMas (autos equipo)
    gananciaTotal = sum (map ganancia autosAVender)


partirHastaNoDarMas :: [Auto] -> ([Auto], [Auto]) 
partirHastaNoDarMas [] = ([], [])
partirHastaNoDarMas (auto:resto)
  | noDaMas auto = (auto : vendidos, restantes) 
  | otherwise = ([], auto:resto) 
  where
    (vendidos, restantes) = partirHastaNoDarMas resto

ganancia :: Auto -> Float
ganancia auto = velMax auto * 2000


----------------------------------------
----- PARTE 2 DEL TRABAJO PRACTICO -----
----------------------------------------
--1a)
equipoRep:: Equipo
equipoRep = Equipo{
    nombre = "Los optimizadores",
    autos = [ferrari', lamborghini'],
    presupuesto = 20000
}

equipoSoloFiat :: Equipo
equipoSoloFiat = Equipo{
    nombre = "Los Fiat",
    autos = [fiat'],
    presupuesto = 10000
}

aplicarSiEs :: Auto -> (Auto -> Auto) -> Auto -> Auto
aplicarSiEs auto transformacion otroAuto
  | otroAuto == auto = transformacion otroAuto
  | otherwise = otroAuto

repararEquipo :: Equipo -> Equipo
repararEquipo equipoOriginal = foldl repararSiHayPresupuesto equipoOriginal (autos equipoOriginal)
  where
    repararSiHayPresupuesto :: Equipo -> Auto -> Equipo
    repararSiHayPresupuesto equipo auto
      | costoReparacion auto <= presupuesto equipo = equipo {
            autos = map (aplicarSiEs auto repararAuto) (autos equipo),
            presupuesto = presupuesto equipo - costoReparacion auto
          }
      | otherwise = equipo

--1b)

equipo3 :: Equipo
equipo3 = Equipo{
    nombre = "Los de siempre",
    autos = [ferrari, lamborghini],
    presupuesto = 20000 --cambiar a 10000 para el 1er caso
}

optimizarEquipo :: Equipo -> Equipo
optimizarEquipo equipoOriginal = foldl optimizarSiHayPresupuesto equipoOriginal (autos equipoOriginal)
  where
    optimizarSiHayPresupuesto :: Equipo -> Auto -> Equipo
    optimizarSiHayPresupuesto equipo auto
      | costoPonerNitro auto <= presupuesto equipo = equipo {
            autos = map (aplicarSiEs auto ponerNitro) (autos equipo),
            presupuesto = presupuesto equipo - costoPonerNitro auto
          }
      | otherwise = equipo 

--1c)

equipo4 :: Equipo
equipo4 = Equipo{
    nombre = "Los mas o menos",
    autos = [peugeot, lamborghini],
    presupuesto = 20000 
}

equipo5 :: Equipo
equipo5 = Equipo{
    nombre = "Los de siempre 2",
    autos = [peugeot, lamborghini],
    presupuesto = 4000
}

equipo6 :: Equipo
equipo6 = Equipo{
    nombre = "Los mas caros",
    autos = [peugeot, ferrari, lamborghini],
    presupuesto = 20000
}

ferrarizar :: Equipo -> Equipo
ferrarizar equipoOriginal = foldl ferrarizarSiEsNecesario equipoOriginal (autos equipoOriginal)
  where
    ferrarizarSiEsNecesario :: Equipo -> Auto -> Equipo
    ferrarizarSiEsNecesario equipo auto
      | marca auto == "Ferrari" = equipo 
      | costoFerrari <= presupuesto equipo = equipo {
            autos = map (aplicarSiEs auto (autoAlDesarmadero "Ferrari" "F50")) (autos equipo),
            presupuesto = presupuesto equipo - costoFerrari
          }
      | otherwise = equipo
      
costoFerrari = 3500

costoPonerNitro :: Auto -> Float
costoPonerNitro auto = velMax auto * 100

costoReparacion :: Auto -> Float
costoReparacion auto = 500 * desgasteChasis auto * 0.85



-----------------------
----- Ejercicio 2 -----
-----------------------

equipoInfinia :: Equipo
equipoInfinia = Equipo {
    nombre = "Infinia",
    autos = autosInfinitos,
    presupuesto = 50000
}

autosInfinitos :: [Auto]
autosInfinitos = map autoFerrariConVelocidad [1..] -- Genera una lista infinita de autos Ferrari con velocidad creciente

autoFerrariConVelocidad :: Int -> Auto
autoFerrariConVelocidad multiplicador = Auto {
    marca = "Ferrari",
    modelo = "F50",
    desgasteRuedas = 0,
    desgasteChasis = 1,
    velMax = 65 * fromIntegral multiplicador,
    tiempoCarrera = 0,
    apodos = ["Infinito"]
}

--Poner repararEquipo con equipoInfinia me devuelve un error de interrupcion ya que nunca va a poder terminar de evaluar la lista de auto por el motivo

--de que siempre entra al othrewise, salteandolo en caso de que el costo de reparacion sea mayor al presupuesto.

--Optiimizar Autos los primeros autos los resuelve, pero una vez que se pasan del presupuesto entra en un loop infinito, ya que siempre va autoDeLaLista caer en el otherwise.

--ferrarizar equipoInfinia me devuelve un equipo con una lista de autos infinitos sin ferrarizar ya que todo son ferraris. El presupuesto no se ve afectado.

--Al aplicar nivelDeJoyes a autosInfinitos, este nos da un resultado que no converge, ya que la funcion va a tratar de transformar toda la lista completado siendo que esta es infinita nunca
--va a poder terminar de evaluarla. Si en el caso de que en vez de transformarla quisieramos saber si alguno cumple cierta condicion, ahi si nos devolveria un resultado concreto.

---------------------------------
---------- Ejercicio 3 ----------
---------------------------------

--a)
boxes :: Tramo -> Tramo --boxes es donde donde se reparar los autos, son paradas: ir a los pits
boxes tramo auto
  | estaEnBuenEstado auto = tramo auto
  | otherwise             = (aplicarPenalidad 10 . repararAuto . tramo) auto --aplicarPenalidad 10 (repararAuto (tramo auto))

--b)
mojado :: Tramo -> Tramo
mojado tramo auto =
  aplicarPenalidad (0.5 * incrementoTiempo) autoModificado --el (0.5 * incrementoTiempo) osea aumentamos el 50% que el tramo aumento al auto
  where --devuelta sumamos el 50% que que el tramo produce al auto
    autoModificado = tramo auto
    incrementoTiempo = tiempoCarrera autoModificado - tiempoCarrera auto --obviamente el tiempo del autoModificado > auto

--c)
ripio :: Tramo -> Tramo
ripio tramo = tramo . tramo --aplicamos el efecto del tramo 2 veces y implicitamente tambien lo hace el tiempo

--d)
aumentarDesgasteRuedas :: Float -> Tramo
aumentarDesgasteRuedas cantidad auto = auto {
    desgasteRuedas = desgasteRuedas auto + cantidad
}

obstruccion :: Float -> Tramo -> Tramo
obstruccion metros tramo = aumentarDesgasteRuedas (2 * metros) . tramo --lo que hacemos es agregar efectos adicionales al tramo, devolviendome el tramo modificado

--e)
turbo :: Tramo -> Tramo
turbo tramo auto =
  restaurarVelocidad (velMax auto) (tramo (auto { velMax = 2 * velMax auto }))

restaurarVelocidad :: Float -> Tramo
restaurarVelocidad velocidadOriginal auto = auto { velMax = velocidadOriginal }

--4)
pasarPorTramo :: Tramo -> Auto -> Auto  --ataraviesa el tramo si, no (noDaMas)
pasarPorTramo tramo auto
  | noDaMas auto = auto
  | otherwise    = tramo auto

--5)
--a)
vueltaALaManzana :: Pista
vueltaALaManzana = Pista{
    nombrePista = "La manzana",
    pais = "Italia",
    ------------3.25t------
    precio = 30,------------2.4--4.4----------4.8-----6.8--------7.2-----9.2 ------ 9.6
    tramos = [tramoRecto, curva, tramoRecto, curva, tramoRecto, curva, tramoRecto, curva]
}
  where
    tramoRecto = atravesarTramoRecto 130
    curva = atravesarCurva (90, 13)


--b)

superPista :: Pista
superPista = Pista{
    nombrePista = "Argentina",
    pais = "superPista",
    precio = 300,
    tramos = [
        tramoRectoClassic,               -- i
        curvaTranca,                    -- ii
        turbo tramito,                  -- iii primer tramo
        mojado tramito,                 -- iii segundo tramo
        atravesarTramoRulo 10,          -- iv rulo de 10m
        obstruccion 2 (atravesarCurva (80, 400)), -- v curva con obstrucción 2m
        atravesarCurva (115, 650),      -- vi curva 115° 650m
        atravesarTramoRecto 970,        -- vii tramo recto 970m
        curvaPeligrosa,                 -- viii curva peligrosa
        ripio tramito,                  -- ix tramito con ripio
        boxes (atravesarTramoRecto 800),-- x boxes con tramo recto 800m
        obstruccion 5 casiCurva,        -- xi casiCurva con obstrucción 5m
        atravesarZigZag 2,              -- xii zigzag 2 cambios
        ripio (mojado (deseoDeMuerte)), -- xiii deseoDeMuerte mojado y ripio
        ruloClasico,                   -- xiv ruloClasico
        zigZagLoco                    -- xv zigZagLoco
    ]
}
--5c)


-- Paso por un tramo solo si el auto está en buen estado
pasarSiPuede :: Auto -> Tramo -> Auto
pasarSiPuede auto tramo
  | not . noDaMas $ auto = tramo auto
  | otherwise = auto

-- Hace que un auto avance por los tramos mientras pueda, usando foldl
avanzarHastaNoDarMas :: [Tramo] -> Auto -> Auto
avanzarHastaNoDarMas tramos auto = foldl pasarSiPuede auto tramos

-- Aplica a todos los autos en la pista
peganLaVuelta :: Pista -> [Auto] -> [Auto]
peganLaVuelta pista autos = map (avanzarHastaNoDarMas (tramos pista)) autos


---------------------------
------- Ejercicio 6 -------
---------------------------
--6a)
data Carrera = Carrera{
    pistaCarrera :: Pista,
    vueltas :: Int
} deriving(Show)

--6b)
tourBuenosAires :: Carrera
tourBuenosAires = Carrera{
    pistaCarrera = superPista,
    vueltas = 20
}

--6c)
--se puede usar peganLaVuelta
ganadorCarrera :: Carrera -> [Auto] -> Auto
ganadorCarrera carrera = menorTiempo . filter (not . noDaMas) . map (simularCarrera (vueltas carrera) (tramos (pistaCarrera carrera)))


menorTiempo :: [Auto] -> Auto
menorTiempo autos = foldl1 comparacionTiempo autos

comparacionTiempo :: Auto -> Auto -> Auto
comparacionTiempo auto1 auto2
  | tiempoCarrera auto1 <= tiempoCarrera auto2 = auto1
  | otherwise = auto2

superListaTramos :: [Tramo]
superListaTramos = [
    tramoRectoClassic,               -- i
    curvaTranca,                    -- ii
    turbo tramito,                  -- iii primer tramo
    mojado tramito,                 -- iii segundo tramo
    atravesarTramoRulo 10,          -- iv rulo de 10m
    obstruccion 2 (atravesarCurva (80, 400)), -- v curva con obstrucción 2m
    atravesarCurva (115, 650),      -- vi curva 115° 650m
    atravesarTramoRecto 970,        -- vii tramo recto 970m
    curvaPeligrosa,                 -- viii curva peligrosa
    ripio tramito,                  -- ix tramito con ripio
    boxes (atravesarTramoRecto 800),-- x boxes con tramo recto 800m
    obstruccion 5 casiCurva,        -- xi casiCurva con obstrucción 5m
    atravesarZigZag 2,              -- xii zigzag 2 cambios
    ripio (mojado (deseoDeMuerte)), -- xiii deseoDeMuerte mojado y ripio
    ruloClasico,                   -- xiv ruloClasico
    zigZagLoco]                    -- xv zigZagLoco]

lamboMod :: Auto
lamboMod = Auto {
    marca = "Lamborghini", 
    modelo = "Diablo", 
    desgasteRuedas = 165.9, 
    desgasteChasis = 21.3375, 
    velMax = 73.0, 
    tiempoCarrera = 141.26028, 
    apodos = ["Lambo","La bestia"]
}

ferrariMod :: Auto
ferrariMod = Auto {
    marca = "Ferrari", 
    modelo = "F50", 
    desgasteRuedas = 159.5, 
    desgasteChasis = 20.2875, 
    velMax = 65.0, 
    tiempoCarrera = 154.46155, 
    apodos = ["La nave","El fierro","Ferrucho"]
}

fiatMod :: Auto
fiatMod = Auto {
    marca = "Fiat", 
    modelo = "600", 
    desgasteRuedas = 27.0, 
    desgasteChasis = 33.0, 
    velMax = 44.0, 
    tiempoCarrera = 0.0, 
    apodos = ["La Bocha","La bolita","Fitito"]
}

autosLocosL :: [Auto]
autosLocosL = [ferrariMod, lamboMod, fiatMod]

probarFiltroAutos :: [Auto] -> [Auto]
probarFiltroAutos autos = filter (not . noDaMas) autos