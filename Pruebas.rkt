#lang racket

(require "TDA-System_212515965_Sanchez.rkt")

; Scripts de prueba

; Scripts del enunciado
;creando un sistema
(define S0 (system "newSystem"))
;añadiendo unidades. Incluye caso S2 que intenta añadir unidad con una letra que ya existe
(define S1 ((run S0 add-drive) #\C "SO" 1000))
(define S2 ((run S1 add-drive) #\C "SO1" 3000))
(define S3 ((run S2 add-drive) #\D "Util" 2000))
;añadiendo usuarios. Incluye caso S6 que intenta registrar usuario duplicado
(define S4 ((run S3 register) "user1"))
(define S5 ((run S4 register) "user1"))
(define S6 ((run S5 register) "user2"))
;iniciando sesión con usuarios. Incluye caso S8 que intenta iniciar sesión con user2 sin antes haber salido con user1
(define S7 ((run S6 login) "user1"))
(define S8 ((run S7 login) "user2"))
;cerrando sesión user1 e iniciando con user2
(define S9 (run S8 logout))
(define S10 ((run S9 login) "user2"))
;cambios de unidad, incluyendo unidad inexistente K
(define S11 ((run S10 switch-drive) #\K))
(define S12 ((run S11 switch-drive) #\C))
;añadiendo carpetas. Incluye casos de carpetas duplicadas.
(define S13 ((run S12 md) "folder1"))
(define S14 ((run S13 md) "folder2"))
(define S15 ((run S14 md) "folder2"))
(define S16 ((run S15 md) "folder3"))
;ingresa a carpeta folder2
(define S17 ((run S16 cd) "folder2"))
;crea subcarpeta folder21 dentro de folder2 (incluye caso S18 de carpeta con nombre duplicado)
(define S18 ((run S17 md) "folder21"))
(define S19 ((run S18 md) "folder21"))
;ingresa a subcarpeta e intenta ingresar a subcarpeta inexistente S21
(define S20 ((run S19 cd) "folder21"))
(define S21 ((run S20 cd) "folder22"))
;vuelve a carpeta anterior
(define S22 ((run S21 cd) ".."))
;vuelve a ingresar folder21
(define S23 ((run S22 cd) "folder21"))
;crea subcarpeta folder211 e ingresa
(define S24 ((run S23 md) "folder211"))
(define S25 ((run S24 cd) "folder211"))
;vuelve a la raíz de la unidad c:/
(define S26 ((run S25 cd) "/"))
;se cambia de unidad
(define S27 ((run S26 switch-drive) #\D))
;crea carpeta e ingresa a carpeta
(define S28 ((run S27 md) "folder5"))
(define S29 ((run S28 cd) "folder5"))
;se cambia de carpeta en base a la ruta especificada
(define S30 ((run S29 cd) "C:/folder1/"))
;formateando drive D:
(define S31 ((run S30 format) #\D "newD"))
;añadiendo archivos
(define S32 ((run S30 add-file) (file "foo1.txt" "txt" "hello world 1")))
(define S33 ((run S32 add-file) (file "foo2.txt" "txt" "hello world 2")))
(define S34 ((run S33 add-file) (file "foo3.docx" "docx" "hello world 3")))
(define S35 ((run S34 add-file) (file "goo4.docx" "docx" "hello world 4" #\h #\r))) ;con atributosde seguridad oculto (h) y de solo lectura (r)
;eliminando archivos
(define S36 ((run S35 del) "*.txt"))
(define S37 ((run S35 del) "f*.docx"))
(define S38 ((run S35 del) "goo4.docx"))
(define S39 ((run S35 cd) ".."))
(define S40 ((run S39 del) "folder1"))
;borrando una carpeta
(define S41 ((run S39 rd) "folder1")) ;no debería borrarla, pues tiene archivos
(define S42 ((run S41 cd) "folder1"))
(define S43 ((run S42 del) "*.*"))
(define S44 ((run S43 cd) ".."))
(define S45 ((run S44 rd) "folder1"))
;copiando carpetas y archivos
(define S46 ((run S35 copy) "foo1.txt" "c:/folder3/"))
(define S47 ((run S46 cd) ".."))
(define S48 ((run S47 copy) "folder1" "d:/"))
;moviendo carpetas y archivos
(define S49 ((run S48 move) "folder3" "d:/"))
(define S50 ((run S49 cd) "folder1"))
(define S51 ((run S50 move) "foo3.docx" "d:/folder3/"))
;renombrando carpetas y archivos
(define S52 ((run S51 ren) "foo1.txt" "newFoo1.txt"))
(define S53 ((run S52 ren) "foo2.txt" "newFoo1.txt")) ;no debería efectuar cambios pues ya existe archivo con este nombre
(define S54 ((run S53 cd) ".."))
(define S55 ((run S54 ren) "folder1" "newFolder1"))
;listando la información
(display ((run S16 dir)))
(display ((run S55 dir)))

;Scripts realizados por mi

;Creacion de sistemas
;system 1/3
(define W0 (system "Windows"))  
;system 2/3                        
(define L0 (system "Linux"))
;system 3/3                            
(define M0 (system "MacOs"))

;Se trabajara con el sistema creado en los scripts del enunciado.
;add-drive 1/3
(define S56 ((run S55 add-drive) #\c "juegos" 10000))
;add-drive 2/3
(define S57 ((run S56 add-drive) #\d "juegos" 10000))
;add-drive 3/3
(define S58 ((run S57 add-drive) #\e "juegos" 10000))
;switch-drive 1/3
(define S59 ((run S58 switch-drive) #\E))
;md 1/3
(define S60 ((run S59 md) "carpetaA"))
;cd 1/3
(define S61 ((run S60 cd) "CARPETAa"))
;md 2/3
(define S62 ((run S61 md) "carpetaB"))
;md 3/3
(define S63 ((run S62 md) "CaRpEtAc"))
;add-file 1/3
(define S64 ((run S63 add-file) (file "Nota.txt" "txt" "archivo para leer")))
;cd 2/3
(define S65 ((run S64 cd) "CARPETAc"))
;add-file 2/3
(define S66 ((run S65 add-file) (file "Nota2.txt" "txt" "gato naranjo")))
;add-file 3/3
(define S67 ((run S66 add-file) (file "Nota3.docx" "txt" "hace frio")))
;cd 3/3
(define S68 ((run S67 cd) "/"))
;cd 4/3
(define S69 ((run S68 cd) ".."))
;cd 5/3
(define S70 ((run S69 cd) "carpetaa/CARPETAB"))
;add-file 4/3
(define S71 ((run S70 add-file) (file "nota4.txt" "txt" "ahora voy a eliminar los archivos de la carpetac")))
;cd 6/3
(define S72 ((run S71 cd) ".."))
;cd 7/3
(define S73 ((run S72 cd) "carpetac"))
;del 1/3
(define S74 ((run S73 del) "*.*"))
;add-file 5/3
(define S75 ((run S74 add-file) (file "nota5.txt" "txt" "ahora voy a crear otro usuario y desloguearme")))
;register 1/3
(define S76 ((run S75 register) "elpepe"))
;login 1/3
(define S77 ((run S76 login) "elpepe"))
;logout 1/3
(define S78 (run S77 logout))
;login 2/3
(define S79 ((run S78 login) "elpepe"))
;add-file 6/3
(define S80 ((run S79 add-file) (file "nota6.txt" "txt" "ahora mandare este archivo a...")))
;move 1/3
(define S81 ((run S80 move) "nota6.txt" "c:/folder2/folder21/folder211/"))
;cd 8/3
(define S82 ((run S81 cd) "c:/folder2/folder21/folder211/"))
;del 2/3
(define S83 ((run S82 del) "nota6.txt"))
;del 3/3
(define S84 ((run S83 del) "nota9890798798.txt"))
;cd 9/3
(define S85 ((run S84 cd) "/"))
;rd 1/3
(define S86 ((run S85 rd) "folder2/folder21/folder211"))
;rd 2/3
(define S87 ((run S86 rd) "folder2/folder21"))
;rd 3/3
(define S88 ((run S87 rd) "folder2"))
;cd 10/3
(define S89 ((run S88 cd) "folideruno"))
;cd 11/3
(define S90 ((run S89 cd) "newfolder1"))
;copy 1/3
(define S91 ((run S90 copy) "foo1.txt" "e:/carpetaa/carpetac/"))
;copy 2/3
(define S92 ((run S91 copy) "newfoo1.txt" "e:/carpetaa/carpetac/"))
;copy 3/3
(define S93 ((run S92 copy) "newfoo1.txt" "e:/carpetaa/carpetac/"))
;cd 12/3
(define S94 ((run S93 cd) "e:/carpetaa/carpetac/"))
;ren 1/3
(define S95 ((run S94 ren) "newfoo1.txt" "newfoo2.txt"))
;ren 2/3
(define S96 ((run S95 ren) "newfoo11.txt" "newfoo3.txt"))
;ren 3/3
(define S97 ((run S96 ren) "nota5.txt" "nota.txt"))
;move 2/3
(define S98 ((run S97 move) "newfoo2.txt" "d:/folder5/"))
;move 3/3
(define S99 ((run S98 move) "newfoo3.txt" "d:/folder5/"))
;switch-drive 2/3
(define S100 ((run S99 switch-drive) #\d))
;register 2/3
(define S101 ((run S100 register) "AalSaa"))
;register 3/3
(define S102 ((run S101 register) "Alonsans"))
;loguout 2/3
(define S103 (run S102 logout))
;logout 3/3
(define S104 (run S103 logout))
;login 3/3
(define S105 ((run S104 login) "AalSaa"))
;format 1/3
(define S106 ((run S105 format) #\c "NewSO"))
;format 2/3
(define S107 ((run S106 format) #\e "Archivos USACH"))
;cd 13/3
(define S108 ((run S107 cd) "folder1"))
;switch-drive 3/3
(define S109 ((run S108 switch-drive) #\e))
;format 3/3
(define S110 ((run S109 format) #\d "fin"))
;dir 1/3
(display ((run S107 dir)))
;dir 2/3
(display ((run S108 dir)))
;dir 3/3
(display ((run S110 dir)))