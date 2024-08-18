program Main

   use readFiles

   implicit none
   ! Declaracion de variables

   integer :: option
   character(len=:), allocatable :: contenido

   ! Mostrar Menu
   do while (option /= 4)
      print *, "---------------------------------------------------"
      print *, "Practica 1 - Lenguajes Formales y de programacion"
      print *, "---------------------------------------------------"
      print *, ""
      print *, "MENU"
      print *, "1. Cargar Inventario Inicial"
      print *, "2. Cargar Instrucciones de Movimientos"
      print *, "3. Crear Informe de Inventario"
      print *, "4. Salir"
      print *, ""
      print *, "Ingrese una opcion: "
      read *, option

      select case (option)
      case (1)
         contenido = loadInventory()
         print *, contenido
      case (2)
         !call resta()
      case (3)
         !call multiplicacion()
      case (4)
         print *, "Saliendo del programa"
      case default
         print *, "Opcion no valida"
      end select
   end do

end program Main

