program Main

   !importar modulos
   use read_inventory
   use equipment_type
   use read_mov
   use generateInfo

   implicit none

   ! Declaracion de variables
   integer :: option
   type(Equipment), allocatable :: EquipmentList(:)
   type(Equipment), allocatable :: New_EquipmentList(:)

   !inicializar variables
   allocate (EquipmentList(0))

   ! Mostrar Menu
   do while (option /= 4)

      print *, "---------------------------------------------------"
      print *, "Practica 1 - Lenguajes Formales y de programacion"
      print *, "    Diego Alejandro Vásquez Alonzo 202300638 "
      print *, "---------------------------------------------------"
      print *, ""
      print *, "MENU"
      print *, " [1]. Cargar Inventario Inicial"
      print *, " [2]. Cargar Instrucciones de Movimientos"
      print *, " [3]. Crear Informe de Inventario"
      print *, " [0]. Salir"
      print *, ""
      write (*, '(A)', advance='no') " Ingrese una opcion: "
      read *, option
      print *, ""

      select case (option)
      case (1)

         ! Cargar Inventario Inicial
         print *, "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
         print *, "                Ingresar Inventario"
         print *, "---------------------------------------------------"
         print *, ""

         New_EquipmentList = loadInventory()
         EquipmentList = margeEquipmentsLists(EquipmentList, New_EquipmentList)

      case (2)
         ! Cargar Instrucciones de Movimientos
         if (size(EquipmentList) == 0) then
            call initailContidition()
            cycle
         end if
         EquipmentList = readMov(EquipmentList)

      case (3)

         if (size(EquipmentList) == 0) then
            call initailContidition()
            cycle
         end if

         ! Crear Informe de Inventario
         print *, "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
         print *, "         Generando Informe de Inventario"
         print *, "---------------------------------------------------"
         print *, ""
         call generar_informe_inventario(EquipmentList, size(EquipmentList))
         print *, "///////////////////////////////////////////////////"
         print *, "     Informe de Inventario generado con exito"
         print *, "///////////////////////////////////////////////////"
         print *, ""
      case (0)

         print *, "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
         print *, "               Saliendo del programa"
         print *, "---------------------------------------------------"
         print *, ""
         stop

      case default
         print *, "Opcion no valida"
      end select
   end do

contains

   subroutine initailContidition()

      print *, "//////////////////////////////////////////////"
      print *, " !Primero debe cargar el inventario inicial¡"
      print *, "//////////////////////////////////////////////"
      print *, ""
      print *, ""

   end subroutine initailContidition

   subroutine printEquipment(e)
      type(Equipment), intent(in) :: e
      call e%mostrarProducto()
   end subroutine printEquipment

   subroutine printEquipmentList(list)
      type(Equipment), allocatable, intent(in) :: list(:)
      integer :: i
      do i = 1, size(list)
         call printEquipment(list(i))
      end do
   end subroutine printEquipmentList

end program Main

