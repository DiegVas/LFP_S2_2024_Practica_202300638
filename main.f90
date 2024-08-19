program Main

   use read_inventory
   use equipment_type
   use read_mov

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

         if (size(EquipmentList) == 0) then
            EquipmentList = loadInventory()
         else
            New_EquipmentList = loadInventory()
            EquipmentList = margeEquipmentsLists(EquipmentList, New_EquipmentList)
         end if

      case (2)
         EquipmentList = readMov(EquipmentList)
      case (3)
         EquipmentList = EquipmentList
         call printEquipmentList(EquipmentList)
         print *, "Informe de Inventario"
         print *, size(EquipmentList)
      case (4)
         print *, "Saliendo del programa"
      case default
         print *, "Opcion no valida"
      end select
   end do

contains

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

