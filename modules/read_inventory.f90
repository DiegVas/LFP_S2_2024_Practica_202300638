module read_inventory
   use equipment_type
contains

   function loadInventory() result(EquipmentList)
      implicit none

      type(Equipment), allocatable :: EquipmentList(:)
      type(Equipment):: Equipment
      integer :: count

      ! Variables para lectura de archivo
      integer :: unit_number, iostat_val ! Unidad de asignado /  Valor de estado de E/S
      character(len=100) :: line, Filename! Almacernar cada línea / Nombre del archivo

      !Variables temporales
      character(len=100) :: tempName, tempUbication
      real :: tempPrice
      integer :: tempQuantity
      integer :: pos1, pos2, pos3

      ! Inicializar variables
      Filename = ''
      count = 0
      allocate (EquipmentList(0))

      ! Preguntar por el nombre
      print *, '////////////////////////////////////////'
      print *, 'Por favor ingrese el nombre del archivo'
      print *, '       Si desea salir ingrese 0'
      print *, '////////////////////////////////////////'
      print *, ""

      write (*, '(A)', advance='no') 'Ingrese el nombre del archivo: '
      read *, Filename
      print *, ""

      ! No se ingresó un nombre
      if (Filename == "0") then
         print *, '//////////////////////////////////////'
         print *, '              Volviendo'
         print *, '//////////////////////////////////////'
         print *, ""
         return
      end if

      ! Abrir archivo
      open (newunit=unit_number, file='../data/'//trim(Filename)//'.inv', status='old', action='read', iostat=iostat_val)

      ! Error al abrir el archivo
      if (iostat_val /= 0) then
         print *, ''
         print *, 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
         print *, 'Error al abrir el archivo o no se encontro el archivo'
         print *, '    Por favor, verifique el nombre del archivo'
         print *, 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
         print *, ''
         return
      end if

      ! Leer archivo línea por línea y concatenar
      do
         read (unit_number, '(A)', iostat=iostat_val) line
         if (iostat_val /= 0) exit
         if (index(line, 'crear_equipo') == 1) then

            !Econtrar posiciones
            pos1 = index(line, ';')
            pos2 = index(line(pos1 + 1:), ';') + pos1
            pos3 = index(line(pos2 + 1:), ';') + pos2

            !Extraer los valores
            tempName = adjustl(trim(line(14:pos1 - 1)))
            read (line(pos1 + 1:pos2 - 1), "(I6)", iostat=iostat_val) tempQuantity
            read (line(pos2 + 1:pos3 - 1), "(F6.2)", iostat=iostat_val) tempPrice
            tempUbication = adjustl(trim(line(pos3 + 1:)))

            if (iostat_val /= 0) then
               print *, ''
               print *, 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
               print *, '               Error al LEER LA LINEA'
               print *, 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
               print *, ''
            else

               !Crear el equipo
               Equipment%Name = tempName
               Equipment%Quantity = tempQuantity
               Equipment%Price = tempPrice
               Equipment%Ubication = tempUbication

               !Agregar el equipo a la lista
               count = count + 1
               call addEquipmentToList(EquipmentList, Equipment, count)

            end if
         end if
      end do

      ! Cerrar archivo
      close (unit_number)

      print *, ''
      print *, '////////////////////////////////////////'
      print *, '    INVENTARIO CARGADO CON EXITO'
      print *, '////////////////////////////////////////'
      print *, ''

   end function loadInventory

   subroutine addEquipmentToList(equipment_List, Equipments, count)
      implicit none
      type(Equipment), allocatable, intent(inout) :: equipment_List(:)
      type(Equipment), intent(in) :: Equipments
      integer, intent(in) :: count

      if (allocated(equipment_List)) then
         equipment_List = [equipment_List, Equipments]
      else
         allocate (equipment_List(count))
         equipment_List(count) = Equipments
      end if
   end subroutine addEquipmentToList

   function margeEquipmentsLists(oldList, newList) result(margeList)
      implicit none
      type(Equipment), allocatable, intent(in) :: oldList(:), newList(:)
      type(Equipment), allocatable :: margeList(:)
      integer :: i
      margeList = oldList

      do i = 1, size(newList)
         if (.not. isEquipmentInList(margeList, newList(i))) then
            call addEquipmentToList(margeList, newList(i), size(margeList))
         end if
      end do

   end function margeEquipmentsLists

   function isEquipmentInList(oldList, Equipments) result(isInList)
      implicit none
      type(Equipment), allocatable, intent(in) :: oldList(:)
      type(Equipment) :: Equipments

      logical :: isInList
      integer :: i

      isInList = .false.

      do i = 1, size(oldList)
         if (trim(oldList(i)%name) == trim(Equipments%name) .AND. trim(oldList(i)%name) == trim(Equipments%name)) then
            isInList = .true.
         end if
      end do

   end function isEquipmentInList

end module read_inventory
