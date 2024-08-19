module read_mov

   use equipment_type
contains

   function readMov(EquipmentLists) result(MovList)
      implicit none
      type(Equipment), allocatable :: MovList(:)
      type(Equipment), allocatable:: EquipmentLists(:)
      type(Equipment):: Equipment
      integer :: count

      ! Variables para lectura de archivo
      integer :: unit_number, iostat_val ! Unidad de asignado /  Valor de estado de E/S
      character(len=100) :: line, Filename! Almacernar cada línea / Nombre del archivo

      !Variables temporales
      character(len=100) :: tempName, tempUbication
      integer :: first_space
      integer :: tempQuantity
      integer :: pos1, pos2
      integer :: tempInstruction
      integer :: indexItem

      ! Inicializar variables}
      allocate (MovList(0))
      Filename = ''
      count = 0
      tempInstruction = 0

      ! Preguntar por el nombre
      print *, '--------------------------------'
      print *, 'Si desea salir, ingrese 1000'
      print *, '--------------------------------'
      print *, 'Ingrese el nombre del archivo: '

      read *, Filename

      if (Filename == "0") then
         ! No se ingresó un nombre
         print *, 'No se ingresó un nombre'
         print *, ""
         return
      end if

      ! Abrir archivo

      open (newunit=unit_number, file='../data/'//trim(Filename)//'.mov', status='old', action='read', iostat=iostat_val)

      ! Error al abrir el archivo
      if (iostat_val /= 0) then
         print *, ''
         print *, '--------------------------------'
         print *, 'Error al abrir el archivo o no se encontro el archivo'
         print *, 'Por favor, verifique el nombre del archivo'
         print *, '--------------------------------'
         print *, ''
         return
      end if

      ! Leer archivo línea por línea y concatenar
      do
         read (unit_number, '(A)', iostat=iostat_val) line
         if (iostat_val /= 0) exit

         if (index(line, 'agregar_stock') == 1 .or. index(line, 'eliminar_equipo') == 1) then
            !Econtrar posiciones
            pos1 = index(line, ';')
            pos2 = index(line(pos1 + 1:), ';') + pos1

            !Extraer la primera palabra
            first_space = index(line, ' ')

            !Extraer los valores
            tempName = adjustl(trim(line(first_space + 1:pos1 - 1)))
            read (line(pos1 + 1:pos2 - 1), "(I6)", iostat=iostat_val) tempQuantity
            tempUbication = adjustl(trim(line(pos2 + 1:)))

            if (iostat_val /= 0) then
               print *, "Error al leer la linea"
            else

               !Crear el equipo
               Equipment%Name = tempName
               Equipment%Quantity = tempQuantity
               Equipment%Ubication = tempUbication

            end if
         end if

         if (index(line, 'agregar_stock') == 1) then
            indexItem = isEquipmentInListIndex(EquipmentLists, Equipment)
            print *, indexItem
            if (indexItem /= -1) then
               EquipmentLists(indexItem)%Quantity = EquipmentLists(indexItem)%Quantity + Equipment%Quantity

               print *, "----------------------"
               print *, EquipmentLists(indexItem)%Quantity
               print *, "----------------------"
               print *, ""
            end if
         else if (index(line, 'eliminar_equipo') == 1) then
            indexItem = isEquipmentInListIndex(EquipmentLists, Equipment)
            print *, indexItem
            if (indexItem /= -1) then
               if (EquipmentLists(indexItem)%Quantity >= Equipment%Quantity) then
                  EquipmentLists(indexItem)%Quantity = EquipmentLists(indexItem)%Quantity - Equipment%Quantity

                  print *, "----------------------"
                  print *, EquipmentLists(indexItem)%Quantity
                  print *, "----------------------"
                  print *, ""
               else
                  print *, "Error: La cantidad a eliminar excede la cantidad disponible."
               end if
            else
               print *, "Error: El equipo no se encuentra en la lista."
            end if
         end if
      end do

      movList = EquipmentLists
   end function

   function isEquipmentInListIndex(List, Equipments) result(index)
      implicit none
      type(Equipment), allocatable, intent(in) :: List(:)
      type(Equipment), intent(in) :: Equipments
      integer :: index, i

      index = -1

      do i = 1, size(List)
         if (trim(List(i)%name) == trim(Equipments%name) .AND. trim(List(i)%Ubication) == trim(Equipments%Ubication)) then
            index = i
            exit
         end if
      end do

   end function isEquipmentInListIndex

end module read_mov
