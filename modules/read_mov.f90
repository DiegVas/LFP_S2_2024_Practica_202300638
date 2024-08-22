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
      integer :: pos1, pos2
      integer :: tempInstruction, indexItem, first_space, tempQuantity

      ! Inicializar variables}
      Filename = ''
      count = 0
      tempInstruction = 0

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
         movList = EquipmentLists
         return
      end if

      ! Abrir archivo

      open (newunit=unit_number, file='../data/'//trim(Filename)//'.mov', status='old', action='read', iostat=iostat_val)

      ! Error al abrir el archivo
      if (iostat_val /= 0) then
         print *, ''
         print *, 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
         print *, 'Error al abrir el archivo o no se encontro el archivo'
         print *, '    Por favor, verifique el nombre del archivo'
         print *, 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
         print *, ''
         movList = EquipmentLists
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

         ! Verificar si el equipo ya existe en la lista
         indexItem = isEquipmentInListIndex(EquipmentLists, Equipment)

         if (indexItem /= -1) then
            if (index(line, 'agregar_stock') == 1) then
               
               EquipmentLists(indexItem)%Quantity = EquipmentLists(indexItem)%Quantity + Equipment%Quantity

            else if (index(line, 'eliminar_equipo') == 1) then

               if (EquipmentLists(indexItem)%Quantity >= Equipment%Quantity) then
      
                  EquipmentLists(indexItem)%Quantity = EquipmentLists(indexItem)%Quantity - Equipment%Quantity

               else

                  print *, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
                  print *, "Error: El Equipo: ", trim(Equipment%name), " Exede la cantidad disponible."
                  print *, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
                  print *, ""

               end if
            end if
         else

            print *, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
            print *, "Error: El Equipo: ", trim(Equipment%name), "no se encuentra en la lista."
            print *, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
            print *, ""

         end if
      end do

      close (unit_number)

      print *, ''
      print *, '////////////////////////////////////////'
      print *, '    INSTRUCIONES CARGADO CON EXITO'
      print *, '////////////////////////////////////////'
      print *, ''

      ! Devolver cambios
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
