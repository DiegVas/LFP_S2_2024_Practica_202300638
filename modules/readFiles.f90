module readFiles

   use equipment_type
contains

   ! Función para leer el archivo y devolver su contenido
   function loadInventory() result(contenido)
      implicit none
      character(len=:), allocatable :: contenido
      integer :: unit_number, iostat_val ! Unidad de asignado /  Valor de estado de E/S
      character(len=100) :: line ! Almacernar cada línea

      !Variables para analisis del archivo
      character(len=100) :: Filename ! Nombre del archivo
      type(Equipment):: Equipment

      !Variables temporales
      character(len=100) :: tempName, tempPrice, tempUbication
      integer :: tempQuantity
      integer :: pos1, pos2, pos3

      ! Inicializar variables
      contenido = ''
      Filename = ''

      ! Preguntar por el nombre
      print *, '--------------------------------'
      print *, 'Si desea salir, ingrese 0'
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

      open (newunit=unit_number, file='../data/'//trim(Filename)//'.inv', status='old', action='read', iostat=iostat_val)

      ! Error al abrir el archivo
      if (iostat_val /= 0) then
         print *, '--------------------------------'
         print *, 'Error al abrir el archivo o no se encontro el archivo'
         print *, 'Por favor, verifique el nombre del archivo'
         print *, '--------------------------------'
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
            tempPrice = adjustl(trim(line(pos2 + 1:pos3 - 1)))
            tempUbication = adjustl(trim(line(pos3 + 1:)))

            if (iostat_val /= 0) then
               print *, "Error al leer la linea"
            else

               print *, 'Nombre:', trim(tempName)
               print *, 'Cantidad', tempQuantity
               print *, 'Precio:', tempPrice
               print *, 'Ubicación:', trim(tempUbication)

            end if
         end if
      end do

      ! Cerrar archivo
      close (unit_number)
   end function loadInventory
end module ReadFiles
