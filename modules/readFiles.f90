module readFiles
contains

   ! Función para leer el archivo y devolver su contenido
   function loadInventory() result(contenido)
      implicit none
      character(len=:), allocatable :: contenido
      integer :: unit_number, iostat_val ! Unidad de asignado /  Valor de estado de E/S
      character(len=100) :: line ! Almacernar cada línea

      character(len=100) :: Filename ! Nombre del archivo
      integer :: start, end, len_line ! Inicio, fin, longitud de la línea
      character(len=100) :: word

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
         return
      end if

      ! Abrir archivo
      open (newunit=unit_number, file=Filename//'.inv', status='old', action='read', iostat=iostat_val)
      if (iostat_val /= 0) then
         ! Error al abrir el archivo
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
            ! Si la línea empieza con crear_equipo
            ! Se debe reemplazar por el nombre del equipo
            start = index(line, 'crear_equipo') + 13
            end = index(line, 'con_') - 1
            len_line = end - start + 1
            word = line(start:start + len_line - 1)

         end if
         contenido = trim(adjustl(contenido))//trim(adjustl(line))//char(10)
      end do

      ! Cerrar archivo
      close (unit_number)
   end function loadInventory
end module ReadFiles
