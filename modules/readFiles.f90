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
      integer :: start, end, len_line ! Inicio, fin, longitud de la línea
      character(len=100) :: word ! Primera palabra de la línea
      type(Equipment):: Equipment

      !Variables temporales
      character(len=100) :: tempName
      integer :: tempQuantity
      real :: tempPrice
      character(len=100) :: tempUbication

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

      open (newunit=unit_number, file='../data/'//trim(Filename)//'.inv', status='old', action='read', iostat=iostat_val)
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
            ! Extraer los valores
            read (line(14:), '(A50, I5, F10.2, A50)', iostat=iostat_val) tempName, tempQuantity, tempPrice, tempUbication
            if (iostat_val == 0) then
               ! Guardar en el tipo Equipo
               Equipment%name = trim(tempName)
               Equipment%quantity = tempQuantity
               Equipment%price = tempPrice
               Equipment%ubication = trim(tempUbication)
               ! Aquí puedes hacer lo que necesites con la variable equipo
            end if
         end if
      end do

      ! Cerrar archivo
      close (unit_number)
   end function loadInventory
end module ReadFiles
