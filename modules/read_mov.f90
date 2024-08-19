module read_mov

   use equipment_type
contains

   function readMov() result(MovList)
      implicit none

      type(Equipment), allocatable :: MovList(:)
      type(Equipment):: Equipment
      integer :: count

      ! Variables para lectura de archivo
      integer :: unit_number, iostat_val ! Unidad de asignado /  Valor de estado de E/S
      character(len=100) :: line, Filename! Almacernar cada línea / Nombre del archivo

      ! Inicializar variables
      Filename = ''
      count = 0
      allocate (MovList(0))

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
         if (index(line, 'agregar_stock') == 1) then
            print *, 'agregar_stock'
         else if (index(line, 'eliminar_equipo') == 1) then
            print *, 'eliminar equipo'
         end if
      end do
   end function

end module read_mov
